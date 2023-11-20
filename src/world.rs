use std::cell::UnsafeCell;
use std::error::Error;
use std::marker::PhantomData;
use std::mem;
use std::panic::{RefUnwindSafe, UnwindSafe};
use std::ptr::NonNull;

use crate::archetype::{ArchetypeId, Archetypes};
use crate::component::{Component, ComponentId, Components};
use crate::entity::{Entities, EntityId, EntityLocation, ReservedEntities};
use crate::event::{Event, EventId, EventKind, EventPtr, EventQueue, Events};
use crate::system::{InitSystem, SystemId, SystemInfo, SystemInitArgs, SystemInitError, Systems};
use crate::util::{GetDebugChecked, UnwrapDebugChecked};

#[derive(Debug)]
pub struct World {
    entities: Entities,
    reserved_entities: ReservedEntities,
    components: Components,
    systems: Systems,
    archetypes: Archetypes,
    events: Events,
    event_queue: EventQueue,
}

impl World {
    pub fn new() -> Self {
        Self {
            entities: Entities::new(),
            reserved_entities: ReservedEntities::new(),
            components: Components::new(),
            systems: Systems::new(),
            archetypes: Archetypes::new(),
            events: Events::new(),
            event_queue: EventQueue::new(),
        }
    }

    pub fn add_system<S, M>(&mut self, system: S) -> Result<SystemId, Box<dyn Error>>
    where
        S: InitSystem<M>,
    {
        let mut args = SystemInitArgs::new(self);

        let system = system.init_system(&mut args)?;

        if args.world.events.get(args.event_id).is_none() {
            return Err(Box::new(SystemInitError::InvalidEventId(args.event_id)));
        }

        let info = SystemInfo {
            priority: args.priority,
            event_id: args.event_id,
            event_access: args.event_access,
            system_id: SystemId::NULL, // Filled in later.
        };

        Ok(self.systems.add(Box::new(system), info))
    }

    pub fn send<E: Event>(&mut self, event: E) {
        let event_id = self.init_event::<E>();

        let event_count = self.event_queue.len();

        unsafe {
            self.event_queue.push(event, event_id);
        }

        self.eval_event_queue(event_count)
    }

    fn eval_event_queue(&mut self, event_start_idx: usize) {
        handle_events(event_start_idx, self);

        if self.event_queue.is_empty() {
            // Resets the bump allocator.
            self.event_queue.reset();
        }

        fn handle_events(event_start_idx: usize, world: &mut World) {
            debug_assert!(event_start_idx < world.event_queue.len());

            'next_event: for event_idx in event_start_idx..world.event_queue.len() {
                let item = unsafe { world.event_queue.get_unchecked_mut(event_idx) };

                let event_id = item.event_id;

                let event_info = unsafe { world.events.get_unchecked(event_id) };

                let event_kind = event_info.kind();

                // In case System::run or handle_events unwinds, we need to drop the event we're
                // holding on the stack. The other events in the event queue will be handled by
                // World's Drop impl.
                struct EventDropper {
                    ptr: Option<NonNull<u8>>,
                    drop: Option<unsafe fn(NonNull<u8>)>,
                }

                impl Drop for EventDropper {
                    fn drop(&mut self) {
                        if let (Some(drop), Some(ptr)) = (self.drop, self.ptr) {
                            unsafe {
                                drop(ptr);
                            }
                        }
                    }
                }

                let mut event_dropper = EventDropper {
                    // take() marks this pointer so we'll avoid dropping the event in World's Drop
                    // impl.
                    ptr: item.event_ptr.take(),
                    drop: event_info.drop(),
                };

                debug_assert!(event_dropper.ptr.is_some());

                // Initialize the system slice.
                item.system_slice =
                    unsafe { world.systems.systems_for_event_unchecked_mut(event_id) };

                loop {
                    let events_before = world.event_queue.len();
                    let item = unsafe { world.event_queue.get_unchecked_mut(event_idx) };

                    if item.system_idx >= unsafe { (*item.system_slice).len() as u32 } {
                        // No more systems to run for this event.
                        break;
                    }

                    let entry = unsafe {
                        (*item.system_slice).get_debug_checked_mut(item.system_idx as usize)
                    };

                    item.system_idx += 1;

                    unsafe {
                        entry.system.run(
                            &entry.info,
                            EventPtr::new(&mut event_dropper.ptr),
                            UnsafeWorldCell::new(world),
                        )
                    }

                    let empty = world.archetypes.empty_mut();

                    // Spawn any entities reserved by the system.
                    world
                        .entities
                        .flush_reserved(&mut world.reserved_entities, |id| {
                            let (row, _) = unsafe { empty.add(id) };
                            EntityLocation {
                                archetype: ArchetypeId::EMPTY,
                                row,
                            }
                        });

                    let events_after = world.event_queue.len();

                    if events_before < events_after {
                        // Eagerly handle any events produced by the system.
                        handle_events(events_before, world);
                    }

                    debug_assert_eq!(world.event_queue.len(), events_before);

                    // Did the system take ownership of the event?
                    if event_dropper.ptr.is_none() {
                        // No need to run event_dropper Drop because it wouldn't do anything.
                        mem::forget(event_dropper);
                        continue 'next_event;
                    }
                }

                // SAFETY: `None` check was done at the end of the loop above.
                let event_ptr = unsafe { event_dropper.ptr.unwrap_debug_checked() };
                let drop_event = event_dropper.drop;
                mem::forget(event_dropper);

                match event_kind {
                    EventKind::Other => {
                        if let Some(drop) = drop_event {
                            unsafe { drop(event_ptr) }
                        }
                    }
                    EventKind::Insert(component_id) => todo!(),
                    EventKind::Remove(component_id) => todo!(),
                    EventKind::Spawn => todo!(),
                    EventKind::Despawn => todo!(),
                    EventKind::SendTo(_) => todo!(),
                }
            }

            // All events in this slice have been handled.
            unsafe {
                world.event_queue.set_len(event_start_idx);
            }
        }
    }

    fn flush_reserved_entities(&mut self) {}

    pub fn init_event<E: Event>(&mut self) -> EventId {
        let kind = E::init(self);
        let id = unsafe { self.events.init::<E>(kind) };
        self.systems.init_event(id);
        id
    }

    pub fn init_component<C: Component>(&mut self) -> ComponentId {
        self.components.init_component::<C>()
    }

    pub fn entities(&self) -> &Entities {
        &self.entities
    }
}

impl Default for World {
    fn default() -> Self {
        Self::new()
    }
}

impl Drop for World {
    fn drop(&mut self) {
        // Drop unhandled events in the event queue.
        for item in self.event_queue.iter() {
            if let Some(event_ptr) = item.event_ptr {
                let info = unsafe { self.events.get(item.event_id).unwrap_debug_checked() };

                if let Some(drop) = info.drop() {
                    unsafe {
                        drop(event_ptr);
                    }
                }
            }
        }
    }
}

unsafe impl Send for World {}
unsafe impl Sync for World {}

impl UnwindSafe for World {}
impl RefUnwindSafe for World {}

pub trait FromWorld {
    fn from_world(world: &mut World) -> Self;
}

impl<T: Default> FromWorld for T {
    fn from_world(_: &mut World) -> Self {
        Self::default()
    }
}

#[derive(Clone, Copy, Debug)]
pub struct UnsafeWorldCell<'a> {
    world: *mut World,
    _marker: PhantomData<(&'a World, &'a UnsafeCell<World>)>,
}

// SAFETY: `&World` and `&mut World` are both `Send`
unsafe impl Send for UnsafeWorldCell<'_> {}
// SAFETY: `&World` and `&mut World` are both `Sync`
unsafe impl Sync for UnsafeWorldCell<'_> {}

impl<'a> UnsafeWorldCell<'a> {
    pub(crate) fn new(world: &'a mut World) -> Self {
        Self {
            world,
            _marker: PhantomData,
        }
    }

    pub fn entities(self) -> &'a Entities {
        unsafe { &(*self.world).entities }
    }

    pub unsafe fn event_queue(self) -> &'a EventQueue {
        &(*self.world).event_queue
    }

    pub unsafe fn event_queue_mut(self) -> &'a mut EventQueue {
        &mut (*self.world).event_queue
    }

    /// # Safety
    ///
    /// Must have write permission to reserve entities.
    ///
    /// # Panics
    pub unsafe fn reserve_entity(self) -> EntityId {
        let reserved_entities = &mut (*self.world).reserved_entities;
        self.entities().reserve(reserved_entities)
    }
}
