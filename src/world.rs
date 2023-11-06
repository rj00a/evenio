use std::error::Error;
use std::marker::PhantomData;
use std::mem;
use std::panic::{RefUnwindSafe, UnwindSafe};
use std::ptr::NonNull;

use crate::archetype::Archetypes;
use crate::component::{Component, ComponentId, Components};
use crate::entity::{Entities, EntityId};
use crate::event::{Event, EventId, EventKind, EventQueue, EventQueueItem, Events};
use crate::system::{
    InitSystem, SystemId, SystemInfo, SystemInitArgs, SystemInitError, SystemListEntry,
    Systems,
};
use crate::util::{GetDebugChecked, UnwrapDebugChecked};

#[derive(Debug)]
pub struct World {
    entities: Entities,
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
            components: Components::new(),
            systems: Systems::new(),
            archetypes: Archetypes::new(),
            events: Events::new(),
            event_queue: EventQueue::new(),
        }
    }

    pub fn send_event<E: Event>(&mut self, event: E) {
        let event_id = self.init_event::<E>();

        unsafe {
            self.event_queue.push(event, event_id);
        }

        self.eval_event_queue()
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

    fn eval_event_queue(&mut self) {
        handle_events(0, self);

        debug_assert!(self.event_queue.is_empty());
        self.event_queue.clear();

        fn handle_events(event_start_idx: usize, world: &mut World) {
            debug_assert!(event_start_idx < world.event_queue.len());

            'next_event: for event_idx in event_start_idx..world.event_queue.len() {
                let item = unsafe { world.event_queue.get_unchecked_mut(event_idx) };

                let event_id = item.event_id;

                let event_info = unsafe { world.events.get_unchecked(event_id) };

                let event_kind = event_info.kind();

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
                    ptr: item.event_ptr.take(),
                    drop: event_info.drop(),
                };

                // Initialize the system slice.
                item.system_slice = unsafe {
                    world
                        .systems
                        .systems_for_event_unchecked_mut(event_id.index())
                };

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

                    let args = SystemRunArgs {
                        event_ptr: &mut event_dropper.ptr,
                        system_info: &entry.info,
                        world,
                        _marker: PhantomData,
                    };

                    unsafe { entry.system.run(args) }

                    let events_after = world.event_queue.len();

                    if events_before < events_after {
                        // Eagerly handle any events produced by the system we just ran.
                        handle_events(events_before, world);
                    }

                    debug_assert_eq!(world.event_queue.len(), events_before);

                    if event_dropper.ptr.is_none() {
                        // The system consumed the event. No other systems get to run.
                        // No need to run event_dropper `Drop` impl because ownership of the event
                        // has changed.
                        mem::forget(event_dropper);
                        continue 'next_event;
                    }
                }

                /*
                while item.system_idx < unsafe { (*item.system_slice).len() as u32 } {
                    let events_before = world.event_queue.len();

                    {
                        let entry = unsafe {
                            (*item.system_slice).get_debug_checked_mut(item.system_idx as usize)
                        };

                        let args = SystemRunArgs {
                            event_ptr: &mut event_ptr,
                            system_info: &entry.info,
                            world,
                            _marker: PhantomData,
                        };

                        unsafe { entry.system.run(args) }

                        if event_ptr.is_none() {
                            // The system consumed the event. No other systems get to run.
                            continue 'next_event;
                        }
                    }

                    let events_after = world.event_queue.len();

                    if events_before < events_after {
                        // Eagerly handle any events produced by the system we just ran.
                        handle_events(events_before, world);
                    }

                    debug_assert_eq!(world.event_queue.len(), events_before);
                }*/

                match event_kind {
                    EventKind::Other => {}
                    EventKind::Spawn => todo!(),
                    EventKind::InsertComponent(component_id) => todo!(),
                    EventKind::RemoveComponent(component_id) => todo!(),
                    EventKind::Despawn => todo!(),
                    EventKind::FlushCommands => todo!(),
                }

                /*
                // None of the systems consumed the event, so it is our responsibility to call
                // the destructor.
                if let Some(drop) = event_info.drop() {
                    unsafe {
                        drop(event_ptr);
                    }
                }*/
            }

            // All events in this slice have been handled.
            unsafe {
                world.event_queue.set_len(event_start_idx);
            }
        }
    }

    pub fn init_event<E: Event>(&mut self) -> EventId {
        let kind = E::init(self);
        let id = unsafe { self.events.init::<E>(kind) };
        self.systems.init_event(id);
        id
    }

    pub fn init_component<C: Component>(&mut self) -> ComponentId {
        self.components.init_component::<C>()
    }

    // TODO: temp
    pub fn spawn_entity_with_one_component<C: Component>(&mut self, component: C) -> EntityId {
        self.init_component::<C>();

        todo!()
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
pub struct SystemRunArgs<'a> {
    event_ptr: *mut Option<NonNull<u8>>,
    system_info: &'a SystemInfo,
    world: *mut World,
    _marker: PhantomData<&'a mut World>,
}

impl<'a> SystemRunArgs<'a> {
    pub unsafe fn event_ptr(&self) -> Option<NonNull<u8>> {
        *self.event_ptr
    }

    pub unsafe fn event_ptr_mut(&self) -> &'a mut Option<NonNull<u8>> {
        &mut *self.event_ptr
    }

    pub fn system_info(&self) -> &'a SystemInfo {
        self.system_info
    }

    pub unsafe fn event_queue(&self) -> &'a EventQueue {
        &(*self.world).event_queue
    }

    pub unsafe fn event_queue_mut(&self) -> &'a mut EventQueue {
        &mut (*self.world).event_queue
    }

    // TODO: more world methods.
}
