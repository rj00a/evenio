use std::error::Error;
use std::marker::PhantomData;
use std::panic::{RefUnwindSafe, UnwindSafe};
use std::ptr::NonNull;

use crate::archetype::Archetypes;
use crate::component::{Component, ComponentId, ComponentRegistry};
use crate::event::{Event, EventId, EventQueue, EventQueueItem, EventRegistry};
use crate::system::{
    InitSystem, SystemId, SystemInfo, SystemInitArgs, SystemInitError, SystemListEntry,
    SystemRegistry,
};
use crate::util::{GetDebugChecked, UnwrapDebugChecked};

#[derive(Debug)]
pub struct World {
    components: ComponentRegistry,
    archetypes: Archetypes,
    events: EventRegistry,
    event_queue: EventQueue,
    systems: SystemRegistry,
}

impl World {
    pub fn new() -> Self {
        Self {
            components: ComponentRegistry::new(),
            archetypes: Archetypes::new(),
            events: EventRegistry::new(),
            event_queue: EventQueue::new(),
            systems: SystemRegistry::new(),
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

        if args.world.events.event(args.event_id).is_none() {
            return Err(Box::new(SystemInitError::InvalidEventId(args.event_id)));
        }

        let info = SystemInfo {
            priority: args.priority,
            event_id: args.event_id,
            event_access: args.event_access,
            system_id: SystemId::NULL, // Filled in later.
        };

        Ok(self.systems.add_system(Box::new(system), info))
    }

    fn eval_event_queue(&mut self) {
        handle_events(0, self);

        debug_assert!(self.event_queue.is_empty());
        self.event_queue.clear();

        fn handle_events(event_start_idx: usize, world: &mut World) {
            debug_assert!(event_start_idx < world.event_queue.len());

            'next_event: for event_idx in event_start_idx..world.event_queue.len() {
                let EventQueueItem {
                    event_id,
                    event_ptr,
                } = unsafe { world.event_queue.get_unchecked(event_idx) };

                // Get a raw slice to the list of systems for this event.
                // We use a pointer here to get around some lifetime/borrowing issues. Systems
                // aren't allowed to add or remove other systems while they're running, so
                // there's no risk of this pointer becoming invalid.
                let system_slice: *mut [SystemListEntry] =
                    unsafe { world.systems.systems_for_event_unchecked_mut(event_id) };

                // TODO: simplify when slice_ptr_len stabilizes.
                // SAFETY: pointer is still valid from above.
                let system_count = unsafe { (*system_slice).len() };

                let mut event_ptr = Some(event_ptr);

                for system_idx in 0..system_count {
                    let events_before = world.event_queue.len();

                    // TODO: make EventDropper type with Drop impl to drop the remaining live events
                    // if `system.run` unwinds.

                    {
                        let entry = unsafe { (*system_slice).get_debug_checked_mut(system_idx) };

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
                        unsafe { handle_events(events_before, world) };
                    }

                    debug_assert_eq!(world.event_queue.len(), events_before);
                }

                // None of the systems consumed the event, so it is our responsibility to call
                // the destructor.
                let event_info = unsafe { world.events.event(event_id).unwrap_debug_checked() };

                if let Some(drop) = event_info.drop() {
                    unsafe {
                        drop(event_ptr.unwrap_debug_checked());
                    }
                }
            }

            unsafe {
                world.event_queue.set_len(event_start_idx);
            }
        }
    }

    pub fn init_event<E: Event>(&mut self) -> EventId {
        let id = self.events.init_event::<E>();
        self.systems.init_event(id);
        id
    }

    pub fn init_component<C: Component>(&mut self) -> ComponentId {
        self.components.init_component::<C>()
    }
}

impl Default for World {
    fn default() -> Self {
        Self::new()
    }
}

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

unsafe impl Send for World {}
unsafe impl Sync for World {}

impl UnwindSafe for World {}
impl RefUnwindSafe for World {}
