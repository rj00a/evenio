use std::error::Error;
use std::mem;
use std::ptr::NonNull;

use bumpalo::Bump;

use crate::archetype::Archetypes;
use crate::util::{GetDebugChecked, UnwrapDebugChecked};
use crate::{
    Component, ComponentId, Components, Event, EventId, EventInfo, EventQueueItem, Events,
    InitSystem, InvalidEventId, MissingEvent, System, SystemId, SystemInfo, SystemInitArgs,
    SystemListEntry, SystemRunArgs, Systems,
};

#[derive(Debug)]
pub struct World {
    components: Components,
    archetypes: Archetypes,
    events: Events,
    event_queue: Vec<EventQueueItem>,
    /// Bump allocator for events.
    event_bump: Bump,
    systems: Systems,
}

impl World {
    pub fn new() -> Self {
        Self {
            components: Components::new(),
            archetypes: Archetypes::new(),
            events: Events::new(),
            event_queue: vec![],
            event_bump: Bump::new(),
            systems: Systems::new(),
        }
    }

    pub fn send_event<E: Event>(&mut self, event: E) {
        let event_id = self.init_event::<E>();

        let event = NonNull::from(self.event_bump.alloc(event)).cast::<u8>();

        self.event_queue.push(EventQueueItem {
            event_id,
            event_ptr: event,
        });

        self.eval_event_queue()
    }

    pub fn add_system<S, M>(&mut self, mut system: S) -> Result<SystemId, Box<dyn Error>>
    where
        S: InitSystem<M>,
    {
        let mut args = SystemInitArgs::new(self);

        let system = system.init_system(&mut args)?;

        let event_id = args.event_id.ok_or_else(|| Box::new(MissingEvent))?;

        if args.world.events.event(event_id).is_none() {
            return Err(Box::new(InvalidEventId(event_id)));
        }

        let info = SystemInfo {
            priority: args.priority,
            event_id,
            event_access: args.event_access,
            system_id: SystemId::NULL, // Filled in later.
        };

        Ok(self.systems.add_system(Box::new(system), info))
    }

    fn eval_event_queue(&mut self) {
        unsafe {
            handle_events(0, self);
        }

        debug_assert!(self.event_queue.is_empty());
        self.event_bump.reset();

        unsafe fn handle_events(event_start_idx: usize, world: &mut World) {
            debug_assert!(event_start_idx < world.event_queue.len());

            'next_event: for event_idx in event_start_idx..world.event_queue.len() {
                let EventQueueItem {
                    event_id,
                    event_ptr,
                } = *unsafe { world.event_queue.get_debug_checked(event_idx) };

                // Get a raw slice to the list of systems for this event.
                // We use a pointer here to get around some lifetime/borrowing issues. Systems
                // aren't allowed to add or remove other systems while they're running, so
                // there's no risk of this pointer becoming invalid.
                let system_slice: *mut [SystemListEntry] =
                    unsafe { world.systems.systems_for_event_unchecked_mut(event_id) };

                // TODO: simplify when slice_ptr_len stabilizes.
                // SAFETY: pointer is valid as explained above.
                let system_count = unsafe { (*system_slice).len() };

                for system_idx in 0..system_count {
                    let events_before = world.event_queue.len();

                    // TODO: make EventDropper type with Drop impl to drop the remaining live events
                    // if `system.run` unwinds.

                    {
                        let entry = unsafe { (*system_slice).get_debug_checked_mut(system_idx) };

                        let mut args = SystemRunArgs {
                            event_ptr: Some(event_ptr),
                            system_info: &entry.info,
                        };

                        unsafe { entry.system.run(&mut args) }

                        if args.event_ptr.is_none() {
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
                        drop(event_ptr);
                    }
                }
            }

            unsafe {
                world.event_queue.set_len(event_start_idx);
            }
        }
    }

    pub fn init_event<E: Event>(&mut self) -> EventId {
        self.events.init_event::<E>()
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

#[cfg(test)]
mod tests {}
