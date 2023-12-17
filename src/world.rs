use core::cell::UnsafeCell;
use core::marker::PhantomData;
use core::panic::{RefUnwindSafe, UnwindSafe};

use crate::archetype::Archetypes;
use crate::component::{Component, ComponentId, Components};
use crate::entity::{Entities, EntityId, ReservedEntities};
use crate::event::{Event, EventId, EventIdx, EventQueue, Events};
use crate::system::{IntoSystem, SystemId, Systems};

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

    pub fn send<E: Event>(&mut self, event: E) {
        todo!()
    }

    pub fn add_system<S: IntoSystem<M>, M>(&mut self, system: S) -> SystemId {
        todo!()
    }

    pub fn init_component<C: Component>(&mut self) -> ComponentId {
        todo!()
    }

    pub fn init_event<E: Event>(&mut self) -> EventId {
        todo!()
    }

    pub fn entities(&self) -> &Entities {
        &self.entities
    }

    pub fn components(&self) -> &Components {
        &self.components
    }

    pub fn systems(&self) -> &Systems {
        &self.systems
    }

    pub fn archetypes(&self) -> &Archetypes {
        &self.archetypes
    }

    pub fn events(&self) -> &Events {
        &self.events
    }
}

impl Default for World {
    fn default() -> Self {
        Self::new()
    }
}

unsafe impl Send for World {}
unsafe impl Sync for World {}

impl UnwindSafe for World {}
impl RefUnwindSafe for World {}

#[derive(Clone, Copy, Debug)]
pub struct UnsafeWorldCell<'a> {
    world: *mut World,
    _marker: PhantomData<(&'a World, &'a UnsafeCell<World>)>,
}

impl<'a> UnsafeWorldCell<'a> {
    pub(crate) fn new(world: &'a mut World) -> Self {
        Self {
            world,
            _marker: PhantomData,
        }
    }

    /// # Safety
    ///
    /// - Must have permission to access the event queue mutably.
    /// - Event index must be correct for the given event.
    pub unsafe fn send_unchecked<E: Event>(self, event: E, event_idx: EventIdx) {
        unsafe { (*self.world).event_queue.push(event, event_idx) }
    }

    pub unsafe fn reserve_entity(self) -> EntityId {
        (*self.world).reserved_entities.reserve(self.entities())
    }

    pub fn entities(self) -> &'a Entities {
        unsafe { &(*self.world).entities }
    }

    pub fn components(self) -> &'a Components {
        unsafe { &(*self.world).components }
    }

    pub fn systems(self) -> &'a Systems {
        unsafe { &(*self.world).systems }
    }

    pub fn archetypes(self) -> &'a Archetypes {
        unsafe { &(*self.world).archetypes }
    }
}
