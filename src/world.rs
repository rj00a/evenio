use core::cell::UnsafeCell;
use core::marker::PhantomData;
use core::panic::{RefUnwindSafe, UnwindSafe};
use std::alloc::Layout;
use std::any::{self, TypeId};
use std::mem;
use std::ptr::{self, NonNull};

use crate::archetype::Archetypes;
use crate::component::{
    AddComponent, Component, ComponentDescriptor, ComponentId, ComponentInfo, Components,
    RemoveComponent,
};
use crate::debug_checked::UnwrapDebugChecked;
use crate::entity::{Entities, EntityId, ReservedEntities};
use crate::event::{
    AddEvent, Despawn, Event, EventDescriptor, EventId, EventIdx, EventInfo, EventKind, EventMeta,
    EventPtr, EventQueue, Events, Insert, Remove, RemoveEvent, Spawn, SpawnQueued,
};
use crate::system::{
    AddSystem, Config, IntoSystem, RemoveSystem, System, SystemId, SystemInfo, SystemInfoInner,
    SystemList, Systems,
};
use crate::{drop_fn_of, AssertMutable, DropFn};

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
    /// # Examples
    ///
    /// ```
    /// use evenio::prelude::*;
    ///
    /// let mut world = World::new();
    /// ```
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

    /// # Examples
    ///
    /// ```
    /// use evenio::prelude::*;
    ///
    /// #[derive(Event)]
    /// struct MyEvent(i32);
    ///
    /// fn my_system(r: Receiver<MyEvent>) {
    ///     println!("got event: {}", r.event.0);
    /// }
    ///
    /// let mut world = World::new();
    ///
    /// world.add_system(my_system);
    /// world.send(MyEvent(123));
    /// ```
    pub fn send<E: Event>(&mut self, event: E) {
        self.send_many(|mut s| s.send(event))
    }

    pub fn send_many<F, R>(&mut self, f: F) -> R
    where
        F: FnOnce(Sender) -> R,
    {
        let res = f(Sender { world: self });

        self.flush_event_queue();

        res
    }

    /// Examples
    ///
    /// ```
    /// use evenio::prelude::*;
    ///
    /// let mut world = World::new();
    /// let id = world.spawn();
    ///
    /// assert!(world.entities().contains(id));
    /// ```
    pub fn spawn(&mut self) -> EntityId {
        self.send_many(|mut s| s.spawn())
    }

    pub fn insert<C: Component>(&mut self, entity: EntityId, component: C) {
        self.send(Insert::new(entity, component))
    }

    pub fn remove<C: Component>(&mut self, entity: EntityId) {
        self.send(Remove::<C>::new(entity))
    }

    pub fn despawn(&mut self, entity: EntityId) {
        self.send(Despawn(entity))
    }

    /// # Examples
    ///
    /// ```
    /// use evenio::prelude::*;
    ///
    /// #[derive(Component, PartialEq, Debug)]
    /// struct MyComponent(i32);
    ///
    /// let mut world = World::new();
    ///
    /// let e = world.spawn();
    /// world.insert(e, MyComponent(123));
    ///
    /// assert_eq!(
    ///     world.get_component::<MyComponent>(e),
    ///     Some(&MyComponent(123))
    /// );
    /// ```
    pub fn get_component<C: Component>(&self, entity: EntityId) -> Option<&C> {
        let loc = self.entities.get(entity)?;

        let component_idx = self
            .components()
            .get_by_type_id(TypeId::of::<C>())?
            .id()
            .index();

        let arch = unsafe { self.archetypes().get(loc.archetype).unwrap_debug_checked() };

        let col = arch.column_of(component_idx)?;

        Some(unsafe {
            &*col
                .data()
                .as_ptr()
                .cast_const()
                .cast::<C>()
                .add(loc.row.0 as usize)
        })
    }

    /// # Examples
    ///
    /// ```
    /// use evenio::prelude::*;
    ///
    /// #[derive(Component, PartialEq, Debug)]
    /// struct MyComponent(i32);
    ///
    /// let mut world = World::new();
    ///
    /// let e = world.spawn();
    /// world.insert(e, MyComponent(123));
    ///
    /// assert_eq!(
    ///     world.get_component_mut::<MyComponent>(e),
    ///     Some(&mut MyComponent(123))
    /// );
    /// ```
    pub fn get_component_mut<C: Component>(&mut self, entity: EntityId) -> Option<&mut C> {
        let _ = AssertMutable::<C>::COMPONENT;

        let loc = self.entities.get(entity)?;

        let component_idx = self
            .components()
            .get_by_type_id(TypeId::of::<C>())?
            .id()
            .index();

        let arch = unsafe { self.archetypes().get(loc.archetype).unwrap_debug_checked() };

        let col = arch.column_of(component_idx)?;

        Some(unsafe { &mut *col.data().as_ptr().cast::<C>().add(loc.row.0 as usize) })
    }

    /// # Examples
    ///
    /// ```
    /// use evenio::prelude::*;
    /// # #[derive(Event)]
    /// # struct MyEvent;
    ///
    /// fn my_system(_: Receiver<MyEvent>) {};
    ///
    /// let mut world = World::new();
    /// let id = world.add_system(my_system);
    /// ```
    #[track_caller]
    pub fn add_system<S: IntoSystem<M>, M>(&mut self, system: S) -> SystemId {
        let mut system = system.into_system();
        let mut config = Config::default();

        let type_id = system.type_id();

        if let Some(type_id) = type_id {
            if let Some(info) = self.systems.get_by_type_id(type_id) {
                return info.id();
            }
        }

        if let Err(e) = system.init(self, &mut config) {
            panic!("{e}");
        }

        let Some(received_event) = config.received_event else {
            panic!(
                "system `{}` did not specify an event to receive. All systems must listen for \
                 exactly one event type (see `Receiver`)",
                any::type_name::<S>()
            )
        };

        let info = SystemInfo::new(SystemInfoInner {
            name: system.name(),
            received_event,
            received_event_access: config.received_event_access,
            targeted_event_expr: config.targeted_event_expr,
            sent_untargeted_events: config.sent_untargeted_events,
            sent_targeted_events: config.sent_targeted_events,
            event_queue_access: config.event_queue_access,
            component_access: config.component_access,
            referenced_components: config.referenced_components,
            priority: config.priority,
            id: SystemId::NULL, // Filled in later.
            type_id,
            system,
        });

        let id = self.systems.add(info);
        let info = self.systems.get_mut(id).unwrap();

        self.archetypes.register_system(info);

        self.send(AddSystem(id));

        id
    }

    /// # Example
    ///
    /// ```
    /// use evenio::prelude::*;
    ///
    /// let mut world = World::new();
    ///
    /// # #[derive(Event)]
    /// # struct MyEvent;
    /// let system_id = world.add_system(|_: Receiver<MyEvent>| {});
    ///
    /// let info = world.remove_system(system_id).unwrap();
    ///
    /// assert_eq!(info.id(), system_id);
    /// assert!(!world.systems().contains(system_id));
    /// ```
    pub fn remove_system(&mut self, id: SystemId) -> Option<SystemInfo> {
        if !self.systems.contains(id) {
            return None;
        }

        self.send(RemoveSystem(id));

        let info = self.systems.remove(id).unwrap();

        self.archetypes.remove_system(&info);

        Some(info)
    }

    /// # Examples
    ///
    /// ```
    /// use evenio::prelude::*;
    ///
    /// #[derive(Component)]
    /// struct MyComponent;
    ///
    /// let mut world = World::new();
    /// let id = world.add_component::<MyComponent>();
    ///
    /// assert_eq!(id, world.add_component::<MyComponent>());
    ///
    /// println!("{}", world.components()[id].name());
    /// ```
    pub fn add_component<C: Component>(&mut self) -> ComponentId {
        let desc = ComponentDescriptor {
            name: any::type_name::<C>().into(),
            type_id: Some(TypeId::of::<C>()),
            layout: Layout::new::<C>(),
            drop: drop_fn_of::<C>(),
            is_immutable: C::IS_IMMUTABLE,
        };

        unsafe { self.add_component_with_descriptor(desc) }
    }

    pub unsafe fn add_component_with_descriptor(
        &mut self,
        desc: ComponentDescriptor,
    ) -> ComponentId {
        let (id, is_new) = self.components.add(desc);

        if is_new {
            self.send(AddComponent(id));
        }

        id
    }

    pub fn remove_component(&mut self, id: ComponentId) -> Option<ComponentInfo> {
        if !self.components.contains(id) {
            return None;
        }

        self.send(RemoveComponent(id));

        let despawn_idx = self.add_event::<Despawn>().index().as_u32();

        // Attempt to despawn all entities that still have this component.
        for arch in self.archetypes.iter() {
            if arch.column_of(id.index()).is_some() {
                for &entity_id in arch.entity_ids() {
                    unsafe { self.event_queue.push(Despawn(entity_id), despawn_idx) };
                }
            }
        }

        self.flush_event_queue();

        // Remove all systems that reference this component.
        let mut systems_to_remove = vec![];

        for sys in self.systems.iter() {
            if sys.referenced_components().contains(id.index()) {
                systems_to_remove.push(sys.id());
            }
        }

        for sys_id in systems_to_remove {
            self.remove_system(sys_id);
        }

        let info = &self.components[id];

        // Remove all the `Insert` and `Remove` events for this component.
        let events_to_remove = info
            .insert_events()
            .iter()
            .copied()
            .chain(info.remove_events().iter().copied())
            .collect::<Vec<_>>();

        for event in events_to_remove {
            self.remove_event(event);
        }

        // Remove all archetypes with this component. If there are still entities with
        // the component by this point, then they will be silently removed.
        self.archetypes.remove_component(id.index(), |entity_id| {
            self.entities.remove(entity_id);
        });

        self.components.remove(id)
    }

    /// # Examples
    ///
    /// ```
    /// use evenio::prelude::*;
    ///
    /// #[derive(Event)]
    /// struct MyEvent;
    ///
    /// let mut world = World::new();
    /// let id = world.add_event::<MyEvent>();
    ///
    /// assert_eq!(id, world.add_event::<MyEvent>());
    ///
    /// println!("{}", world.events()[id].name());
    /// ```
    pub fn add_event<E: Event>(&mut self) -> EventId {
        let desc = EventDescriptor {
            name: any::type_name::<E>().into(),
            type_id: Some(TypeId::of::<E>()),
            is_targeted: E::IS_TARGETED,
            kind: unsafe { E::init(self) },
            layout: Layout::new::<E>(),
            drop: drop_fn_of::<E>(),
            is_immutable: E::IS_IMMUTABLE,
        };

        unsafe { self.add_event_with_descriptor(desc) }
    }

    pub unsafe fn add_event_with_descriptor(&mut self, desc: EventDescriptor) -> EventId {
        let kind = desc.kind;

        let (id, is_new) = self.events.add(desc);

        if is_new {
            self.systems.register_event(id.index());

            match kind {
                EventKind::Other => {}
                EventKind::Insert { component_idx, .. } => {
                    if let Some(info) = self.components.get_by_index_mut(component_idx) {
                        info.insert_events.insert(id);
                    }
                }
                EventKind::Remove { component_idx } => {
                    if let Some(info) = self.components.get_by_index_mut(component_idx) {
                        info.remove_events.insert(id);
                    }
                }
                EventKind::SpawnQueued => {}
                EventKind::Despawn => {}
            }

            self.send(AddEvent(id));
        }

        id
    }

    /// # Examples
    ///
    /// ```
    /// use evenio::prelude::*;
    ///
    /// #[derive(Event)]
    /// struct MyEvent;
    ///
    /// let mut world = World::new();
    ///
    /// let id = world.add_event::<MyEvent>();
    /// world.remove_event(id);
    ///
    /// assert!(!world.events().contains(id));
    /// ```
    pub fn remove_event(&mut self, id: EventId) -> Option<EventInfo> {
        assert!(self.event_queue.is_empty());

        if !self.events.contains(id) || id == EventId::SPAWN_QUEUED {
            return None;
        }

        // Send event before removing anything.
        self.send(RemoveEvent(id));

        // Remove all systems that send or receive this event.
        let mut to_remove = vec![];

        for sys in self.systems.iter() {
            if sys.received_event() == id
                || match id.index() {
                    EventIdx::Targeted(idx) => sys.sent_targeted_events().contains(idx),
                    EventIdx::Untargeted(idx) => sys.sent_untargeted_events().contains(idx),
                }
            {
                to_remove.push(sys.id());
            }
        }

        for id in to_remove {
            self.remove_system(id);
        }

        let info = self.events.remove(id).unwrap();

        match info.kind() {
            EventKind::Other => {}
            EventKind::Insert { component_idx, .. } => {
                if let Some(info) = self.components.get_by_index_mut(component_idx) {
                    info.insert_events.remove(&id);
                }
            }
            EventKind::Remove { component_idx } => {
                if let Some(info) = self.components.get_by_index_mut(component_idx) {
                    info.remove_events.remove(&id);
                }
            }
            EventKind::SpawnQueued => {}
            EventKind::Despawn => {}
        }

        Some(info)
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

    /// Send all queued events to systems. The event queue will be empty after
    /// this call.
    fn flush_event_queue(&mut self) {
        handle_events(0, self);
        debug_assert_eq!(self.event_queue.len(), 0);
        self.event_queue.clear();

        fn handle_events(queue_start_idx: usize, world: &mut World) {
            debug_assert!(queue_start_idx < world.event_queue.len());

            'next_event: for queue_idx in queue_start_idx..world.event_queue.len() {
                let item = unsafe { world.event_queue.get_debug_checked_mut(queue_idx) };
                let event_meta = item.meta;
                let event_info = unsafe {
                    world
                        .events
                        .get_by_index(event_meta.event_idx())
                        .unwrap_debug_checked()
                };
                let event_kind = event_info.kind();

                // Put the event pointer on the stack because pointers into the event queue
                // would be invalidated by pushes.
                let mut event = EventDropper {
                    // Set pointer to null so the `World`'s destructor will know we've taken
                    // ownership of the event.
                    event: mem::replace(&mut item.event, ptr::null_mut()),
                    drop: event_info.drop(),
                };

                struct EventDropper {
                    event: *mut u8,
                    drop: DropFn,
                }

                impl EventDropper {
                    /// Extracts the event pointer and drop fn without running
                    /// the destructor.
                    #[inline]
                    fn unpack(self) -> (*mut u8, DropFn) {
                        let event = self.event;
                        let drop = self.drop;
                        mem::forget(self);

                        (event, drop)
                    }
                }

                // In case `System::run` or `handle_events` unwinds, we need to drop the event
                // we're holding on the stack. The other events in the event queue will be
                // handled by `World`'s destructor.
                impl Drop for EventDropper {
                    #[inline]
                    fn drop(&mut self) {
                        if let (Some(event), Some(drop)) = (NonNull::new(self.event), self.drop) {
                            unsafe { drop(event) };
                        }
                    }
                }

                let system_list = match event_meta {
                    EventMeta::Untargeted { idx } => unsafe {
                        world
                            .systems
                            .get_untargeted_list(idx)
                            .unwrap_debug_checked()
                    },
                    EventMeta::Targeted { idx, target } => {
                        let Some(location) = world.entities.get(target) else {
                            continue;
                        };

                        let arch = unsafe {
                            world
                                .archetypes
                                .get(location.archetype)
                                .unwrap_debug_checked()
                        };

                        static EMPTY: SystemList = SystemList::new();

                        // Return an empty system list instead of continuing in case this event is
                        // special.
                        arch.system_list_for(idx).unwrap_or(&EMPTY)
                    }
                };

                let systems: *const [_] = system_list.systems();

                for info_ptr in unsafe { &*systems } {
                    let events_before = world.event_queue.len();

                    let system = unsafe { &mut (*info_ptr.as_ptr()).system };

                    let info = unsafe { SystemInfo::ref_from_ptr(info_ptr) };

                    let event_ptr = EventPtr::new(NonNull::from(&mut event.event));
                    let world_cell = world.unsafe_cell_mut();

                    unsafe { system.run(info, event_ptr, world_cell) };

                    let events_after = world.event_queue.len();

                    if events_before < events_after {
                        // Eagerly handle any events produced by the system.
                        handle_events(events_before, world);
                    }

                    debug_assert_eq!(world.event_queue.len(), events_before);

                    // Did the system take ownership of the event?
                    if event.event.is_null() {
                        // Event is null; destructor wouldn't do anything.
                        event.unpack();

                        continue 'next_event;
                    }
                }

                match event_kind {
                    EventKind::Other => {
                        // Ordinary event. Run event dropper destructor.
                    }
                    EventKind::Insert {
                        component_idx,
                        component_offset,
                    } => {
                        let entity_id = unsafe { *event.event.cast::<EntityId>() };

                        if let Some(loc) = world.entities.get(entity_id) {
                            let dst = unsafe {
                                world.archetypes.traverse_insert(
                                    loc.archetype,
                                    component_idx,
                                    &mut world.components,
                                    &mut world.systems,
                                )
                            };

                            let component_ptr =
                                unsafe { event.event.add(component_offset as usize) }.cast_const();

                            unsafe {
                                world.archetypes.move_entity(
                                    loc,
                                    dst,
                                    [(component_idx, component_ptr)],
                                    &mut world.entities,
                                )
                            };

                            // Inserted component is owned by the archetype now. We wait to unpack
                            // in case one of the above functions panics.
                            event.unpack();
                        }
                    }
                    EventKind::Remove { component_idx } => {
                        // `Remove` doesn't need drop.
                        let (event, _) = event.unpack();

                        // SAFETY: `Remove` is `repr(transparent)` with the first field being the
                        // `EntityId`, so we can safely reinterpret this pointer.
                        let entity_id = unsafe { *event.cast::<EntityId>() };

                        if let Some(loc) = world.entities.get(entity_id) {
                            let dst = unsafe {
                                world.archetypes.traverse_remove(
                                    loc.archetype,
                                    component_idx,
                                    &mut world.components,
                                    &mut world.systems,
                                )
                            };

                            unsafe {
                                world
                                    .archetypes
                                    .move_entity(loc, dst, [], &mut world.entities)
                            };
                        }
                    }
                    EventKind::SpawnQueued => {
                        // `SpawnQueued` doesn't need drop.
                        let _ = event.unpack();

                        // Spawn one entity from the reserved entity queue.
                        world
                            .reserved_entities
                            .spawn_one(&mut world.entities, |id| world.archetypes.spawn(id));
                    }
                    EventKind::Despawn => {
                        // `Despawn` doesn't need drop.
                        let (event, _) = event.unpack();

                        let entity_id = unsafe { *event.cast::<Despawn>() }.0;

                        world
                            .archetypes
                            .remove_entity(entity_id, &mut world.entities);

                        // Reset next key iter.
                        world.reserved_entities.refresh(&world.entities);
                    }
                }
            }

            debug_assert!(queue_start_idx < world.event_queue.len());

            unsafe { world.event_queue.set_len(queue_start_idx) };
        }
    }

    /// Returns a new [`UnsafeWorldCell`] with permission to _read_ all data in
    /// this world.
    pub fn unsafe_cell(&self) -> UnsafeWorldCell {
        UnsafeWorldCell {
            world: (self as *const World).cast_mut(),
            _marker: PhantomData,
        }
    }

    /// Returns a new [`UnsafeWorldCell`] with permission to _read and write_
    /// all data in this world.
    pub fn unsafe_cell_mut(&mut self) -> UnsafeWorldCell {
        UnsafeWorldCell {
            world: self,
            _marker: PhantomData,
        }
    }
}

impl Default for World {
    fn default() -> Self {
        Self::new()
    }
}

impl Drop for World {
    fn drop(&mut self) {
        // Drop in-flight events in the event queue.
        for item in self.event_queue.iter() {
            if let Some(event) = NonNull::new(item.event) {
                let info = unsafe {
                    self.events
                        .get_by_index(item.meta.event_idx())
                        .unwrap_debug_checked()
                };

                if let Some(drop) = info.drop() {
                    unsafe { drop(event) };
                }
            }
        }
    }
}

unsafe impl Send for World {}
unsafe impl Sync for World {}

impl UnwindSafe for World {}
impl RefUnwindSafe for World {}

#[derive(Debug)]
pub struct Sender<'a> {
    world: &'a mut World,
}

impl Sender<'_> {
    pub fn send<E: Event>(&mut self, event: E) {
        let idx = self.world.add_event::<E>().index().as_u32();
        unsafe { self.world.event_queue.push(event, idx) };
    }

    pub fn spawn(&mut self) -> EntityId {
        let id = self.world.reserved_entities.reserve(&self.world.entities);
        unsafe {
            self.world
                .event_queue
                .push(SpawnQueued, EventId::SPAWN_QUEUED.index().as_u32())
        }
        self.send(Spawn(id));
        id
    }

    pub fn insert<C: Component>(&mut self, entity: EntityId, component: C) {
        self.send(Insert::new(entity, component))
    }

    pub fn remove<C: Component>(&mut self, entity: EntityId) {
        self.send(Remove::<C>::new(entity))
    }

    pub fn despawn(&mut self, entity: EntityId) {
        self.send(Despawn(entity))
    }
}

#[derive(Clone, Copy, Debug)]
pub struct UnsafeWorldCell<'a> {
    world: *mut World,
    _marker: PhantomData<(&'a World, &'a UnsafeCell<World>)>,
}

impl<'a> UnsafeWorldCell<'a> {
    /// # Safety
    ///
    /// - Must be called from within a system.
    /// - Must have permission to access the event queue mutably.
    /// - Event index must be correct for the given event.
    pub unsafe fn send_with_index<E: Event>(self, event: E, idx: u32) {
        unsafe { (*self.world).event_queue.push(event, idx) }
    }

    /// # Safety
    ///
    /// - Must be called from within a system.
    /// - Must have permission to access the event queue mutably.
    pub unsafe fn queue_spawn(self) -> EntityId {
        let entity_id = (*self.world).reserved_entities.reserve(self.entities());
        self.send_with_index(SpawnQueued, EventId::SPAWN_QUEUED.index().as_u32());
        entity_id
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

    pub fn events(self) -> &'a Events {
        unsafe { &(*self.world).events }
    }

    pub fn world(self) -> &'a World {
        unsafe { &*self.world }
    }

    pub unsafe fn world_mut(self) -> &'a mut World {
        &mut *self.world
    }
}

#[cfg(test)]
mod tests {
    use std::panic;
    use std::sync::Arc;

    use crate::prelude::*;

    #[test]
    fn world_drops_events() {
        #[derive(Event)]
        struct A(Arc<()>);

        impl Drop for A {
            fn drop(&mut self) {
                eprintln!("calling A destructor");
            }
        }

        #[derive(Event)]
        struct B(Arc<()>);

        impl Drop for B {
            fn drop(&mut self) {
                eprintln!("calling B destructor");
            }
        }

        #[derive(Event)]
        struct C(Arc<()>);

        impl Drop for C {
            fn drop(&mut self) {
                eprintln!("calling C destructor");
            }
        }

        let mut world = World::new();

        world.add_system(|r: Receiver<A>, mut s: Sender<B>| {
            s.send(B(r.event.0.clone()));
            s.send(B(r.event.0.clone()));
        });

        world.add_system(|r: Receiver<B>, mut s: Sender<C>| {
            s.send(C(r.event.0.clone()));
            s.send(C(r.event.0.clone()));
        });

        world.add_system(|r: Receiver<C>| println!("got C {:?}", Arc::as_ptr(&r.event.0)));

        let arc = Arc::new(());

        world.send(A(arc.clone()));

        drop(world);

        assert_eq!(Arc::strong_count(&arc), 1);
    }

    #[test]
    fn world_drops_events_on_panic() {
        #[derive(Event)]
        struct A(Arc<()>);

        impl Drop for A {
            fn drop(&mut self) {
                eprintln!("calling A destructor");
            }
        }

        #[derive(Event)]
        struct B(Arc<()>);

        impl Drop for B {
            fn drop(&mut self) {
                eprintln!("calling B destructor");
            }
        }

        #[derive(Event)]
        struct C(Arc<()>);

        impl Drop for C {
            fn drop(&mut self) {
                eprintln!("calling C destructor");
            }
        }

        let mut world = World::new();

        world.add_system(|r: Receiver<A>, mut s: Sender<B>| {
            s.send(B(r.event.0.clone()));
            s.send(B(r.event.0.clone()));
        });

        world.add_system(|r: Receiver<B>, mut sender: Sender<C>| {
            sender.send(C(r.event.0.clone()));
            sender.send(C(r.event.0.clone()));
        });

        world.add_system(|_: Receiver<C>| panic!("oops!"));

        let arc = Arc::new(());
        let arc_cloned = arc.clone();

        let res = panic::catch_unwind(move || world.send(A(arc_cloned)));

        assert_eq!(*res.unwrap_err().downcast::<&str>().unwrap(), "oops!");

        assert_eq!(Arc::strong_count(&arc), 1);
    }

    /*
    #[test]
    fn unsafe_world_cell_access() {
        let mut world = World::new();

        let cell = world.unsafe_cell_mut();

        todo!()
    }
    */
}
