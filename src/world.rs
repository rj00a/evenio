//! Defines the [`World`] and related APIs.

use alloc::vec;
use alloc::vec::Vec;
use core::alloc::Layout;
use core::any::{self, TypeId};
use core::cell::UnsafeCell;
use core::marker::PhantomData;
use core::mem;
use core::panic::{RefUnwindSafe, UnwindSafe};
use core::ptr::{self, NonNull};

use crate::archetype::Archetypes;
use crate::assert::{AssertMutable, UnwrapDebugChecked};
use crate::component::{
    AddComponent, Component, ComponentDescriptor, ComponentId, ComponentInfo, Components,
    RemoveComponent,
};
use crate::drop::{drop_fn_of, DropFn};
use crate::entity::{Entities, EntityId, ReservedEntities};
use crate::event::{
    AddEvent, Despawn, Event, EventDescriptor, EventId, EventIdx, EventInfo, EventKind, EventMeta,
    EventPtr, EventQueue, Events, Insert, Remove, RemoveEvent, Spawn, SpawnQueued,
};
use crate::system::{
    AddSystem, Config, IntoSystem, RemoveSystem, System, SystemId, SystemInfo, SystemInfoInner,
    SystemList, Systems,
};

/// A container for all data in the ECS. This includes entities, components,
/// systems, and events.
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
    /// Creates a new, empty world.
    ///
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

    /// Broadcast an event to all systems in this world.
    ///
    /// Any events sent by systems will also broadcast. This process continues
    /// recursively until all events have finished broadcasting.
    ///
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
    ///
    /// Output:
    ///
    /// ```txt
    /// got event: 123
    /// ```
    pub fn send<E: Event>(&mut self, event: E) {
        self.send_many(|mut s| s.send(event))
    }

    /// Enqueue an arbitrary number of events and send them all at once.
    ///
    /// The closure `f` is passed a [`Sender`] used to add events to a queue.
    /// Once the closure returns, all enqueued events are broadcasted as
    /// described by [`send`].
    ///
    /// [`send`]: World::send
    ///
    /// # Examples
    ///
    /// ```
    /// # use evenio::prelude::*;
    /// #
    /// # #[derive(Event)]
    /// # struct A;
    /// #
    /// # #[derive(Event)]
    /// # struct B;
    /// #
    /// # let mut world = World::new();
    /// #
    /// world.send_many(|mut sender| {
    ///     sender.send(A);
    ///     sender.send(B);
    /// });
    /// ```
    pub fn send_many<F, R>(&mut self, f: F) -> R
    where
        F: FnOnce(Sender) -> R,
    {
        let res = f(Sender { world: self });

        self.flush_event_queue();

        res
    }

    /// Creates a new entity, returns its [`EntityId`], and sends the [`Spawn`]
    /// event to signal its creation.
    ///
    /// The new entity is spawned without any components attached. The returned
    /// `EntityId` is not used by any previous entities in this world.
    ///
    /// # Panics
    ///
    /// Panics if the maximum number of entities has been reached.
    ///
    /// # Examples
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

    /// Sends the [`Insert`] event. This is a shorthand for:
    ///
    /// ```
    /// # use evenio::prelude::*;
    /// #
    /// # let mut world = World::new();
    /// #
    /// # let entity = world.spawn();
    /// #
    /// # #[derive(Component)]
    /// # struct C;
    /// #
    /// # let component = C;
    /// #
    /// world.send(Insert::new(entity, component));
    /// ```
    pub fn insert<C: Component>(&mut self, entity: EntityId, component: C) {
        self.send(Insert::new(entity, component))
    }

    /// Sends the [`Remove`] event. This is a shorthand for:
    ///
    /// ```
    /// # use evenio::prelude::*;
    /// #
    /// # let mut world = World::new();
    /// #
    /// # let entity = world.spawn();
    /// #
    /// # #[derive(Component)]
    /// # struct C;
    /// #
    /// world.send(Remove::<C>::new(entity));
    /// ```
    pub fn remove<C: Component>(&mut self, entity: EntityId) {
        self.send(Remove::<C>::new(entity))
    }

    /// Sends the [`Despawn`] event. This is shorthand for:
    ///
    /// ```
    /// # use evenio::prelude::*;
    /// #
    /// # let mut world = World::new();
    /// #
    /// # let entity = world.spawn();
    /// #
    /// world.send(Despawn(entity));
    /// ```
    pub fn despawn(&mut self, entity: EntityId) {
        self.send(Despawn(entity))
    }

    /// Gets an immutable reference to component `C` on `entity`. Returns `None`
    /// if `entity` doesn't exist or doesn't have the requested component.
    ///
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

    /// Gets a mutable reference to component `C` on `entity`. Returns `None` if
    /// `entity` doesn't exist or doesn't have the requested component.
    ///
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
        let () = AssertMutable::<C>::COMPONENT;

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

    /// Adds a new system to the world, returns its [`SystemId`], and sends the
    /// [`AddSystem`] event to signal its creation.
    ///
    /// If the system already exists (as determined by [`System::type_id`]) then
    /// the `SystemId` of the existing system is returned and no event is sent.
    ///
    /// # Panics
    ///
    /// Panics if the configuration of the system is invalid. This can occur
    /// when, for instance, the system does not specify an event to receive.
    ///
    /// ```should_panic
    /// # use evenio::prelude::*;
    /// #
    /// # let mut world = World::new();
    /// #
    /// world.add_system(|| {}); // Panics
    /// ```
    ///
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
    ///
    /// assert!(world.systems().contains(id));
    /// ```
    ///
    /// [`System::type_id`]: crate::system::System::type_id
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

    /// Removes a system from the world, returns its [`SystemInfo`], and sends
    /// the [`RemoveSystem`] event. If the `system` ID is invalid, then `None`
    /// is returned and no event is sent.
    ///
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
    pub fn remove_system(&mut self, system: SystemId) -> Option<SystemInfo> {
        if !self.systems.contains(system) {
            return None;
        }

        self.send(RemoveSystem(system));

        let info = self.systems.remove(system).unwrap();

        self.archetypes.remove_system(&info);

        Some(info)
    }

    /// Adds the component `C` to the world, returns its [`ComponentId`], and
    /// sends the [`AddComponent`] event to signal its creation.
    ///
    /// If the component already exists, then the [`ComponentId`] of the
    /// existing component is returned and no event is sent.
    ///
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

    /// Adds a component described by a given [`ComponentDescriptor`].
    ///
    /// Like [`add_component`], an [`AddComponent`] event is sent if the
    /// component is newly added. If the [`TypeId`] of the component matches an
    /// existing component, then the existing component's [`ComponentId`] is
    /// returned and no event is sent.
    ///
    /// # Safety
    ///
    /// - If the component is given a [`TypeId`], then the `layout` and `drop`
    ///   function must be compatible with the Rust type identified by the type
    ///   ID.
    /// - Drop function must be safe to call with a pointer to the component as
    ///   described by [`DropFn`]'s documentation.
    ///
    /// [`add_component`]: World::add_component
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

    /// Removes a component from the world and returns its [`ComponentInfo`]. If
    /// the `component` ID is invalid, then `None` is returned and the function
    /// has no effect.
    ///
    /// Removing a component has the following effects in the order listed:
    /// 1. The [`RemoveComponent`] event is sent.
    /// 2. All entities with the component are despawned.
    /// 3. All systems that reference the component are removed.
    /// 4. The corresponding [`Insert`] events for the component are removed.
    /// 5. The corresponding [`Remove`] events for the component are removed.
    ///
    /// # Examples
    ///
    /// ```
    /// # use evenio::prelude::*;
    /// # let mut world = World::new();
    /// # #[derive(Component)] struct C;
    /// # #[derive(Event)] struct E;
    /// #
    /// let component = world.add_component::<C>();
    /// let system = world.add_system(|_: Receiver<E>, _: Fetcher<&C>|);
    ///
    /// assert!(world.components().contains(component));
    /// assert!(world.systems().contains(system));
    ///
    /// world.remove_component(component);
    ///
    /// assert!(!world.components().contains(component));
    /// // System was also removed because it references `C` in its `Fetcher`.
    /// assert!(!world.systems().contains(system));
    /// ```
    pub fn remove_component(&mut self, component: ComponentId) -> Option<ComponentInfo> {
        if !self.components.contains(component) {
            return None;
        }

        self.send(RemoveComponent(component));

        let despawn_idx = self.add_event::<Despawn>().index().as_u32();

        // Attempt to despawn all entities that still have this component.
        for arch in self.archetypes.iter() {
            if arch.column_of(component.index()).is_some() {
                for &entity_id in arch.entity_ids() {
                    unsafe { self.event_queue.push(Despawn(entity_id), despawn_idx) };
                }
            }
        }

        self.flush_event_queue();

        // Remove all systems that reference this component.
        let mut systems_to_remove = vec![];

        for sys in self.systems.iter() {
            if sys.referenced_components().contains(component.index()) {
                systems_to_remove.push(sys.id());
            }
        }

        for sys_id in systems_to_remove {
            self.remove_system(sys_id);
        }

        let info = &self.components[component];

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
        self.archetypes
            .remove_component(component.index(), |entity_id| {
                self.entities.remove(entity_id);
            });

        self.components.remove(component)
    }

    /// Adds the event `E` to the world, returns its [`EventId`], and sends the
    /// [`AddEvent`] event to signal its creation.
    ///
    /// If the event already exists, then the [`EventId`] of the existing event
    /// is returned and no event is sent.
    ///
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

    /// Adds an event described by a given [`EventDescriptor`].
    ///
    /// Like [`add_event`], an [`AddEvent`] event is sent if the
    /// event is newly added. If the [`TypeId`] of the event matches an
    /// existing event, then the existing event's [`EventId`] is
    /// returned and no event is sent.
    ///
    /// # Safety
    ///
    /// - If the event is given a [`TypeId`], then the `layout` and `drop`
    ///   function must be compatible with the Rust type identified by the type
    ///   ID.
    /// - Drop function must be safe to call with a pointer to the event as
    ///   described by [`DropFn`]'s documentation.
    /// - The event's kind must be correct for the descriptor. See
    ///   [`EventKind`]'s documentation for more information.
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

    /// Removes an event from the world and returns its [`EventInfo`]. If
    /// the `event` ID is invalid, then `None` is returned and the function
    /// has no effect.
    ///
    /// Removing an event has the following effects in the order listed:
    /// 1. The [`RemoveEvent`] event is sent.
    /// 2. All systems that send or receive the event are removed.
    ///
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
    pub fn remove_event(&mut self, event: EventId) -> Option<EventInfo> {
        assert!(self.event_queue.is_empty());

        if !self.events.contains(event) || event == EventId::SPAWN_QUEUED {
            return None;
        }

        // Send event before removing anything.
        self.send(RemoveEvent(event));

        // Remove all systems that send or receive this event.
        let mut to_remove = vec![];

        for sys in self.systems.iter() {
            if sys.received_event() == event
                || match event.index() {
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

        let info = self.events.remove(event).unwrap();

        match info.kind() {
            EventKind::Other => {}
            EventKind::Insert { component_idx, .. } => {
                if let Some(info) = self.components.get_by_index_mut(component_idx) {
                    info.insert_events.remove(&event);
                }
            }
            EventKind::Remove { component_idx } => {
                if let Some(info) = self.components.get_by_index_mut(component_idx) {
                    info.remove_events.remove(&event);
                }
            }
            EventKind::SpawnQueued => {}
            EventKind::Despawn => {}
        }

        Some(info)
    }

    /// Returns the [`Entities`] for this world.
    pub fn entities(&self) -> &Entities {
        &self.entities
    }

    /// Returns the [`Components`] for this world.  
    pub fn components(&self) -> &Components {
        &self.components
    }

    /// Returns the [`Systems`] for this world.
    pub fn systems(&self) -> &Systems {
        &self.systems
    }

    /// Returns the [`Archetypes`] for this world.
    pub fn archetypes(&self) -> &Archetypes {
        &self.archetypes
    }

    /// Returns the [`Events`] for this world.
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
        // Drop in-flight events still in the event queue. This can happen if a panic
        // occurs.
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

/// Used for queueing events. Passed to the closure given in [`send_many`].
///
/// [`send_many`]: World::send_many
#[derive(Debug)]
pub struct Sender<'a> {
    world: &'a mut World,
}

impl Sender<'_> {
    /// Enqueue an event.
    pub fn send<E: Event>(&mut self, event: E) {
        let idx = self.world.add_event::<E>().index().as_u32();
        unsafe { self.world.event_queue.push(event, idx) };
    }

    /// Enqueue the spawning of an entity and [`Spawn`] event. Returns the
    /// [`EntityId`] of the entity that will be spawned.
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

    /// Enqueue an [`Insert`] event.
    pub fn insert<C: Component>(&mut self, entity: EntityId, component: C) {
        self.send(Insert::new(entity, component))
    }

    /// Enqueue a [`Remove`] event.
    pub fn remove<C: Component>(&mut self, entity: EntityId) {
        self.send(Remove::<C>::new(entity))
    }

    /// Enqueue a [`Despawn`] event.
    pub fn despawn(&mut self, entity: EntityId) {
        self.send(Despawn(entity))
    }
}

/// Reference to a [`World`] where all methods take `&self` and accesses are not
/// checked at compile time. It is the caller's responsibility to ensure that
/// Rust's aliasing rules are not violated.
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

    /// Returns the [`Entities`] for this world.
    pub fn entities(self) -> &'a Entities {
        unsafe { &(*self.world).entities }
    }

    /// Returns the [`Components`] for this world.
    pub fn components(self) -> &'a Components {
        unsafe { &(*self.world).components }
    }

    /// Returns the [`Systems`] for this world.
    pub fn systems(self) -> &'a Systems {
        unsafe { &(*self.world).systems }
    }

    /// Returns the [`Archetypes`] for this world.
    pub fn archetypes(self) -> &'a Archetypes {
        unsafe { &(*self.world).archetypes }
    }

    /// Returns the [`Events`] for this world.
    pub fn events(self) -> &'a Events {
        unsafe { &(*self.world).events }
    }

    /// Returns an immutable reference to the underlying world.
    ///
    /// # Safety
    ///
    /// Must have permission to access the entire world immutably.
    pub fn world(self) -> &'a World {
        unsafe { &*self.world }
    }

    /// Returns a mutable reference to the underlying world.
    ///
    /// # Safety
    ///
    /// Must have permission the access the entire world immutably.
    pub unsafe fn world_mut(self) -> &'a mut World {
        &mut *self.world
    }
}

#[cfg(test)]
mod tests {
    use alloc::sync::Arc;
    use std::panic;

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

        #[allow(dead_code)]
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
}
