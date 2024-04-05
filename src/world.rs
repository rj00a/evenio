//! Defines the [`World`] and related APIs.

#[cfg(not(feature = "std"))]
use alloc::{vec, vec::Vec};
use core::alloc::Layout;
use core::any::{self, TypeId};
use core::cell::UnsafeCell;
use core::fmt::Write;
use core::marker::PhantomData;
use core::mem;
use core::ptr::NonNull;

use crate::access::ComponentAccess;
use crate::archetype::Archetypes;
use crate::assert::{AssertMutable, UnwrapDebugChecked};
use crate::component::{
    AddComponent, Component, ComponentDescriptor, ComponentId, ComponentInfo, Components,
    RemoveComponent,
};
use crate::drop::{drop_fn_of, DropFn};
use crate::entity::{Entities, EntityId, EntityLocation, ReservedEntities};
use crate::event::{
    AddEvent, Despawn, Event, EventDescriptor, EventId, EventIdx, EventInfo, EventKind, EventMeta,
    EventPtr, EventQueue, Events, Insert, Remove, RemoveEvent, Spawn, SpawnQueued,
};
use crate::handler::{
    AddHandler, Handler, HandlerConfig, HandlerId, HandlerInfo, HandlerInfoInner, HandlerList,
    Handlers, InitError, IntoHandler, MaybeInvalidAccess, ReceivedEventId, RemoveHandler,
};

/// A container for all data in the ECS. This includes entities, components,
/// handlers, and events.
#[derive(Debug)]
pub struct World {
    entities: Entities,
    reserved_entities: ReservedEntities,
    components: Components,
    handlers: Handlers,
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
            handlers: Handlers::new(),
            archetypes: Archetypes::new(),
            events: Events::new(),
            event_queue: EventQueue::new(),
        }
    }

    /// Broadcast an event to all handlers in this world.
    ///
    /// Any events sent by handlers will also broadcast. This process continues
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
    /// fn my_handler(r: Receiver<MyEvent>) {
    ///     println!("got event: {}", r.event.0);
    /// }
    ///
    /// let mut world = World::new();
    ///
    /// world.add_handler(my_handler);
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
        let event_count = self.event_queue.len();
        let res = f(Sender { world: self });
        unsafe { self.event_queue.reverse_from(event_count) };

        self.flush_event_queue();

        res
    }

    /// Creates a new entity, returns its [`EntityId`], and sends the [`Spawn`]
    /// event to signal its creation.
    ///
    /// The new entity is spawned without any components attached. The returned
    /// `EntityId` is not used by any previous entities in this world.
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

    /// Sends the [`Insert`] event.
    ///
    /// This is equivalent to:
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

    /// Sends the [`Remove`] event.
    ///
    /// This is equivalent to:
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

    /// Sends the [`Despawn`] event.
    ///
    /// This is equivalent to:
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
    /// assert_eq!(world.get::<MyComponent>(e), Some(&MyComponent(123)));
    /// ```
    pub fn get<C: Component>(&self, entity: EntityId) -> Option<&C> {
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
    /// assert_eq!(world.get_mut::<MyComponent>(e), Some(&mut MyComponent(123)));
    /// ```
    pub fn get_mut<C: Component>(&mut self, entity: EntityId) -> Option<&mut C> {
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

    pub(crate) fn try_add_handler<H: IntoHandler<M>, M>(
        &mut self,
        handler: H,
    ) -> Result<HandlerId, String> {
        let mut handler = handler.into_handler();
        let mut config = HandlerConfig::default();

        let type_id = handler.type_id();

        if let Some(type_id) = type_id {
            if let Some(info) = self.handlers.get_by_type_id(type_id) {
                return Ok(info.id());
            }
        }

        let handler_name = handler.name();

        if let Err(e) = handler.init(self, &mut config) {
            return Err(format!("initialization of {handler_name} failed: {e}"));
        }

        let received_event = match config.received_event {
            ReceivedEventId::None => {
                return Err(format!(
                    "handler {handler_name} did not specify an event to receive"
                ));
            }
            ReceivedEventId::Ok(event) => event,
            ReceivedEventId::Invalid => {
                return Err(format!(
                    "handler {handler_name} attempted to listen for more than one event type"
                )
                .into())
            }
        };

        let received_event_access = match config.received_event_access {
            MaybeInvalidAccess::Ok(access) => access,
            MaybeInvalidAccess::Invalid => {
                panic!("handler {handler_name} has conflicting access to the received event")
            }
        };

        let event_queue_access = match config.event_queue_access {
            MaybeInvalidAccess::Ok(access) => access,
            MaybeInvalidAccess::Invalid => todo!(),
        };

        let component_access_conjunction = config
            .component_accesses
            .iter()
            .fold(ComponentAccess::new_true(), |acc, a| acc.and(a));

        let conflicts = component_access_conjunction.collect_conflicts();

        if !conflicts.is_empty() {
            let mut errmsg = format!(
                "handler {handler_name} contains conflicting component access (aliased \
                 mutability)\nconflicting components are...\n"
            );

            for idx in conflicts {
                errmsg += "- ";
                match self.components.get_by_index(idx) {
                    Some(info) => errmsg += info.name().as_ref(),
                    None => {
                        write!(&mut errmsg, "{idx:?}").unwrap();
                    }
                };
            }

            return Err(errmsg);
        }

        let component_access_disjunction = config
            .component_accesses
            .iter()
            .fold(ComponentAccess::new_false(), |acc, a| acc.or(a));

        let info = HandlerInfo::new(HandlerInfoInner {
            name: handler_name,
            id: HandlerId::NULL, // Filled in later.
            type_id,
            order: 0, // Filled in later.
            received_event,
            received_event_access,
            targeted_event_component_access: config.targeted_event_component_access,
            sent_untargeted_events: config.sent_untargeted_events,
            sent_targeted_events: config.sent_targeted_events,
            event_queue_access,
            component_access: component_access_conjunction,
            archetype_filter: component_access_disjunction,
            referenced_components: config.referenced_components,
            priority: config.priority,
            handler,
        });

        let id = self.handlers.add(info);
        let info = self.handlers.get_mut(id).unwrap();

        self.archetypes.register_handler(info);

        self.send(AddHandler(id));

        Ok(id)
    }

    /// Adds a new handler to the world, returns its [`HandlerId`], and sends
    /// the [`AddHandler`] event to signal its creation.
    ///
    /// If the handler already exists (as determined by [`Handler::type_id`])
    /// then the `HandlerId` of the existing handler is returned and no
    /// event is sent.
    ///
    /// # Panics
    ///
    /// Panics if the configuration of the handler is invalid. This can occur
    /// when, for instance, the handler does not specify an event to receive.
    ///
    /// ```should_panic
    /// # use evenio::prelude::*;
    /// #
    /// # let mut world = World::new();
    /// #
    /// world.add_handler(|| {}); // Panics
    /// ```
    ///
    /// # Examples
    ///
    /// ```
    /// use evenio::prelude::*;
    /// # #[derive(Event)]
    /// # struct MyEvent;
    ///
    /// fn my_handler(_: Receiver<MyEvent>) {};
    ///
    /// let mut world = World::new();
    /// let id = world.add_handler(my_handler);
    ///
    /// assert!(world.handlers().contains(id));
    /// ```
    ///
    /// [`Handler::type_id`]: crate::handler::Handler::type_id
    #[track_caller]
    pub fn add_handler<H: IntoHandler<M>, M>(&mut self, handler: H) -> HandlerId {
        match self.try_add_handler(handler) {
            Ok(id) => id,
            Err(e) => panic!("{e}"),
        }
    }

    /// Removes a handler from the world, returns its [`HandlerInfo`], and sends
    /// the [`RemoveHandler`] event. If the `handler` ID is invalid, then `None`
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
    /// let handler_id = world.add_handler(|_: Receiver<MyEvent>| {});
    ///
    /// let info = world.remove_handler(handler_id).unwrap();
    ///
    /// assert_eq!(info.id(), handler_id);
    /// assert!(!world.handlers().contains(handler_id));
    /// ```
    pub fn remove_handler(&mut self, handler: HandlerId) -> Option<HandlerInfo> {
        if !self.handlers.contains(handler) {
            return None;
        }

        self.send(RemoveHandler(handler));

        let info = self.handlers.remove(handler).unwrap();

        self.archetypes.remove_handler(&info);

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
    /// 3. All handlers that reference the component are removed.
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
    /// let handler = world.add_handler(|_: Receiver<E>, _: Fetcher<&C>| {});
    ///
    /// assert!(world.components().contains(component));
    /// assert!(world.handlers().contains(handler));
    ///
    /// world.remove_component(component);
    ///
    /// assert!(!world.components().contains(component));
    /// // Handler was also removed because it references `C` in its `Fetcher`.
    /// assert!(!world.handlers().contains(handler));
    /// ```
    pub fn remove_component(&mut self, component: ComponentId) -> Option<ComponentInfo> {
        if !self.components.contains(component) {
            return None;
        }

        // Send event first.
        self.send(RemoveComponent(component));

        let despawn_idx = self.add_event::<Despawn>().index().as_u32();

        // Attempt to despawn all entities that still have this component.
        for arch in self.archetypes.iter() {
            if arch.column_of(component.index()).is_some() {
                for &entity_id in arch.entity_ids() {
                    unsafe { self.event_queue.push_front(Despawn(entity_id), despawn_idx) };
                }
            }
        }

        self.flush_event_queue();

        // Remove all handlers that reference this component.
        let mut handlers_to_remove = vec![];

        for handler in self.handlers.iter() {
            if handler.references_component(component.index()) {
                handlers_to_remove.push(handler.id());
            }
        }

        for handler_id in handlers_to_remove {
            self.remove_handler(handler_id);
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

        let mut info = self
            .components
            .remove(component)
            .expect("component should still exist");

        // Remove all archetypes with this component. If there are still entities with
        // the component by this point, then they will be silently removed.
        self.archetypes
            .remove_component(&mut info, &mut self.components, |id| {
                self.entities.remove(id);
            });

        Some(info)
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
    ///
    /// [`add_event`]: World::add_event
    pub unsafe fn add_event_with_descriptor(&mut self, desc: EventDescriptor) -> EventId {
        let kind = desc.kind;

        let (id, is_new) = self.events.add(desc);

        if is_new {
            self.handlers.register_event(id.index());

            match kind {
                EventKind::Normal => {}
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
    /// 2. All handlers that send or receive the event are removed.
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

        // Remove all handlers that send or receive this event.
        let mut to_remove = vec![];

        for handler in self.handlers.iter() {
            if handler.received_event() == event
                || match event.index() {
                    EventIdx::Targeted(idx) => handler.sent_targeted_events().contains(idx),
                    EventIdx::Untargeted(idx) => handler.sent_untargeted_events().contains(idx),
                }
            {
                to_remove.push(handler.id());
            }
        }

        for id in to_remove {
            self.remove_handler(id);
        }

        let info = self.events.remove(event).unwrap();

        match info.kind() {
            EventKind::Normal => {}
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

    /// Returns the [`Handlers`] for this world.
    pub fn handlers(&self) -> &Handlers {
        &self.handlers
    }

    /// Returns the [`Archetypes`] for this world.
    pub fn archetypes(&self) -> &Archetypes {
        &self.archetypes
    }

    /// Returns the [`Events`] for this world.
    pub fn events(&self) -> &Events {
        &self.events
    }

    /// Send all queued events to handlers. The event queue will be empty after
    /// this call.
    fn flush_event_queue(&mut self) {
        'next_event: while let Some(item) = self.event_queue.pop_front() {
            let event_meta = item.meta;
            let event_info = unsafe {
                self.events
                    .get_by_index(event_meta.event_idx())
                    .unwrap_debug_checked()
            };
            let event_kind = event_info.kind();

            // In case `Handler::run` unwinds, we need to drop the event we're holding on
            // the stack. The other events in the event queue will be handled by
            // `World`'s destructor.
            struct EventDropper {
                event: NonNull<u8>,
                drop: DropFn,
                ownership_flag: bool,
            }

            impl EventDropper {
                /// Extracts the event pointer and drop fn without running
                /// the destructor.
                #[inline]
                fn unpack(self) -> (NonNull<u8>, DropFn) {
                    let event = self.event;
                    let drop = self.drop;
                    mem::forget(self);

                    (event, drop)
                }
            }

            impl Drop for EventDropper {
                fn drop(&mut self) {
                    if !self.ownership_flag {
                        if let Some(drop) = self.drop {
                            unsafe { drop(self.event) };
                        }
                    }
                }
            }

            let mut event = EventDropper {
                event: item.event,
                drop: event_info.drop(),
                ownership_flag: false,
            };

            let (handler_list, target_location) = match event_meta {
                EventMeta::Untargeted { idx } => (
                    unsafe {
                        self.handlers
                            .get_untargeted_list(idx)
                            .unwrap_debug_checked()
                    },
                    EntityLocation::NULL,
                ),
                EventMeta::Targeted { idx, target } => {
                    let Some(location) = self.entities.get(target) else {
                        // Entity doesn't exist. Skip the event.
                        continue;
                    };

                    let arch = unsafe {
                        self.archetypes
                            .get(location.archetype)
                            .unwrap_debug_checked()
                    };

                    static EMPTY: HandlerList = HandlerList::new();

                    // Return an empty handler list instead of continuing in case this event is
                    // special.
                    (arch.handler_list_for(idx).unwrap_or(&EMPTY), location)
                }
            };

            let handlers: *const [_] = handler_list.handlers();

            let events_before = self.event_queue.len();

            for mut info_ptr in unsafe { (*handlers).iter().copied() } {
                let info = unsafe { info_ptr.as_info_mut() };

                let handler: *mut dyn Handler = info.handler_mut();

                let event_ptr =
                    EventPtr::new(event.event, NonNull::from(&mut event.ownership_flag));

                let world_cell = self.unsafe_cell_mut();

                unsafe { (*handler).run(info, event_ptr, target_location, world_cell) };

                // Did the handler take ownership of the event?
                if event.ownership_flag {
                    // Don't drop event since we don't own it anymore.
                    event.unpack();

                    // Reverse pushed events so they're handled in FIFO order.
                    unsafe { self.event_queue.reverse_from(events_before) };

                    continue 'next_event;
                }
            }

            // Reverse pushed events so they're handled in FIFO order.
            unsafe { self.event_queue.reverse_from(events_before) };

            match event_kind {
                EventKind::Normal => {
                    // Ordinary event. Run drop fn.
                    if let (ptr, Some(drop)) = event.unpack() {
                        unsafe { drop(ptr) };
                    }
                }
                EventKind::Insert {
                    component_idx,
                    component_offset,
                } => {
                    let entity_id = unsafe { *event.event.as_ptr().cast::<EntityId>() };

                    if let Some(loc) = self.entities.get(entity_id) {
                        let dst = unsafe {
                            self.archetypes.traverse_insert(
                                loc.archetype,
                                component_idx,
                                &mut self.components,
                                &mut self.handlers,
                            )
                        };

                        let component_ptr =
                            unsafe { event.event.as_ptr().add(component_offset as usize) }
                                .cast_const();

                        unsafe {
                            self.archetypes.move_entity(
                                loc,
                                dst,
                                [(component_idx, component_ptr)],
                                &mut self.entities,
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
                    let entity_id = unsafe { *event.as_ptr().cast::<EntityId>() };

                    if let Some(loc) = self.entities.get(entity_id) {
                        let dst = unsafe {
                            self.archetypes.traverse_remove(
                                loc.archetype,
                                component_idx,
                                &mut self.components,
                                &mut self.handlers,
                            )
                        };

                        unsafe {
                            self.archetypes
                                .move_entity(loc, dst, [], &mut self.entities)
                        };
                    }
                }
                EventKind::SpawnQueued => {
                    // `SpawnQueued` doesn't need drop.
                    let _ = event.unpack();

                    // Spawn one entity from the reserved entity queue.
                    self.reserved_entities
                        .spawn_one(&mut self.entities, |id| self.archetypes.spawn(id));
                }
                EventKind::Despawn => {
                    // `Despawn` doesn't need drop.
                    let (event, _) = event.unpack();

                    let entity_id = unsafe { *event.as_ptr().cast::<Despawn>() }.0;

                    self.archetypes.remove_entity(entity_id, &mut self.entities);

                    // Reset next key iter.
                    self.reserved_entities.refresh(&self.entities);
                }
            }
        }

        self.event_queue.clear();
    }

    /// Returns a new [`UnsafeWorldCell`] with permission to _read_ all data in
    /// this world.
    pub fn unsafe_cell(&self) -> UnsafeWorldCell {
        UnsafeWorldCell {
            world: NonNull::from(self),
            _marker: PhantomData,
        }
    }

    /// Returns a new [`UnsafeWorldCell`] with permission to _read and write_
    /// all data in this world.
    pub fn unsafe_cell_mut(&mut self) -> UnsafeWorldCell {
        UnsafeWorldCell {
            world: NonNull::from(self),
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
            let info = unsafe {
                self.events
                    .get_by_index(item.meta.event_idx())
                    .unwrap_debug_checked()
            };

            if let Some(drop) = info.drop() {
                unsafe { drop(item.event) };
            }
        }
    }
}

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
        unsafe { self.world.event_queue.push_front(event, idx) };
    }

    /// Enqueue the spawning of an entity and [`Spawn`] event. Returns the
    /// [`EntityId`] of the entity that will be spawned.
    pub fn spawn(&mut self) -> EntityId {
        let id = self.world.reserved_entities.reserve(&self.world.entities);
        unsafe {
            self.world
                .event_queue
                .push_front(SpawnQueued, EventId::SPAWN_QUEUED.index().as_u32())
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

/// Reference to a [`World`] where all methods take `&self` and aliasing rules
/// are not checked. It is the caller's responsibility to ensure that
/// Rust's aliasing rules are not violated.
#[derive(Clone, Copy, Debug)]
pub struct UnsafeWorldCell<'a> {
    world: NonNull<World>,
    _marker: PhantomData<(&'a World, &'a UnsafeCell<World>)>,
}

impl<'a> UnsafeWorldCell<'a> {
    /// # Safety
    ///
    /// - Must be called from within a handler.
    /// - Must have permission to access the event queue mutably.
    /// - Event index must be correct for the given event.
    pub unsafe fn send_with_index<E: Event>(self, event: E, idx: u32) {
        unsafe { (*self.world.as_ptr()).event_queue.push_front(event, idx) }
    }

    /// # Safety
    ///
    /// - Must be called from within a handler.
    /// - Must have permission to access the event queue mutably.
    pub unsafe fn queue_spawn(self) -> EntityId {
        let entity_id = (*self.world.as_ptr())
            .reserved_entities
            .reserve(self.entities());
        self.send_with_index(SpawnQueued, EventId::SPAWN_QUEUED.index().as_u32());
        entity_id
    }

    /// Returns the [`Entities`] for this world.
    pub fn entities(self) -> &'a Entities {
        unsafe { &(*self.world.as_ptr()).entities }
    }

    /// Returns the [`Components`] for this world.
    pub fn components(self) -> &'a Components {
        unsafe { &(*self.world.as_ptr()).components }
    }

    /// Returns the [`Handlers`] for this world.
    pub fn handlers(self) -> &'a Handlers {
        unsafe { &(*self.world.as_ptr()).handlers }
    }

    /// Returns the [`Archetypes`] for this world.
    pub fn archetypes(self) -> &'a Archetypes {
        unsafe { &(*self.world.as_ptr()).archetypes }
    }

    /// Returns the [`Events`] for this world.
    pub fn events(self) -> &'a Events {
        unsafe { &(*self.world.as_ptr()).events }
    }

    /// Returns an immutable reference to the underlying world.
    ///
    /// # Safety
    ///
    /// Must have permission to access the entire world immutably.
    pub fn world(self) -> &'a World {
        unsafe { &*self.world.as_ptr() }
    }

    /// Returns a mutable reference to the underlying world.
    ///
    /// # Safety
    ///
    /// Must have permission the access the entire world immutably.
    pub unsafe fn world_mut(self) -> &'a mut World {
        &mut *self.world.as_ptr()
    }
}

// SAFETY: `&World` and `&mut World` are `Send`.
unsafe impl Send for UnsafeWorldCell<'_> {}
// SAFETY: `&World` and `&mut World` are `Sync`.
unsafe impl Sync for UnsafeWorldCell<'_> {}

#[cfg(test)]
mod tests {
    use alloc::sync::Arc;
    use core::panic::{RefUnwindSafe, UnwindSafe};
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

        world.add_handler(|r: Receiver<A>, mut s: Sender<B>| {
            s.send(B(r.event.0.clone()));
            s.send(B(r.event.0.clone()));
        });

        world.add_handler(|r: Receiver<B>, mut s: Sender<C>| {
            s.send(C(r.event.0.clone()));
            s.send(C(r.event.0.clone()));
        });

        world.add_handler(|r: Receiver<C>| println!("got C {:?}", Arc::as_ptr(&r.event.0)));

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

        world.add_handler(|r: Receiver<A>, mut s: Sender<B>| {
            s.send(B(r.event.0.clone()));
            s.send(B(r.event.0.clone()));
        });

        world.add_handler(|r: Receiver<B>, mut sender: Sender<C>| {
            sender.send(C(r.event.0.clone()));
            sender.send(C(r.event.0.clone()));
        });

        world.add_handler(|_: Receiver<C>| panic!("oops!"));

        let arc = Arc::new(());
        let arc_cloned = arc.clone();

        let res = panic::catch_unwind(move || world.send(A(arc_cloned)));

        assert_eq!(*res.unwrap_err().downcast::<&str>().unwrap(), "oops!");

        assert_eq!(Arc::strong_count(&arc), 1);
    }

    /// Asserts that `World` has the expected auto trait implementations.
    fn _assert_auto_trait_impls()
    where
        World: Send + Sync + UnwindSafe + RefUnwindSafe,
        for<'a> &'a World: Send + Sync,
        for<'a> &'a mut World: Send + Sync,
    {
    }
}
