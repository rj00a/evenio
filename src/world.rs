//! Defines the [`World`] and related APIs.

#[cfg(not(feature = "std"))]
use alloc::{format, string::String, vec, vec::Vec};
use core::alloc::Layout;
use core::any::{self, TypeId};
use core::fmt::Write;
use core::marker::PhantomData;
use core::mem;
use core::ptr::NonNull;

use bumpalo::Bump;

use crate::access::ComponentAccess;
use crate::archetype::Archetypes;
use crate::component::{
    AddComponent, Component, ComponentDescriptor, ComponentId, ComponentInfo, Components,
    RemoveComponent,
};
use crate::drop::{drop_fn_of, DropFn};
use crate::entity::{Entities, EntityId, EntityLocation, ReservedEntities};
use crate::event::{
    AddGlobalEvent, AddTargetedEvent, Despawn, EventDescriptor, EventKind, EventMeta, EventPtr,
    EventQueueItem, GlobalEvent, GlobalEventId, GlobalEventIdx, GlobalEventInfo, GlobalEvents,
    Insert, Remove, RemoveGlobalEvent, RemoveTargetedEvent, Spawn, TargetedEvent, TargetedEventId,
    TargetedEventIdx, TargetedEventInfo, TargetedEvents,
};
use crate::handler::{
    AddHandler, Handler, HandlerConfig, HandlerId, HandlerInfo, HandlerInfoInner, HandlerList,
    Handlers, IntoHandler, MaybeInvalidAccess, ReceivedEventId, RemoveHandler,
};
use crate::mutability::{Mutability, Mutable};

/// A container for all data in the ECS. This includes entities, components,
/// handlers, and events.
#[derive(Debug)]
pub struct World {
    entities: Entities,
    reserved_entities: ReservedEntities,
    components: Components,
    handlers: Handlers,
    archetypes: Archetypes,
    global_events: GlobalEvents,
    targeted_events: TargetedEvents,
    event_queue: Vec<EventQueueItem>,
    bump: Bump,
    /// So the world doesn't accidentally implement `Send` or `Sync`.
    _marker: PhantomData<*const ()>,
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
            global_events: GlobalEvents::new(),
            targeted_events: TargetedEvents::new(),
            event_queue: vec![],
            bump: Bump::new(),
            _marker: PhantomData,
        }
    }

    /// Broadcast a global event to all handlers in this world. All handlers
    /// which listen for this event
    ///
    /// Any events sent by handlers will also broadcast. This process continues
    /// recursively until all events have finished broadcasting.
    ///
    /// See also [`World::send_to`] to send a [`TargetedEvent`].
    ///
    /// # Examples
    ///
    /// ```
    /// use evenio::prelude::*;
    ///
    /// #[derive(GlobalEvent)]
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
    pub fn send<E: GlobalEvent>(&mut self, event: E) {
        let idx = self.add_global_event::<E>().index();

        self.event_queue.push(EventQueueItem {
            meta: EventMeta::Global { idx },
            event: NonNull::from(self.bump.alloc(event)).cast(),
        });

        self.flush_event_queue();
    }

    /// Broadcast a targeted event to all handlers in this world.
    ///
    /// Any events sent by handlers will also broadcast. This process continues
    /// recursively until all events have finished broadcasting.
    ///
    /// See also [`World::send`] to send a [`GlobalEvent`].
    ///
    /// # Examples
    ///
    /// ```
    /// use evenio::prelude::*;
    ///
    /// #[derive(TargetedEvent)]
    /// struct MyEvent(i32);
    ///
    /// fn my_handler(r: Receiver<MyEvent, EntityId>) {
    ///     println!("target of received event is {:?}", r.query);
    /// }
    ///
    /// let mut world = World::new();
    ///
    /// world.add_handler(my_handler);
    ///
    /// let target = world.spawn();
    ///
    /// // Send my event to `target` entity.
    /// world.send_to(target, MyEvent(123));
    /// ```
    pub fn send_to<E: TargetedEvent>(&mut self, target: EntityId, event: E) {
        let idx = self.add_targeted_event::<E>().index();

        self.event_queue.push(EventQueueItem {
            meta: EventMeta::Targeted { target, idx },
            event: NonNull::from(self.bump.alloc(event)).cast(),
        });

        self.flush_event_queue();
    }

    /// Creates a new entity, returns its [`EntityId`], and sends the [`Spawn`]
    /// event to signal its creation.
    ///
    /// The new entity is spawned without any components attached. The returned
    /// `EntityId` will not have been used by any previous entities in this
    /// world.
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
        let id = self.reserved_entities.reserve(&self.entities);

        self.send(Spawn(id));

        id
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
    /// world.send_to(entity, Insert(component));
    /// ```
    pub fn insert<C: Component>(&mut self, entity: EntityId, component: C) {
        self.send_to(entity, Insert(component))
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
    /// world.send_to(entity, Remove::<C>);
    /// ```
    pub fn remove<C: Component>(&mut self, entity: EntityId) {
        self.send_to(entity, Remove::<C>)
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
    /// world.send_to(entity, Despawn);
    /// ```
    pub fn despawn(&mut self, entity: EntityId) {
        self.send_to(entity, Despawn)
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

        let arch = unsafe { self.archetypes().get(loc.archetype).unwrap_unchecked() };

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
    pub fn get_mut<C: Component<Mutability = Mutable>>(
        &mut self,
        entity: EntityId,
    ) -> Option<&mut C> {
        let loc = self.entities.get(entity)?;

        let component_idx = self
            .components()
            .get_by_type_id(TypeId::of::<C>())?
            .id()
            .index();

        let arch = unsafe { self.archetypes().get(loc.archetype).unwrap_unchecked() };

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
                ))
            }
        };

        let received_event_access = match config.received_event_access {
            MaybeInvalidAccess::Ok(access) => access,
            MaybeInvalidAccess::Invalid => {
                return Err(format!(
                    "handler {handler_name} has conflicting access to the received event"
                ))
            }
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
                    Some(info) => errmsg += info.name(),
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
            sent_untargeted_events: config.sent_global_events,
            sent_targeted_events: config.sent_targeted_events,
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
    /// # #[derive(GlobalEvent)]
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
    /// # #[derive(GlobalEvent)]
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
            mutability: Mutability::of::<C::Mutability>(),
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
    /// # #[derive(GlobalEvent)] struct E;
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

        let despawn_idx = self.add_targeted_event::<Despawn>().index();

        // Attempt to despawn all entities that still have this component.
        for arch in self.archetypes.iter() {
            if arch.column_of(component.index()).is_some() {
                for &entity_id in arch.entity_ids() {
                    self.event_queue.push(EventQueueItem {
                        meta: EventMeta::Targeted {
                            idx: despawn_idx,
                            target: entity_id,
                        },
                        event: NonNull::<Despawn>::dangling().cast(),
                    });
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
            self.remove_targeted_event(event);
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

    /// Adds the global event `E` to the world, returns its [`GlobalEventId`],
    /// and sends the [`AddGlobalEvent`] event to signal its creation.
    ///
    /// If the event already exists, then the [`GlobalEventId`] of the existing
    /// event is returned and no event is sent.
    ///
    /// # Examples
    ///
    /// ```
    /// use evenio::prelude::*;
    ///
    /// #[derive(GlobalEvent)]
    /// struct MyEvent;
    ///
    /// let mut world = World::new();
    /// let id = world.add_global_event::<MyEvent>();
    ///
    /// assert_eq!(id, world.add_global_event::<MyEvent>());
    /// ```
    pub fn add_global_event<E: GlobalEvent>(&mut self) -> GlobalEventId {
        let desc = EventDescriptor::new::<E>(self);
        unsafe { self.add_global_event_with_descriptor(desc) }
    }

    /// Adds the targeted event `E` to the world, returns its
    /// [`TargetedEventId`], and sends the [`AddTargetedEvent`] event to
    /// signal its creation.
    ///
    /// If the event already exists, then the [`TargetedEventId`] of the
    /// existing event is returned and no event is sent.
    ///
    /// # Examples
    ///
    /// ```
    /// use evenio::prelude::*;
    ///
    /// #[derive(TargetedEvent)]
    /// struct MyEvent;
    ///
    /// let mut world = World::new();
    /// let id = world.add_targeted_event::<MyEvent>();
    ///
    /// assert_eq!(id, world.add_targeted_event::<MyEvent>());
    /// ```
    pub fn add_targeted_event<E: TargetedEvent>(&mut self) -> TargetedEventId {
        let desc = EventDescriptor::new::<E>(self);
        unsafe { self.add_targeted_event_with_descriptor(desc) }
    }

    /// Adds a global event described by a given [`EventDescriptor`].
    ///
    /// Like [`add_global_event`], an [`AddGlobalEvent`] event is sent if the
    /// event is newly added. If the [`TypeId`] of the event matches an
    /// existing global event, then the existing event's [`GlobalEventId`] is
    /// returned and no event is sent.
    ///
    /// # Safety
    ///
    /// - If the event is given a [`TypeId`], then the `layout` and `drop`
    ///   function must be compatible with the Rust type identified by the type
    ///   ID.
    /// - Drop function must be safe to call with a pointer to the event as
    ///   described by [`DropFn`]'s documentation.
    /// - The event's kind must be correct for the descriptor. See [`EventKind`]
    ///   for more information.
    ///
    /// [`add_global_event`]: World::add_global_event
    pub unsafe fn add_global_event_with_descriptor(
        &mut self,
        desc: EventDescriptor,
    ) -> GlobalEventId {
        let (id, is_new) = self.global_events.add(desc);

        if is_new {
            self.handlers.register_event(id.index());
            self.send(AddGlobalEvent(id));
        }

        id
    }

    /// Adds a targeted event described by a given [`EventDescriptor`].
    ///
    /// Like [`add_targeted_event`], an [`AddTargetedEvent`] event is sent if
    /// the event is newly added. If the [`TypeId`] of the event matches an
    /// existing targeted event, then the existing event's [`GlobalEventId`] is
    /// returned and no event is sent.
    ///
    /// # Safety
    ///
    /// - If the event is given a [`TypeId`], then the `layout` and `drop`
    ///   function must be compatible with the Rust type identified by the type
    ///   ID.
    /// - Drop function must be safe to call with a pointer to the event as
    ///   described by [`DropFn`]'s documentation.
    /// - The event's kind must be correct for the descriptor. See [`EventKind`]
    ///   for more information.
    ///
    /// [`add_targeted_event`]: World::add_targeted_event
    pub unsafe fn add_targeted_event_with_descriptor(
        &mut self,
        desc: EventDescriptor,
    ) -> TargetedEventId {
        let kind = desc.kind;

        let (id, is_new) = self.targeted_events.add(desc);

        if is_new {
            match kind {
                EventKind::Normal => {}
                EventKind::Insert { component_idx } => {
                    if let Some(info) = self.components.get_by_index_mut(component_idx) {
                        info.insert_events.insert(id);
                    }
                }
                EventKind::Remove { component_idx } => {
                    if let Some(info) = self.components.get_by_index_mut(component_idx) {
                        info.remove_events.insert(id);
                    }
                }
                EventKind::Spawn => {}
                EventKind::Despawn => {}
            }

            self.send(AddTargetedEvent(id))
        }

        id
    }

    /// Removes a global event from the world and returns its
    /// [`GlobalEventInfo`]. If the event ID is invalid, then `None` is
    /// returned and the function has no effect.
    ///
    /// Removing an event has the following additional effects in the order
    /// listed:
    /// 1. The [`RemoveTargetedEvent`] event is sent.
    /// 2. All handlers that send or receive the event are removed.
    ///
    /// # Examples
    ///
    /// ```
    /// use evenio::prelude::*;
    ///
    /// #[derive(GlobalEvent)]
    /// struct MyEvent;
    ///
    /// let mut world = World::new();
    ///
    /// let id = world.add_global_event::<MyEvent>();
    /// world.remove_global_event(id);
    ///
    /// assert!(!world.global_events().contains(id));
    /// ```
    pub fn remove_global_event(&mut self, event: GlobalEventId) -> Option<GlobalEventInfo> {
        assert!(self.event_queue.is_empty());

        if !self.global_events.contains(event) {
            return None;
        }

        // Send event before removing anything.
        self.send(RemoveGlobalEvent(event));

        // Remove all handlers that send or receive this event.
        let mut to_remove = vec![];

        for handler in self.handlers.iter() {
            if handler.received_event() == event.into()
                || handler.sent_global_events_bitset().contains(event.index())
            {
                to_remove.push(handler.id());
            }
        }

        for id in to_remove {
            self.remove_handler(id);
        }

        Some(self.global_events.remove(event).unwrap())
    }

    /// Removes a targeted event from the world and returns its
    /// [`TargetedEventInfo`]. If the event ID is invalid, then `None` is
    /// returned and the function has no effect.
    ///
    /// Removing an event has the following additional effects in the order
    /// listed:
    /// 1. The [`RemoveTargetedEvent`] event is sent.
    /// 2. All handlers that send or receive the event are removed.
    ///
    /// # Examples
    ///
    /// ```
    /// use evenio::prelude::*;
    ///
    /// #[derive(TargetedEvent)]
    /// struct MyEvent;
    ///
    /// let mut world = World::new();
    ///
    /// let id = world.add_targeted_event::<MyEvent>();
    /// world.remove_targeted_event(id);
    ///
    /// assert!(!world.targeted_events().contains(id));
    /// ```
    pub fn remove_targeted_event(&mut self, event: TargetedEventId) -> Option<TargetedEventInfo> {
        assert!(self.event_queue.is_empty());

        if !self.targeted_events.contains(event) {
            return None;
        }

        // Send event before doing anything else.
        self.send(RemoveTargetedEvent(event));

        // Remove all handlers that send or receive this event.
        let mut to_remove = vec![];

        for handler in self.handlers.iter() {
            if handler.received_event() == event.into()
                || handler
                    .sent_targeted_events_bitset()
                    .contains(event.index())
            {
                to_remove.push(handler.id());
            }
        }

        for id in to_remove {
            self.remove_handler(id);
        }

        let info = self.targeted_events.remove(event).unwrap();

        match info.kind() {
            EventKind::Normal => {}
            EventKind::Insert { component_idx } => {
                if let Some(info) = self.components.get_by_index_mut(component_idx) {
                    info.insert_events.remove(&event);
                }
            }
            EventKind::Remove { component_idx } => {
                if let Some(info) = self.components.get_by_index_mut(component_idx) {
                    info.remove_events.remove(&event);
                }
            }
            EventKind::Spawn => {}
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

    /// Returns the [`GlobalEvents`] for this world.
    pub fn global_events(&self) -> &GlobalEvents {
        &self.global_events
    }

    /// Returns the [`TargetedEvents`] for this world.
    pub fn targeted_events(&self) -> &TargetedEvents {
        &self.targeted_events
    }

    /// Send all queued events to handlers. The event queue will be empty after
    /// this call.
    fn flush_event_queue(&mut self) {
        'next_event: while let Some(item) = self.event_queue.pop() {
            struct EventDropper<'a> {
                event: NonNull<u8>,
                drop: DropFn,
                ownership_flag: bool,
                world: &'a mut World,
            }

            impl<'a> EventDropper<'a> {
                fn new(event: NonNull<u8>, drop: DropFn, world: &'a mut World) -> Self {
                    Self {
                        event,
                        drop,
                        ownership_flag: false,
                        world,
                    }
                }

                /// Extracts the event pointer and drop fn without running
                /// the destructor.
                #[inline]
                fn unpack(self) -> (NonNull<u8>, DropFn) {
                    let event = self.event;
                    let drop = self.drop;
                    mem::forget(self);

                    (event, drop)
                }

                /// Drops the held event.
                #[inline]
                unsafe fn drop_event(self) {
                    if let (event, Some(drop)) = self.unpack() {
                        drop(event);
                    }
                }
            }

            impl Drop for EventDropper<'_> {
                #[cold]
                fn drop(&mut self) {
                    // In case `Handler::run` unwinds, we need to drop the event we're holding on
                    // the stack as well as the events still the in the event queue.

                    #[cfg(feature = "std")]
                    debug_assert!(std::thread::panicking());

                    // Drop the held event.
                    if !self.ownership_flag {
                        if let Some(drop) = self.drop {
                            unsafe { drop(self.event) };
                        }
                    }

                    // Drop all events remaining in the event queue.
                    // This must be done here instead of the World's destructor because events
                    // could contain borrowed data.
                    for item in self.world.event_queue.iter() {
                        let drop = match item.meta {
                            EventMeta::Global { idx } => unsafe {
                                self.world
                                    .global_events
                                    .get_by_index(idx)
                                    .unwrap_unchecked()
                                    .drop()
                            },
                            EventMeta::Targeted { idx, .. } => unsafe {
                                self.world
                                    .targeted_events
                                    .get_by_index(idx)
                                    .unwrap_unchecked()
                                    .drop()
                            },
                        };

                        if let Some(drop) = drop {
                            unsafe { drop(item.event) };
                        }
                    }

                    self.world.event_queue.clear();
                }
            }

            let (mut ctx, event_kind, handlers, target_location) = match item.meta {
                EventMeta::Global { idx } => {
                    let info = unsafe { self.global_events.get_by_index(idx).unwrap_unchecked() };
                    let kind = info.kind();
                    let handlers: *const [_] =
                        unsafe { self.handlers.get_global_list(idx).unwrap_unchecked() }.slice();
                    let ctx = EventDropper::new(item.event, info.drop(), self);

                    let location = EntityLocation::NULL;

                    (ctx, kind, handlers, location)
                }
                EventMeta::Targeted { idx, target } => {
                    let info = unsafe { self.targeted_events.get_by_index(idx).unwrap_unchecked() };
                    let kind = info.kind();
                    let ctx = EventDropper::new(item.event, info.drop(), self);

                    let Some(location) = ctx.world.entities.get(target) else {
                        // Entity doesn't exist. Skip the event.
                        unsafe { ctx.drop_event() };
                        continue;
                    };

                    let arch = unsafe {
                        ctx.world
                            .archetypes
                            .get(location.archetype)
                            .unwrap_unchecked()
                    };

                    static EMPTY: HandlerList = HandlerList::new();

                    let handlers: *const [_] = arch.handler_list_for(idx).unwrap_or(&EMPTY).slice();

                    (ctx, kind, handlers, location)
                }
            };

            let events_before = ctx.world.event_queue.len();

            for mut info_ptr in unsafe { (*handlers).iter().copied() } {
                let info = unsafe { info_ptr.as_info_mut() };

                let handler: *mut dyn Handler = info.handler_mut();

                let event_ptr = EventPtr::new(ctx.event, NonNull::from(&mut ctx.ownership_flag));

                let world_cell = ctx.world.unsafe_cell_mut();

                unsafe { (*handler).run(info, event_ptr, target_location, world_cell) };

                // Did the handler take ownership of the event?
                if ctx.ownership_flag {
                    // Don't drop event since we don't own it anymore.
                    ctx.unpack();

                    // Reverse pushed events so they're handled in FIFO order.
                    unsafe {
                        self.event_queue
                            .get_unchecked_mut(events_before..)
                            .reverse()
                    };

                    continue 'next_event;
                }
            }

            // Reverse pushed events so they're handled in FIFO order.
            unsafe {
                ctx.world
                    .event_queue
                    .get_unchecked_mut(events_before..)
                    .reverse()
            };

            match event_kind {
                EventKind::Normal => {
                    // Ordinary event. Run drop fn.
                    unsafe { ctx.drop_event() };
                }
                EventKind::Insert { component_idx } => {
                    debug_assert_ne!(target_location, EntityLocation::NULL);

                    let dst = unsafe {
                        ctx.world.archetypes.traverse_insert(
                            target_location.archetype,
                            component_idx,
                            &mut ctx.world.components,
                            &mut ctx.world.handlers,
                        )
                    };

                    // `Insert<C>` is `repr(transparent)`.
                    let component_ptr = ctx.event.as_ptr().cast_const();

                    unsafe {
                        ctx.world.archetypes.move_entity(
                            target_location,
                            dst,
                            [(component_idx, component_ptr)],
                            &mut ctx.world.entities,
                        )
                    };

                    // Inserted component is owned by the archetype now. We wait to unpack
                    // in case one of the above functions panics.
                    ctx.unpack();
                }
                EventKind::Remove { component_idx } => {
                    // `Remove` doesn't need drop.
                    let _ = ctx.unpack();

                    let dst = unsafe {
                        self.archetypes.traverse_remove(
                            target_location.archetype,
                            component_idx,
                            &mut self.components,
                            &mut self.handlers,
                        )
                    };

                    unsafe {
                        self.archetypes
                            .move_entity(target_location, dst, [], &mut self.entities)
                    };
                }
                EventKind::Spawn => {
                    // `Spawn` doesn't need drop.
                    let _ = ctx.unpack();

                    // Spawn all entities from the reserved entity queue.
                    self.reserved_entities
                        .spawn_all(&mut self.entities, |id| self.archetypes.spawn(id));
                }
                EventKind::Despawn => {
                    // `Despawn` doesn't need drop.
                    let _ = ctx.unpack();

                    unsafe {
                        self.archetypes
                            .remove_entity(target_location, &mut self.entities)
                    };

                    // Reset next key iter.
                    self.reserved_entities.refresh(&self.entities);
                }
            }
        }

        self.bump.reset();
        debug_assert!(self.event_queue.is_empty());
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

/// Reference to a [`World`] where all methods take `&self` and aliasing rules
/// are not checked. It is the caller's responsibility to ensure that Rust's
/// aliasing rules are not violated.
#[derive(Clone, Copy, Debug)]
pub struct UnsafeWorldCell<'a> {
    world: NonNull<World>,
    _marker: PhantomData<&'a World>,
}

impl<'a> UnsafeWorldCell<'a> {
    /// Allocate data in the world's bump allocator.
    ///
    /// This operation is not thread safe.
    ///
    /// # Safety
    ///
    /// - Must be called from within a handler.
    #[inline]
    pub unsafe fn alloc_layout(self, layout: Layout) -> NonNull<u8> {
        let bump = unsafe { &(*self.world.as_ptr()).bump };
        bump.alloc_layout(layout)
    }

    /// Add a global event to the event queue. Ownership of the event is
    /// transferred.
    ///
    /// # Safety
    ///
    /// - Must be called from within a handler.
    /// - Event must outlive call to top level [`World::send`] or
    ///   [`World::send_to`].
    /// - Event index must be correct for the given event.
    #[inline]
    pub unsafe fn queue_global(self, event: NonNull<u8>, idx: GlobalEventIdx) {
        let event_queue = &mut (*self.world.as_ptr()).event_queue;

        event_queue.push(EventQueueItem {
            meta: EventMeta::Global { idx },
            event,
        });
    }

    /// Add a targeted event to the event queue. Ownership of the event is
    /// transferred.
    ///
    /// # Safety
    ///
    /// - Must be called from within a handler.
    /// - Must have permission to access the event queue
    #[inline]
    pub unsafe fn queue_targeted(
        self,
        target: EntityId,
        event: NonNull<u8>,
        idx: TargetedEventIdx,
    ) {
        let event_queue = &mut (*self.world.as_ptr()).event_queue;

        event_queue.push(EventQueueItem {
            meta: EventMeta::Targeted { idx, target },
            event,
        });
    }

    /// # Safety
    ///
    /// - Must be called from within a handler.
    pub unsafe fn queue_spawn(self) -> EntityId {
        let entity_id = (*self.world.as_ptr())
            .reserved_entities
            .reserve(self.entities());

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

    /// Returns the [`GlobalEvents`] for this world.
    pub fn global_events(self) -> &'a GlobalEvents {
        unsafe { &(*self.world.as_ptr()).global_events }
    }

    /// Returns the [`TargetedEvents`] for this world.
    pub fn targeted_events(self) -> &'a TargetedEvents {
        unsafe { &(*self.world.as_ptr()).targeted_events }
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

#[cfg(test)]
mod tests {
    use alloc::rc::Rc;
    use core::panic::AssertUnwindSafe;
    use std::panic;

    use crate::prelude::*;

    #[test]
    fn world_drops_events() {
        #[derive(GlobalEvent)]
        struct A(Rc<()>);

        #[derive(GlobalEvent)]
        struct B(Rc<()>);

        #[derive(GlobalEvent)]
        struct C(Rc<()>);

        let mut world = World::new();

        world.add_handler(|r: Receiver<A>, s: Sender<B>| {
            s.send(B(r.event.0.clone()));
            s.send(B(r.event.0.clone()));
        });

        world.add_handler(|r: Receiver<B>, s: Sender<C>| {
            s.send(C(r.event.0.clone()));
            s.send(C(r.event.0.clone()));
        });

        world.add_handler(|r: Receiver<C>| println!("got C {:?}", Rc::as_ptr(&r.event.0)));

        let rc = Rc::new(());

        world.send(A(rc.clone()));

        drop(world);

        assert_eq!(Rc::strong_count(&rc), 1);
    }

    #[test]
    fn world_drops_events_on_panic() {
        #[derive(GlobalEvent)]
        struct A(Rc<()>);

        impl Drop for A {
            fn drop(&mut self) {
                eprintln!("calling A destructor");
            }
        }

        #[derive(GlobalEvent)]
        struct B(Rc<()>);

        #[allow(dead_code)]
        #[derive(GlobalEvent)]
        struct C(Rc<()>);

        let mut world = World::new();

        world.add_handler(|r: Receiver<A>, s: Sender<B>| {
            s.send(B(r.event.0.clone()));
            s.send(B(r.event.0.clone()));
        });

        world.add_handler(|r: Receiver<B>, s: Sender<C>| {
            s.send(C(r.event.0.clone()));
            s.send(C(r.event.0.clone()));
        });

        world.add_handler(|_: Receiver<C>| panic!("oops!"));

        let arc = Rc::new(());
        let arc_cloned = arc.clone();

        let mut world = AssertUnwindSafe(world);

        let res = panic::catch_unwind(move || world.send(A(arc_cloned)));

        assert_eq!(*res.unwrap_err().downcast::<&str>().unwrap(), "oops!");

        assert_eq!(Rc::strong_count(&arc), 1);
    }

    #[test]
    fn bump_allocator_is_reset() {
        let mut world = World::new();

        #[derive(GlobalEvent)]
        struct Data(#[allow(dead_code)] u64);

        world.send(Data(123));

        let ptr1 = world.bump.alloc(1u8) as *const u8;
        world.bump.reset();
        let ptr2 = world.bump.alloc(1u8) as *const u8;

        assert_eq!(ptr1, ptr2);
    }
}
