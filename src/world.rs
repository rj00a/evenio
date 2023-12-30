use core::cell::UnsafeCell;
use core::marker::PhantomData;
use core::panic::{RefUnwindSafe, UnwindSafe};
use std::alloc::Layout;
use std::any::{self, TypeId};
use std::mem;
use std::ptr::{self, NonNull};

use crate::archetype::{ArchetypeIdx, Archetypes};
use crate::component::{Component, ComponentDescriptor, ComponentId, ComponentInfo, Components};
use crate::debug_checked::UnwrapDebugChecked;
use crate::entity::{Entities, EntityId, EntityLocation, ReservedEntities};
use crate::event::{
    AddComponent, AddEvent, AddSystem, Event, EventDescriptor, EventId, EventIdx, EventInfo,
    EventKind, EventPtr, EventQueue, Events, Spawn,
};
use crate::system::{
    Config, IntoSystem, System, SystemId, SystemInfo, SystemInfoInner, SystemList, Systems,
};
use crate::DropFn;

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
        let event_id = self.add_event::<E>();

        unsafe { self.event_queue.push(event, event_id.index()) };

        self.flush_event_queue();
    }

    pub fn spawn(&mut self) -> EntityId {
        let id = self.reserved_entities.reserve(&self.entities);
        self.send(Spawn(id));
        id
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
            entity_event_expr: config.entity_event_expr,
            sent_global_events: config.sent_global_events,
            sent_entity_events: config.sent_entity_events,
            event_queue_access: config.event_queue_access,
            reserve_entity_access: config.reserve_entity_access,
            component_access: config.component_access,
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

    pub fn remove_system(&mut self, id: SystemId) -> Option<SystemInfo> {
        todo!()
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
            drop: mem::needs_drop::<C>()
                .then_some(|ptr| unsafe { ptr::drop_in_place(ptr.as_ptr().cast::<C>()) }),
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

    pub fn remove_component(&mut self, id: SystemId) -> Option<ComponentInfo> {
        todo!()
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
            target_offset: E::TARGET_OFFSET,
            kind: E::init(self),
            layout: Layout::new::<E>(),
            drop: mem::needs_drop::<E>()
                .then_some(|ptr| unsafe { ptr::drop_in_place(ptr.as_ptr().cast::<E>()) }),
        };

        unsafe { self.add_event_with_descriptor(desc) }
    }

    pub unsafe fn add_event_with_descriptor(&mut self, desc: EventDescriptor) -> EventId {
        let (id, is_new) = self.events.add(desc);

        if is_new {
            self.systems.register_event(id.index());

            self.send(AddEvent(id));
        }

        id
    }

    pub fn remove_event(&mut self, id: EventId) -> Option<EventInfo> {
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

    /// Send all queued events to systems. The event queue will be empty after
    /// this call.
    ///
    /// Note that methods like [`send`] will automatically flush the event
    /// queue, so this doesn't ususally neeed to be called directly.
    ///
    /// [`send`]: Self::send
    pub fn flush_event_queue(&mut self) {
        handle_events(0, self);
        debug_assert_eq!(self.event_queue.len(), 0);

        fn handle_events(queue_start_idx: usize, world: &mut World) {
            debug_assert!(queue_start_idx < world.event_queue.len());

            'next_event: for queue_idx in queue_start_idx..world.event_queue.len() {
                let item = unsafe { world.event_queue.get_debug_checked_mut(queue_idx) };
                let event_idx = item.event_idx;
                let event_info =
                    unsafe { world.events.get_by_index(event_idx).unwrap_debug_checked() };
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
                        let Self { event, drop } = self;
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

                let system_list = match event_idx {
                    EventIdx::Global(idx) => unsafe {
                        world.systems.get_global_list(idx).unwrap_debug_checked()
                    },
                    EventIdx::Entity(idx) => {
                        // Extract target from event.

                        // SAFETY: event is an entity event, so it must have a target offset.
                        let target_offset =
                            unsafe { event_info.target_offset().unwrap_debug_checked() };

                        let entity_id =
                            unsafe { event.event.add(target_offset).cast::<EntityId>().read() };

                        let Some(location) = world.entities.get(entity_id) else {
                            continue;
                        };

                        let arch = unsafe {
                            world
                                .archetypes
                                .get(location.archetype)
                                .unwrap_debug_checked()
                        };

                        static EMPTY: SystemList = SystemList::new();

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
                    EventKind::Other => {}
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

                            let component_ptr = unsafe { event.event.add(component_offset) };

                            unsafe {
                                world.archetypes.move_entity(
                                    loc,
                                    dst,
                                    [(component_idx, component_ptr)],
                                    &mut world.entities,
                                )
                            };

                            // Inserted component is owned by the archetype now.
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
                    EventKind::Spawn => {
                        let empty = world.archetypes.empty_mut();

                        // Flush reserved entities and add them to the empty archetype.
                        world.reserved_entities.flush(&mut world.entities, |id| {
                            let (row, _) = unsafe { empty.add_entity(id) };

                            EntityLocation {
                                archetype: ArchetypeIdx::EMPTY,
                                row,
                            }
                        });
                    }
                    EventKind::Despawn => {
                        // TODO: flush reserved entities _after_ removing the
                        // entity?
                    }
                }
            }

            debug_assert!(queue_start_idx < world.event_queue.len());

            unsafe { world.event_queue.set_len(queue_start_idx) };
        }
    }

    /// Returns a new [`UnsafeWorldCell`] with permission to read all data in
    /// this world.
    pub fn unsafe_cell(&self) -> UnsafeWorldCell {
        UnsafeWorldCell {
            world: (self as *const World).cast_mut(),
            _marker: PhantomData,
        }
    }

    /// Returns a new [`UnsafeWorldCell`] with permission to read and write all
    /// data in this world.
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
                        .get_by_index(item.event_idx)
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

#[derive(Clone, Copy, Debug)]
pub struct UnsafeWorldCell<'a> {
    world: *mut World,
    _marker: PhantomData<(&'a World, &'a UnsafeCell<World>)>,
}

impl<'a> UnsafeWorldCell<'a> {
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
}
