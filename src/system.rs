//! Event handlers

use alloc::borrow::Cow;
use alloc::boxed::Box;
use alloc::collections::BTreeMap;
use alloc::vec;
use alloc::vec::Vec;
use core::any::TypeId;
use core::marker::PhantomData;
use core::ops::{Deref, DerefMut, Index};
use core::panic::{RefUnwindSafe, UnwindSafe};
use core::ptr::NonNull;
use core::{any, fmt};

use evenio_macros::all_tuples;
pub use evenio_macros::SystemParam;

use crate::access::{Access, ComponentAccessExpr};
use crate::archetype::Archetype;
use crate::assert::UnwrapDebugChecked;
use crate::bit_set::BitSet;
use crate::bool_expr::BoolExpr;
use crate::component::ComponentIdx;
use crate::entity::EntityLocation;
use crate::event::{Event, EventId, EventIdx, EventPtr, TargetedEventIdx, UntargetedEventIdx};
use crate::exclusive::Exclusive;
use crate::map::TypeIdMap;
use crate::slot_map::{Key, SlotMap};
use crate::sparse::SparseIndex;
use crate::world::{UnsafeWorldCell, World};

/// Contains metadata for all the systems in a world.
///
/// This can be obtained in a system by using the `&Systems` system
/// parameter.
///
/// ```
/// # use evenio::prelude::*;
/// # use evenio::system::Systems;
/// #
/// # #[derive(Event)] struct E;
/// #
/// # let mut world = World::new();
/// world.add_system(|_: Receiver<E>, systems: &Systems| {});
/// ```
#[derive(Debug)]
pub struct Systems {
    infos: SlotMap<SystemInfo>,
    /// Maps untargeted event indices to the ordered list of systems that handle
    /// the event.
    by_untargeted_event: Vec<SystemList>,
    by_type_id: TypeIdMap<SystemInfoPtr>,
    /// Counts up as new systems are added.
    insert_counter: u64,
    /// Systems ordered by the order they were added to the world. This ensures
    /// that iteration over all systems is done in insertion order.
    by_insert_order: BTreeMap<u64, SystemInfoPtr>,
}

impl Systems {
    pub(crate) fn new() -> Self {
        Self {
            infos: SlotMap::new(),
            by_untargeted_event: vec![],
            by_type_id: Default::default(),
            insert_counter: 0,
            by_insert_order: BTreeMap::new(),
        }
    }

    pub(crate) fn add(&mut self, info: SystemInfo) -> SystemId {
        let mut ptr = info.ptr();

        if let Some(type_id) = info.type_id() {
            assert!(self.by_type_id.insert(type_id, ptr).is_none());
        }

        let Some(k) = self.infos.insert_with(|k| {
            let id = SystemId(k);

            let inner = unsafe { ptr.as_mut() };

            inner.id = id;
            inner.order = self.insert_counter;

            if let EventIdx::Untargeted(idx) = info.received_event().index() {
                let idx = idx.0 as usize;

                if idx >= self.by_untargeted_event.len() {
                    self.by_untargeted_event
                        .resize_with(idx + 1, SystemList::default);
                }

                self.by_untargeted_event[idx].insert(ptr, info.priority())
            }

            info
        }) else {
            panic!("too many systems")
        };

        self.by_insert_order.insert(self.insert_counter, ptr);
        self.insert_counter += 1;

        debug_assert_eq!(self.infos.len(), self.by_insert_order.len() as u32);

        SystemId(k)
    }

    pub(crate) fn remove(&mut self, id: SystemId) -> Option<SystemInfo> {
        let info = self.infos.remove(id.0)?;

        let received_event = info.received_event();

        if received_event.is_untargeted() {
            let list = &mut self.by_untargeted_event[received_event.index().as_u32() as usize];
            list.remove(info.ptr());
        }

        if let Some(type_id) = info.type_id() {
            self.by_type_id.remove(&type_id);
        }

        self.by_insert_order.remove(&info.order());

        debug_assert_eq!(self.infos.len(), self.by_insert_order.len() as u32);

        Some(info)
    }

    pub(crate) fn register_event(&mut self, event_idx: EventIdx) {
        if let EventIdx::Untargeted(UntargetedEventIdx(idx)) = event_idx {
            if idx as usize >= self.by_untargeted_event.len() {
                self.by_untargeted_event
                    .resize_with(idx as usize + 1, SystemList::default);
            }
        }
    }

    pub(crate) fn get_untargeted_list(&self, idx: UntargetedEventIdx) -> Option<&SystemList> {
        self.by_untargeted_event.get(idx.0 as usize)
    }

    /// Gets the [`SystemInfo`] of the given system. Returns `None` if the ID is
    /// invalid.
    pub fn get(&self, id: SystemId) -> Option<&SystemInfo> {
        self.infos.get(id.0)
    }

    pub(crate) fn get_mut(&mut self, id: SystemId) -> Option<&mut SystemInfo> {
        self.infos.get_mut(id.0)
    }

    /// Gets the [`SystemInfo`] for a system using its [`SystemIdx`].
    /// Returns `None` if the index is invalid.
    pub fn get_by_index(&self, idx: SystemIdx) -> Option<&SystemInfo> {
        self.infos.get_by_index(idx.0).map(|(_, v)| v)
    }

    /// Gets the [`SystemInfo`] for a system using its [`TypeId`]. Returns
    /// `None` if the `TypeId` does not map to a system.
    pub fn get_by_type_id(&self, id: TypeId) -> Option<&SystemInfo> {
        self.by_type_id
            .get(&id)
            .map(|p| unsafe { SystemInfo::from_ptr(p) })
    }

    /// Does the given system exist in the world?
    pub fn contains(&self, id: SystemId) -> bool {
        self.get(id).is_some()
    }

    /// Returns an iterator over all system infos in insertion order.
    pub fn iter(&self) -> impl Iterator<Item = &SystemInfo> {
        self.by_insert_order
            .values()
            .map(|ptr| unsafe { SystemInfo::from_ptr(ptr) })
    }

    /// Returns a mutable iterator over all system infos in insertion order.
    pub(crate) fn iter_mut(&mut self) -> impl Iterator<Item = &mut SystemInfo> {
        self.by_insert_order
            .values_mut()
            .map(|ptr| unsafe { SystemInfo::from_ptr_mut(ptr) })
    }
}

unsafe impl Send for Systems {}
unsafe impl Sync for Systems {}

impl Index<SystemId> for Systems {
    type Output = SystemInfo;

    fn index(&self, index: SystemId) -> &Self::Output {
        if let Some(info) = self.get(index) {
            info
        } else {
            panic!("no such system with ID of {index:?} exists")
        }
    }
}

impl Index<SystemIdx> for Systems {
    type Output = SystemInfo;

    fn index(&self, index: SystemIdx) -> &Self::Output {
        if let Some(info) = self.get_by_index(index) {
            info
        } else {
            panic!("no such system with index of {index:?} exists")
        }
    }
}

impl Index<TypeId> for Systems {
    type Output = SystemInfo;

    fn index(&self, index: TypeId) -> &Self::Output {
        if let Some(info) = self.get_by_type_id(index) {
            info
        } else {
            panic!("no such system with type ID of {index:?} exists")
        }
    }
}

unsafe impl SystemParam for &'_ Systems {
    type State = ();

    type Item<'a> = &'a Systems;

    fn init(_world: &mut World, _config: &mut Config) -> Result<Self::State, InitError> {
        Ok(())
    }

    unsafe fn get<'a>(
        _state: &'a mut Self::State,
        _info: &'a SystemInfo,
        _event_ptr: EventPtr<'a>,
        _target_location: EntityLocation,
        world: UnsafeWorldCell<'a>,
    ) -> Self::Item<'a> {
        world.systems()
    }

    fn refresh_archetype(_state: &mut Self::State, _arch: &Archetype) {}

    fn remove_archetype(_state: &mut Self::State, _arch: &Archetype) {}
}

/// Metadata for a system.
#[repr(transparent)]
pub struct SystemInfo {
    /// Pointer to heap allocated data that's shared between archetypes and the
    /// system index. `SystemInfo` is responsible for dropping and freeing the
    /// memory, like `Box`.
    ///
    /// This is a raw pointer to get around the current aliasing restrictions
    /// of `Box`.
    /// > The aliasing rules for `Box<T>` are the same as for `&mut T`. `Box<T>`
    /// > asserts uniqueness over its content. Using raw pointers derived from a
    /// > box after that box has been mutated through, moved or borrowed as
    /// > `&mut T` is not allowed. For more guidance on working with box from
    /// > unsafe code, see rust-lang/unsafe-code-guidelines#326.
    inner: SystemInfoPtr,
}

pub(crate) type SystemInfoPtr = NonNull<SystemInfoInner>;

// This is generic over `S` so that we can do an unsizing coercion.
pub(crate) struct SystemInfoInner<S: ?Sized = dyn System> {
    pub(crate) name: Cow<'static, str>,
    pub(crate) id: SystemId,
    pub(crate) type_id: Option<TypeId>,
    pub(crate) order: u64,
    pub(crate) received_event: EventId,
    pub(crate) received_event_access: Access,
    pub(crate) targeted_event_expr: BoolExpr<ComponentIdx>,
    pub(crate) sent_untargeted_events: BitSet<UntargetedEventIdx>,
    pub(crate) sent_targeted_events: BitSet<TargetedEventIdx>,
    pub(crate) event_queue_access: Access,
    pub(crate) component_access: ComponentAccessExpr,
    pub(crate) referenced_components: BitSet<ComponentIdx>,
    pub(crate) priority: Priority,
    // SAFETY: There is intentionally no public accessor for this field as it would lead to mutable
    // aliasing.
    pub(crate) system: S,
}

impl SystemInfo {
    pub(crate) fn new<S: System>(inner: SystemInfoInner<S>) -> Self {
        // SAFETY: `Box::into_raw` guarantees the returned pointer is non-null.
        let ptr = unsafe { NonNull::new(Box::into_raw(Box::new(inner))).unwrap_debug_checked() };

        Self { inner: ptr }
    }

    /// Gets the name of the system.
    ///
    /// This name is intended for debugging purposes and should not be relied
    /// upon for correctness.
    pub fn name(&self) -> &str {
        unsafe { &(*self.inner.as_ptr()).name }
    }

    /// Gets the ID of this system.
    pub fn id(&self) -> SystemId {
        unsafe { (*self.inner.as_ptr()).id }
    }

    /// Gets the [`TypeId`] of this system, if any.
    pub fn type_id(&self) -> Option<TypeId> {
        unsafe { (*self.inner.as_ptr()).type_id }
    }

    pub(crate) fn order(&self) -> u64 {
        unsafe { (*self.inner.as_ptr()).order }
    }

    /// Gets the [`EventId`] of the event is system listens for.
    pub fn received_event(&self) -> EventId {
        unsafe { (*self.inner.as_ptr()).received_event }
    }

    /// Gets the system's [`Access`] to the event it listens for.
    pub fn received_event_access(&self) -> Access {
        unsafe { (*self.inner.as_ptr()).received_event_access }
    }

    /// Gets the expression describing the system's targeted event query, or
    /// `None` if this system is not targeted.
    pub fn targeted_event_expr(&self) -> Option<&BoolExpr<ComponentIdx>> {
        self.received_event()
            .is_targeted()
            .then(|| unsafe { &(*self.inner.as_ptr()).targeted_event_expr })
    }

    /// Returns the set of untargeted events this system sends.
    pub fn sent_untargeted_events(&self) -> &BitSet<UntargetedEventIdx> {
        unsafe { &(*self.inner.as_ptr()).sent_untargeted_events }
    }

    /// Returns the set of targeted events this system sends.
    pub fn sent_targeted_events(&self) -> &BitSet<TargetedEventIdx> {
        unsafe { &(*self.inner.as_ptr()).sent_targeted_events }
    }

    /// Gets this system's [`Access`] to the event queue.
    pub fn event_queue_access(&self) -> Access {
        unsafe { (*self.inner.as_ptr()).event_queue_access }
    }

    /// Gets the expression describing this system's access
    pub fn component_access(&self) -> &ComponentAccessExpr {
        unsafe { &(*self.inner.as_ptr()).component_access }
    }

    /// Gets the set of components referenced by this system.
    ///
    /// Referenced components are components used by the system in any way. Used
    /// for cleanup when removing components.
    pub fn referenced_components(&self) -> &BitSet<ComponentIdx> {
        unsafe { &(*self.inner.as_ptr()).referenced_components }
    }

    /// Gets the [`Priority`] of this system.
    pub fn priority(&self) -> Priority {
        unsafe { (*self.inner.as_ptr()).priority }
    }

    pub(crate) fn ptr(&self) -> SystemInfoPtr {
        self.inner
    }

    /// # Safety
    ///
    /// - Pointer must be valid.
    /// - Aliasing rules must be followed.
    pub(crate) unsafe fn from_ptr(this: &SystemInfoPtr) -> &Self {
        // SAFETY: `SystemInfo` is `#[repr(transparent)]`.
        &*(this as *const _ as *const Self)
    }

    /// # Safety
    ///
    /// - Pointer must be valid.
    /// - Aliasing rules must be followed.
    pub(crate) unsafe fn from_ptr_mut(this: &mut SystemInfoPtr) -> &mut Self {
        // SAFETY: `SystemInfo` is `#[repr(transparent)]`.
        &mut *(this as *mut _ as *mut Self)
    }

    pub(crate) fn system_mut(&mut self) -> &mut dyn System {
        unsafe { &mut (*self.inner.as_ptr()).system }
    }
}

impl<S: ?Sized> UnwindSafe for SystemInfoInner<S> {}
impl<S: ?Sized> RefUnwindSafe for SystemInfoInner<S> {}

unsafe impl Send for SystemInfo {}
unsafe impl Sync for SystemInfo {}

impl fmt::Debug for SystemInfo {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("SystemInfo")
            .field("name", &self.name())
            .field("received_event", &self.received_event())
            .field("received_event_access", &self.received_event_access())
            .field("targeted_event_expr", &self.targeted_event_expr())
            .field("sent_untargeted_events", &self.sent_untargeted_events())
            .field("sent_targeted_events", &self.sent_targeted_events())
            .field("event_queue_access", &self.event_queue_access())
            .field("priority", &self.priority())
            .field("id", &self.id())
            .field("type_id", &self.type_id())
            // Don't access the `system` field.
            .finish_non_exhaustive()
    }
}

impl Drop for SystemInfo {
    fn drop(&mut self) {
        // SAFETY: The inner data was derived from a `Box` and is owned by this system
        // info.
        let _ = unsafe { Box::from_raw(self.inner.as_ptr()) };
    }
}

#[derive(Debug, Default)]
pub(crate) struct SystemList {
    before: u32,
    after: u32,
    entries: Vec<SystemInfoPtr>,
}

unsafe impl Sync for SystemList {}

impl SystemList {
    pub(crate) const fn new() -> SystemList {
        Self {
            before: 0,
            after: 0,
            entries: vec![],
        }
    }

    pub(crate) fn insert(&mut self, ptr: SystemInfoPtr, priority: Priority) {
        assert!(self.entries.len() < u32::MAX as usize);

        match priority {
            Priority::Before => {
                self.entries.insert(self.before as usize, ptr);
                self.before += 1;
                self.after += 1;
            }
            Priority::Normal => {
                self.entries.insert(self.after as usize, ptr);
                self.after += 1;
            }
            Priority::After => {
                self.entries.push(ptr);
            }
        }
    }

    pub(crate) fn remove(&mut self, ptr: SystemInfoPtr) -> bool {
        if let Some(idx) = self.entries.iter().position(|&p| p == ptr) {
            self.entries.remove(idx);

            let idx = idx as u32;

            if idx < self.after {
                self.after -= 1;

                if idx < self.before {
                    self.before -= 1;
                }
            }

            true
        } else {
            false
        }
    }

    pub(crate) fn systems(&self) -> &[SystemInfoPtr] {
        &self.entries
    }
}

/// Lightweight identifier for a system.
///
/// System identifiers are implemented using an [index] and a generation count.
/// The generation count ensures that IDs from removed systems are not reused
/// by new systems.
///
/// A system identifier is only meaningful in the [`World`] it was created
/// from. Attempting to use a system ID in a different world will have
/// unexpected results.
///
/// [index]: SystemIdx
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Default, Debug)]
pub struct SystemId(Key);

impl SystemId {
    /// The system ID which never identifies a live system. This is the default
    /// value for `SystemId`.
    pub const NULL: Self = Self(Key::NULL);

    /// Returns the index of this ID.
    pub const fn index(self) -> SystemIdx {
        SystemIdx(self.0.index())
    }

    /// Returns the generation count of this ID.
    pub const fn generation(self) -> u32 {
        self.0.generation().get()
    }
}

/// A [`SystemId`] with the generation count stripped out.
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
pub struct SystemIdx(pub u32);

unsafe impl SparseIndex for SystemIdx {
    const MAX: Self = Self(u32::MAX);

    fn index(self) -> usize {
        self.0.index()
    }

    fn from_index(idx: usize) -> Self {
        Self(u32::from_index(idx))
    }
}

/// Types which can be converted into [`System`]s.
///
/// This trait is implemented for all functions that return `()` and whose
/// arguments are all [`SystemParam`]s.
pub trait IntoSystem<Marker>: Sized {
    /// The system type to convert to.
    type System: System;

    /// Performs the conversion into a [`System`].
    fn into_system(self) -> Self::System;

    /// Ignore this system's reported [`TypeId`]. This can be used to add a
    /// specific system to the world more than once.
    ///
    /// # Examples
    ///
    /// ```
    /// use evenio::prelude::*;
    ///
    /// let mut world = World::new();
    ///
    /// let id_1 = world.add_system(my_system);
    /// let id_2 = world.add_system(my_system.no_type_id());
    /// let id_3 = world.add_system(my_system);
    ///
    /// assert_ne!(id_1, id_2);
    /// assert_eq!(id_1, id_3);
    /// #
    /// # fn my_system(_: Receiver<E>) {}
    /// #
    /// # #[derive(Event)]
    /// # struct E;
    /// ```
    fn no_type_id(self) -> NoTypeId<Self::System> {
        NoTypeId(self.into_system())
    }

    /// Returns a wrapper which sets the priority of this system to
    /// [`Priority::Before`].
    fn before(self) -> Before<Self::System> {
        Before(self.into_system())
    }

    /// Returns a wrapper which sets the priority of this system to
    /// [`Priority::After`].
    fn after(self) -> After<Self::System> {
        After(self.into_system())
    }
}

#[doc(hidden)]
#[derive(Debug)]
pub struct FunctionSystemMarker;

impl<Marker, F> IntoSystem<(FunctionSystemMarker, Marker)> for F
where
    Marker: 'static,
    F: SystemParamFunction<Marker>,
{
    type System = FunctionSystem<Marker, F>;

    fn into_system(self) -> Self::System {
        FunctionSystem::new(self)
    }
}

impl<S: System> IntoSystem<()> for S {
    type System = Self;

    fn into_system(self) -> Self::System {
        self
    }
}

/// The wrapper system returned by [`IntoSystem::no_type_id`].
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Default, Debug)]
pub struct NoTypeId<S>(pub S);

impl<S: System> System for NoTypeId<S> {
    fn type_id(&self) -> Option<TypeId> {
        None
    }

    fn name(&self) -> Cow<'static, str> {
        self.0.name()
    }

    fn init(&mut self, world: &mut World, config: &mut Config) -> Result<(), InitError> {
        self.0.init(world, config)
    }

    unsafe fn run(
        &mut self,
        info: &SystemInfo,
        event_ptr: EventPtr,
        target_location: EntityLocation,
        world: UnsafeWorldCell,
    ) {
        self.0.run(info, event_ptr, target_location, world)
    }

    fn refresh_archetype(&mut self, arch: &Archetype) {
        self.0.refresh_archetype(arch)
    }

    fn remove_archetype(&mut self, arch: &Archetype) {
        self.0.remove_archetype(arch)
    }
}

/// The wrapper system returned by [`IntoSystem::before`].
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Default, Debug)]
pub struct Before<S>(pub S);

impl<S: System> System for Before<S> {
    fn type_id(&self) -> Option<TypeId> {
        self.0.type_id()
    }

    fn name(&self) -> Cow<'static, str> {
        self.0.name()
    }

    fn init(&mut self, world: &mut World, config: &mut Config) -> Result<(), InitError> {
        let res = self.0.init(world, config);
        config.priority = Priority::Before;
        res
    }

    unsafe fn run(
        &mut self,
        info: &SystemInfo,
        event_ptr: EventPtr,
        target_location: EntityLocation,
        world: UnsafeWorldCell,
    ) {
        self.0.run(info, event_ptr, target_location, world)
    }

    fn refresh_archetype(&mut self, arch: &Archetype) {
        self.0.refresh_archetype(arch)
    }

    fn remove_archetype(&mut self, arch: &Archetype) {
        self.0.remove_archetype(arch)
    }
}

/// The wrapper system returned by [`IntoSystem::after`].
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Default, Debug)]
pub struct After<S>(pub S);

impl<S: System> System for After<S> {
    fn type_id(&self) -> Option<TypeId> {
        self.0.type_id()
    }

    fn name(&self) -> Cow<'static, str> {
        self.0.name()
    }

    fn init(&mut self, world: &mut World, config: &mut Config) -> Result<(), InitError> {
        let res = self.0.init(world, config);
        config.priority = Priority::After;
        res
    }

    unsafe fn run(
        &mut self,
        info: &SystemInfo,
        event_ptr: EventPtr,
        target_location: EntityLocation,
        world: UnsafeWorldCell,
    ) {
        self.0.run(info, event_ptr, target_location, world)
    }

    fn refresh_archetype(&mut self, arch: &Archetype) {
        self.0.refresh_archetype(arch)
    }

    fn remove_archetype(&mut self, arch: &Archetype) {
        self.0.remove_archetype(arch)
    }
}

/// An [`Event`] handler function that can be added to a [`World`].
///
/// Systems are added to a world using the [`World::add_system`] method.
///
/// For more information about systems, see the [tutorial](crate::tutorial).
pub trait System: Send + Sync + 'static {
    /// Returns the [`TypeId`] which uniquely identifies this system, or `None`
    /// if there is none.
    ///
    /// No two systems with the same [`TypeId`] will exist in the [`World`] at
    /// the same time.
    fn type_id(&self) -> Option<TypeId>;

    /// Returns the name of this system for debugging purposes.
    fn name(&self) -> Cow<'static, str>;

    /// Initializes the system. Returns [`InitError`] on failure.
    fn init(&mut self, world: &mut World, config: &mut Config) -> Result<(), InitError>;

    /// Execute the system by passing in the system's metadata, a pointer to the
    /// received event of the configured type, the entity location of the
    /// event's target, and an [`UnsafeWorldCell`] with permission to access
    /// the data described in the configuration.
    ///
    /// # Safety
    ///
    /// - System must be initialized via [`init`].
    /// - `info` must be the correct information for this system.
    /// - `event_ptr` must point to the correct type of event configured by this
    ///   system in [`init`].
    /// - `target_location` must be a valid location to an entity matching
    ///   [`Config::targeted_event_expr`], unless the event is not targeted.
    /// - `world` must have permission to access all data configured by this
    ///   system in [`init`].
    ///
    /// [`init`]: Self::init
    unsafe fn run(
        &mut self,
        info: &SystemInfo,
        event_ptr: EventPtr,
        target_location: EntityLocation,
        world: UnsafeWorldCell,
    );

    /// Notifies the system that an archetype it might care about had its
    /// internal state updated.
    ///
    /// This is invoked in the following scenarios:
    /// - The archetype was previously empty, but has now gained at least one
    ///   entity.
    /// - The archetype's columns were reallocated, and any pointers into the
    ///   archetype's columns need to be reacquired.
    ///
    /// This method must not be called with empty archetypes.
    fn refresh_archetype(&mut self, arch: &Archetype);

    /// Notifies the system that an archetype it might care about is no longer
    /// available.
    ///
    /// This is invoked in the following scenarios:
    /// - The archetype was removed from the world.
    /// - The archetype previously had entities in it, but is now empty.
    ///
    /// In either case, the system must assume that the archetype is no longer
    /// available. Attempting to read the component data from a removed
    /// archetype is illegal.
    fn remove_archetype(&mut self, arch: &Archetype);
}

/// An error returned when system initialization fails. Contains an error
/// message.
///
/// The error message is not stable.
#[derive(Clone, Debug)]
pub struct InitError(pub Box<str>);

impl fmt::Display for InitError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", &self.0)
    }
}

#[cfg(feature = "std")]
#[cfg_attr(docsrs, doc(cfg(feature = "std")))]
impl std::error::Error for InitError {}

/// The priority of a system relative to other systems that handle the same
/// event.
///
/// If multiple systems have the same priority, then the order they were added
/// to the [`World`] is used as a fallback.
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Default, Debug)]
pub enum Priority {
    /// The system runs before other systems.
    Before,
    /// The default system priority.
    #[default]
    Normal,
    /// The system runs after other systems.
    After,
}

/// The Configuration of a system. Accessible during system initialization.
#[derive(Clone, Debug)]
#[non_exhaustive]
pub struct Config {
    /// The priority of this system.
    pub priority: Priority,
    /// The event type to be received by the system.
    ///
    /// Defaults to `None`, but must be assigned to `Some` before configuration
    /// is finished.
    pub received_event: Option<EventId>,
    /// Access to the received event value.
    pub received_event_access: Access,
    /// The targeted event filter. This should be a subset of
    /// [`Self::component_access`].
    pub targeted_event_expr: BoolExpr<ComponentIdx>,
    /// The set of untargeted events sent by the system.
    pub sent_untargeted_events: BitSet<UntargetedEventIdx>,
    /// The set of targeted events sent by the system.
    pub sent_targeted_events: BitSet<TargetedEventIdx>,
    /// Access to the queue of events.
    pub event_queue_access: Access,
    /// Expression describing the components accessed by the system.
    pub component_access: ComponentAccessExpr,
    /// The set of components referenced by this system. Used for system cleanup
    /// when a component is removed.
    ///
    /// This is a superset of the components accessed by this system. Consider
    /// the query `Has<&C>`: `Has` does not access `C`, but it still makes use
    /// of `C`'s component index, so the whole system must be removed when
    /// component `C` is removed.
    pub referenced_components: BitSet<ComponentIdx>,
}

impl Config {
    /// Creates the default configuration.
    pub fn new() -> Self {
        Self {
            priority: Default::default(),
            received_event: Default::default(),
            received_event_access: Default::default(),
            targeted_event_expr: BoolExpr::new(false),
            sent_untargeted_events: Default::default(),
            sent_targeted_events: Default::default(),
            event_queue_access: Default::default(),
            component_access: ComponentAccessExpr::new(false),
            referenced_components: Default::default(),
        }
    }
}

impl Default for Config {
    fn default() -> Self {
        Self::new()
    }
}

/// A parameter in a [`System`].
///
/// # Deriving
///
/// This trait can be implemented automatically by using the associated derive
/// macro. The macro works if every field of the struct is also a system param.
///
/// ```
/// use std::marker::PhantomData;
///
/// use evenio::prelude::*;
///
/// #[derive(Component)]
/// struct C;
///
/// #[derive(Event)]
/// struct E;
///
/// #[derive(SystemParam)]
/// struct MySystemParam<'a> {
///     foo: Fetcher<'a, &'static C>,
///     bar: Sender<'a, E>,
/// }
///
/// let mut world = World::new();
///
/// // Add system which uses our custom system param.
/// world.add_system(|_: Receiver<E>, my_param: MySystemParam| {
///     // ...
/// });
/// ```
///
/// # Safety
///
/// Implementors must ensure that [`SystemParam::init`] correctly registers the
/// data accessed by [`SystemParam::get`].
pub unsafe trait SystemParam {
    /// Persistent data stored in the system.
    type State: Send + Sync + 'static;

    /// The type produced by this system param. This is expected to be the type
    /// of `Self` but with the lifetime of `'a`.
    type Item<'a>;

    /// Initializes the system using the input [`World`] and [`Config`].
    ///
    /// If initialization fails, [`InitError`] is returned and the system is not
    /// considered initialized.
    fn init(world: &mut World, config: &mut Config) -> Result<Self::State, InitError>;

    /// Obtains a new instance of the system parameter.
    ///
    /// # Safety
    ///
    /// - `state` must be up to date and originate from [`SystemParam::init`].
    /// - `info` must be correct for the system which this is invoked from.
    /// - `event_ptr`: See [`System::run`].
    /// - `target_location`: See [`System::run`].
    /// - `world` must have permission to access the data configured in
    ///   [`SystemParam::init`].
    unsafe fn get<'a>(
        state: &'a mut Self::State,
        info: &'a SystemInfo,
        event_ptr: EventPtr<'a>,
        target_location: EntityLocation,
        world: UnsafeWorldCell<'a>,
    ) -> Self::Item<'a>;

    /// Refresh an archetype for this system param. Called whenever
    /// [`System::refresh_archetype`] is called.
    fn refresh_archetype(state: &mut Self::State, arch: &Archetype);

    /// Remove the given archetype for this system param. Called whenever
    /// [`System::remove_archetype`] is called.
    fn remove_archetype(state: &mut Self::State, arch: &Archetype);
}

unsafe impl<T> SystemParam for PhantomData<T> {
    type State = ();

    type Item<'a> = Self;

    fn init(_world: &mut World, _config: &mut Config) -> Result<Self::State, InitError> {
        Ok(())
    }

    unsafe fn get<'a>(
        _state: &'a mut Self::State,
        _info: &'a SystemInfo,
        _event_ptr: EventPtr<'a>,
        _target_location: EntityLocation,
        _world: UnsafeWorldCell<'a>,
    ) -> Self::Item<'a> {
        Self
    }

    fn refresh_archetype(_state: &mut Self::State, _arch: &Archetype) {}

    fn remove_archetype(_state: &mut Self::State, _arch: &Archetype) {}
}

macro_rules! impl_system_param_tuple {
    ($(($P:ident, $s:ident)),*) => {
        #[allow(unused_variables, clippy::unused_unit)]
        unsafe impl<$($P: SystemParam),*> SystemParam for ($($P,)*) {
            type State = ($($P::State,)*);

            type Item<'a> = ($($P::Item<'a>,)*);

            #[inline]
            fn init(world: &mut World, config: &mut Config) -> Result<Self::State, InitError> {
                Ok((
                    $(
                        $P::init(world, config)?,
                    )*
                ))
            }

            #[inline]
            unsafe fn get<'a>(
                ($($s,)*): &'a mut Self::State,
                info: &'a SystemInfo,
                event_ptr: EventPtr<'a>,
                target_location: EntityLocation,
                world: UnsafeWorldCell<'a>,
            ) -> Self::Item<'a> {
                (
                    $(
                        $P::get($s, info, event_ptr, target_location, world),
                    )*
                )
            }

            fn refresh_archetype(
                ($($s,)*): &mut Self::State,
                arch: &Archetype
            )
            {
                $(
                    $P::refresh_archetype($s, arch);
                )*
            }

            fn remove_archetype(
                ($($s,)*): &mut Self::State,
                arch: &Archetype
            )
            {
                $(
                    $P::remove_archetype($s, arch);
                )*
            }
        }
    }
}

all_tuples!(impl_system_param_tuple, 0, 15, P, s);

/// The [`System`] implementation for ordinary functions.
///
/// This is obtained by using the [`IntoSystem`] impl on functions which accept
/// only [`SystemParam`]s.
pub struct FunctionSystem<Marker, F: SystemParamFunction<Marker>> {
    func: F,
    state: Option<<F::Param as SystemParam>::State>,
}

impl<Marker, F> FunctionSystem<Marker, F>
where
    F: SystemParamFunction<Marker>,
{
    /// Create a new uninitialized function system.
    pub fn new(func: F) -> Self {
        Self { func, state: None }
    }
}

impl<Marker, F> fmt::Debug for FunctionSystem<Marker, F>
where
    F: SystemParamFunction<Marker> + fmt::Debug,
    <F::Param as SystemParam>::State: fmt::Debug,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("FunctionSystem")
            .field("func", &self.func)
            .field("state", &self.state)
            .finish()
    }
}

impl<Marker, F> System for FunctionSystem<Marker, F>
where
    F: SystemParamFunction<Marker>,
    Marker: 'static,
{
    fn type_id(&self) -> Option<TypeId> {
        Some(TypeId::of::<F>())
    }

    fn name(&self) -> Cow<'static, str> {
        Cow::Borrowed(any::type_name::<F>())
    }

    fn init(&mut self, world: &mut World, config: &mut Config) -> Result<(), InitError> {
        self.state = Some(<F::Param as SystemParam>::init(world, config)?);
        Ok(())
    }

    unsafe fn run(
        &mut self,
        system_info: &SystemInfo,
        event_ptr: EventPtr,
        target_location: EntityLocation,
        world: UnsafeWorldCell,
    ) {
        let state = unsafe {
            self.state
                .as_mut()
                .expect_debug_checked("system must be initialized")
        };

        let param =
            <F::Param as SystemParam>::get(state, system_info, event_ptr, target_location, world);
        self.func.run(param);
    }

    fn refresh_archetype(&mut self, arch: &Archetype) {
        let state = unsafe {
            self.state
                .as_mut()
                .expect_debug_checked("system must be initialized")
        };

        F::Param::refresh_archetype(state, arch)
    }

    fn remove_archetype(&mut self, arch: &Archetype) {
        let state = unsafe {
            self.state
                .as_mut()
                .expect_debug_checked("system must be initialized")
        };

        F::Param::remove_archetype(state, arch)
    }
}

/// Trait for functions whose parameters are [`SystemParam`]s.
pub trait SystemParamFunction<Marker>: Send + Sync + 'static {
    /// The system params used by this function, combined into a single type.
    type Param: SystemParam;

    /// Call the function.
    fn run(&mut self, param: <Self::Param as SystemParam>::Item<'_>);
}

macro_rules! impl_system_param_function {
    ($(($P:ident, $p:ident)),*) => {
        impl<F, $($P: SystemParam),*> SystemParamFunction<fn($($P),*)> for F
        where
            F: FnMut($($P),*) + FnMut($($P::Item<'_>),*) + Send + Sync + 'static,
        {
            type Param = ($($P,)*);

            fn run(
                &mut self,
                ($($p,)*): <Self::Param as SystemParam>::Item<'_>
            ) {
                (self)($($p),*)
            }
        }
    }
}

all_tuples!(impl_system_param_function, 0, 15, P, p);

/// A [`SystemParam`] for storing system-local state.
///
/// Any type that implements [`Default`] can be wrapped in a `Local`.
///
/// # Examples
///
/// ```
/// use evenio::prelude::*;
/// use evenio::system::Local;
///
/// #[derive(Event)]
/// struct E;
///
/// let mut world = World::new();
///
/// fn my_system(_: Receiver<E>, mut counter: Local<u32>) {
///     println!("counter is {counter}");
///     *counter += 1;
/// }
///
/// let sys = world.add_system(my_system);
/// world.send(E);
/// world.send(E);
/// world.send(E);
///
/// // System is destroyed and re-added, so the local is reset.
/// println!("refreshing system...");
/// world.remove_system(sys);
/// world.add_system(my_system);
/// world.send(E);
/// world.send(E);
/// ```
///
/// Output:
///
/// ```txt
/// counter is 0
/// counter is 1
/// counter is 2
/// refreshing system...
/// counter is 0
/// counter is 1
/// ```
#[derive(Debug)]
pub struct Local<'a, T> {
    state: &'a mut T,
}

unsafe impl<T: Default + Send + 'static> SystemParam for Local<'_, T> {
    type State = Exclusive<T>;

    type Item<'a> = Local<'a, T>;

    fn init(_world: &mut World, _config: &mut Config) -> Result<Self::State, InitError> {
        Ok(Exclusive::new(T::default()))
    }

    unsafe fn get<'a>(
        state: &'a mut Self::State,
        _info: &'a SystemInfo,
        _event_ptr: EventPtr<'a>,
        _target_location: EntityLocation,
        _world: UnsafeWorldCell<'a>,
    ) -> Self::Item<'a> {
        Local {
            state: state.get_mut(),
        }
    }

    fn refresh_archetype(_state: &mut Self::State, _arch: &Archetype) {}

    fn remove_archetype(_state: &mut Self::State, _arch: &Archetype) {}
}

impl<T> Deref for Local<'_, T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        self.state
    }
}

impl<T> DerefMut for Local<'_, T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        self.state
    }
}

impl<T: fmt::Display> fmt::Display for Local<'_, T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.state.fmt(f)
    }
}

/// Obtains the [`SystemInfo`] for the running system.
unsafe impl SystemParam for &'_ SystemInfo {
    type State = ();

    type Item<'a> = &'a SystemInfo;

    fn init(_world: &mut World, _config: &mut Config) -> Result<Self::State, InitError> {
        Ok(())
    }

    unsafe fn get<'a>(
        _state: &'a mut Self::State,
        info: &'a SystemInfo,
        _event_ptr: EventPtr<'a>,
        _target_location: EntityLocation,
        _world: UnsafeWorldCell<'a>,
    ) -> Self::Item<'a> {
        info
    }

    fn refresh_archetype(_state: &mut Self::State, _arch: &Archetype) {}

    fn remove_archetype(_state: &mut Self::State, _arch: &Archetype) {}
}

#[cfg(feature = "std")]
#[cfg_attr(docsrs, doc(cfg(feature = "std")))]
unsafe impl<P: SystemParam> SystemParam for std::sync::Mutex<P> {
    type State = P::State;

    type Item<'a> = std::sync::Mutex<P::Item<'a>>;

    fn init(world: &mut World, config: &mut Config) -> Result<Self::State, InitError> {
        P::init(world, config)
    }

    unsafe fn get<'a>(
        state: &'a mut Self::State,
        info: &'a SystemInfo,
        event_ptr: EventPtr<'a>,
        target_location: EntityLocation,
        world: UnsafeWorldCell<'a>,
    ) -> Self::Item<'a> {
        std::sync::Mutex::new(P::get(state, info, event_ptr, target_location, world))
    }

    fn refresh_archetype(state: &mut Self::State, arch: &Archetype) {
        P::refresh_archetype(state, arch)
    }

    fn remove_archetype(state: &mut Self::State, arch: &Archetype) {
        P::remove_archetype(state, arch)
    }
}

#[cfg(feature = "std")]
#[cfg_attr(docsrs, doc(cfg(feature = "std")))]
unsafe impl<P: SystemParam> SystemParam for std::sync::RwLock<P> {
    type State = P::State;

    type Item<'a> = std::sync::RwLock<P::Item<'a>>;

    fn init(world: &mut World, config: &mut Config) -> Result<Self::State, InitError> {
        P::init(world, config)
    }

    unsafe fn get<'a>(
        state: &'a mut Self::State,
        info: &'a SystemInfo,
        event_ptr: EventPtr<'a>,
        target_location: EntityLocation,
        world: UnsafeWorldCell<'a>,
    ) -> Self::Item<'a> {
        std::sync::RwLock::new(P::get(state, info, event_ptr, target_location, world))
    }

    fn refresh_archetype(state: &mut Self::State, arch: &Archetype) {
        P::refresh_archetype(state, arch)
    }

    fn remove_archetype(state: &mut Self::State, arch: &Archetype) {
        P::remove_archetype(state, arch)
    }
}

/// An event sent immediately after a new system is added to the world.
/// Contains the ID of the added system.
#[derive(Event, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
pub struct AddSystem(pub SystemId);

/// An event sent immediately before a system is removed from the world.
/// Contains the ID of the system to be removed.
#[derive(Event, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
pub struct RemoveSystem(pub SystemId);

#[cfg(test)]
mod tests {
    use evenio::prelude::*;

    use super::*;
    use crate::event::Events;

    #[test]
    fn derive_system_param() {
        #![allow(dead_code)]

        #[derive(SystemParam)]
        struct UnitParam;

        #[derive(SystemParam)]
        struct ParamWithLifetime<'a> {
            foo: &'a Systems,
        }

        #[derive(SystemParam)]
        struct ParamWithTwoLifetimes<'a, 'b> {
            foo: &'a Systems,
            bar: &'b Events,
        }

        #[derive(SystemParam)]
        struct ParamWithTypeParam<'a, T> {
            foo: &'a Systems,
            bar: T,
        }

        #[derive(SystemParam)]
        struct TupleStructParam<'a>(&'a Systems, &'a Events);

        assert_system_param::<UnitParam>();
        assert_system_param::<ParamWithLifetime>();
        assert_system_param::<ParamWithTwoLifetimes>();
        assert_system_param::<ParamWithTypeParam<()>>();

        fn assert_system_param<P: SystemParam>() {}
    }

    #[test]
    fn system_run_order() {
        let mut world = World::new();

        #[derive(Event)]
        struct E(#[event(target)] EntityId);

        #[derive(Component)]
        struct Tracker(String);

        fn a(_: Receiver<E, &mut Tracker>) {
            unreachable!()
        }
        fn b(r: Receiver<E, &mut Tracker>) {
            assert_eq!(r.query.0, "");
            r.query.0.push('b');
        }
        fn c(r: Receiver<E, &Tracker>) {
            assert_eq!(r.query.0, "b");
        }

        let a_id = world.add_system(a);
        world.add_system(b);

        world.remove_system(a_id);

        world.add_system(c);

        let e = world.spawn();
        world.insert(e, Tracker(String::new()));

        world.send(E(e));
    }
}
