//! Event handlers

use alloc::borrow::Cow;
use alloc::collections::BTreeMap;
#[cfg(not(feature = "std"))]
use alloc::{boxed::Box, vec, vec::Vec};
use core::any::TypeId;
use core::cmp::Ordering;
use core::hash::{Hash, Hasher};
use core::marker::PhantomData;
use core::ops::{Deref, DerefMut, Index};
use core::ptr::NonNull;
use core::{any, fmt};

use evenio_macros::all_tuples;
pub use evenio_macros::HandlerParam;

use crate::access::{Access, ComponentAccess};
use crate::aliased_box::AliasedBox;
use crate::archetype::Archetype;
use crate::bit_set::BitSet;
use crate::component::ComponentIdx;
use crate::entity::EntityLocation;
use crate::event::{EventId, EventPtr, GlobalEvent, GlobalEventIdx, TargetedEventIdx};
use crate::map::TypeIdMap;
use crate::slot_map::{Key, SlotMap};
use crate::sparse::SparseIndex;
use crate::world::{UnsafeWorldCell, World};

/// Contains metadata for all the handlers in a world.
///
/// This can be obtained in a handler by using the `&Handlers` handler
/// parameter.
///
/// ```
/// # use evenio::prelude::*;
/// # use evenio::handler::Handlers;
/// #
/// # #[derive(GlobalEvent)] struct E;
/// #
/// # let mut world = World::new();
/// world.add_handler(|_: Receiver<E>, handlers: &Handlers| {});
/// ```
#[derive(Debug)]
pub struct Handlers {
    infos: SlotMap<HandlerInfo>,
    /// Maps untargeted event indices to the ordered list of handlers that
    /// handle the event.
    by_global_event: Vec<HandlerList>,
    by_type_id: TypeIdMap<HandlerInfoPtr>,
    /// Counts up as new handlers are added.
    insert_counter: u64,
    /// Handlers ordered by the order they were added to the world. This ensures
    /// that iteration over all handlers is done in insertion order.
    by_insert_order: BTreeMap<u64, HandlerInfoPtr>,
}

impl Handlers {
    pub(crate) fn new() -> Self {
        Self {
            infos: SlotMap::new(),
            by_global_event: vec![],
            by_type_id: Default::default(),
            insert_counter: 0,
            by_insert_order: BTreeMap::new(),
        }
    }

    pub(crate) fn add(&mut self, info: HandlerInfo) -> HandlerId {
        let ptr = info.ptr();

        if let Some(type_id) = info.type_id() {
            assert!(self.by_type_id.insert(type_id, ptr).is_none());
        }

        let Some(k) = self.infos.insert_with(|k| {
            let id = HandlerId(k);

            let inner = unsafe { &mut *ptr.0.as_ptr() };

            inner.id = id;
            inner.order = self.insert_counter;

            if let EventId::Global(id) = info.received_event() {
                let idx = id.index().0 as usize;

                if idx >= self.by_global_event.len() {
                    self.by_global_event
                        .resize_with(idx + 1, HandlerList::default);
                }

                self.by_global_event[idx].insert(ptr, info.priority())
            }

            info
        }) else {
            panic!("too many handlers")
        };

        self.by_insert_order.insert(self.insert_counter, ptr);
        self.insert_counter += 1;

        debug_assert_eq!(self.infos.len(), self.by_insert_order.len() as u32);

        HandlerId(k)
    }

    pub(crate) fn remove(&mut self, id: HandlerId) -> Option<HandlerInfo> {
        let info = self.infos.remove(id.0)?;

        if let EventId::Global(event_id) = info.received_event() {
            let list = &mut self.by_global_event[event_id.index().0 as usize];
            list.remove(info.ptr());
        }

        if let Some(type_id) = info.type_id() {
            self.by_type_id.remove(&type_id);
        }

        self.by_insert_order.remove(&info.order());

        debug_assert_eq!(self.infos.len(), self.by_insert_order.len() as u32);

        Some(info)
    }

    pub(crate) fn register_event(&mut self, event_idx: GlobalEventIdx) {
        let idx = event_idx.0 as usize;
        if idx >= self.by_global_event.len() {
            self.by_global_event
                .resize_with(idx + 1, HandlerList::default);
        }
    }

    pub(crate) fn get_global_list(&self, idx: GlobalEventIdx) -> Option<&HandlerList> {
        self.by_global_event.get(idx.0 as usize)
    }

    /// Gets the [`HandlerInfo`] of the given handler. Returns `None` if the ID
    /// is invalid.
    pub fn get(&self, id: HandlerId) -> Option<&HandlerInfo> {
        self.infos.get(id.0)
    }

    pub(crate) fn get_mut(&mut self, id: HandlerId) -> Option<&mut HandlerInfo> {
        self.infos.get_mut(id.0)
    }

    /// Gets the [`HandlerInfo`] for a handler using its [`HandlerIdx`].
    /// Returns `None` if the index is invalid.
    pub fn get_by_index(&self, idx: HandlerIdx) -> Option<&HandlerInfo> {
        self.infos.get_by_index(idx.0).map(|(_, v)| v)
    }

    /// Gets the [`HandlerInfo`] for a handler using its [`TypeId`]. Returns
    /// `None` if the `TypeId` does not map to a handler.
    pub fn get_by_type_id(&self, id: TypeId) -> Option<&HandlerInfo> {
        self.by_type_id.get(&id).map(|p| unsafe { p.as_info() })
    }

    /// Does the given handler exist in the world?
    pub fn contains(&self, id: HandlerId) -> bool {
        self.get(id).is_some()
    }

    /// Returns an iterator over all handler infos in insertion order.
    pub fn iter(&self) -> impl Iterator<Item = &HandlerInfo> {
        self.by_insert_order
            .values()
            .map(|ptr| unsafe { ptr.as_info() })
    }

    /// Returns a mutable iterator over all handler infos in insertion order.
    pub(crate) fn iter_mut(&mut self) -> impl Iterator<Item = &mut HandlerInfo> {
        self.by_insert_order
            .values_mut()
            .map(|ptr| unsafe { ptr.as_info_mut() })
    }
}

impl Index<HandlerId> for Handlers {
    type Output = HandlerInfo;

    fn index(&self, index: HandlerId) -> &Self::Output {
        if let Some(info) = self.get(index) {
            info
        } else {
            panic!("no such handler with ID of {index:?} exists")
        }
    }
}

impl Index<HandlerIdx> for Handlers {
    type Output = HandlerInfo;

    fn index(&self, index: HandlerIdx) -> &Self::Output {
        if let Some(info) = self.get_by_index(index) {
            info
        } else {
            panic!("no such handler with index of {index:?} exists")
        }
    }
}

impl Index<TypeId> for Handlers {
    type Output = HandlerInfo;

    fn index(&self, index: TypeId) -> &Self::Output {
        if let Some(info) = self.get_by_type_id(index) {
            info
        } else {
            panic!("no such handler with type ID of {index:?} exists")
        }
    }
}

unsafe impl HandlerParam for &'_ Handlers {
    type State = ();

    type This<'a> = &'a Handlers;

    fn init(_world: &mut World, _config: &mut HandlerConfig) -> Result<Self::State, InitError> {
        Ok(())
    }

    unsafe fn get<'a>(
        _state: &'a mut Self::State,
        _info: &'a HandlerInfo,
        _event_ptr: EventPtr<'a>,
        _target_location: EntityLocation,
        world: UnsafeWorldCell<'a>,
    ) -> Self::This<'a> {
        world.handlers()
    }

    fn refresh_archetype(_state: &mut Self::State, _arch: &Archetype) {}

    fn remove_archetype(_state: &mut Self::State, _arch: &Archetype) {}
}

/// Metadata for a handler.
#[repr(transparent)]
pub struct HandlerInfo(AliasedBox<HandlerInfoInner>);

/// Pointer to a handler.
#[derive(Clone, Copy, Debug)]
#[repr(transparent)]
pub(crate) struct HandlerInfoPtr(NonNull<HandlerInfoInner>);

impl HandlerInfoPtr {
    /// # Safety
    ///
    /// - Pointer must be valid.
    /// - Aliasing rules must be followed.
    pub(crate) unsafe fn as_info(&self) -> &HandlerInfo {
        // SAFETY: Both `` and `Ptr` have non-null pointer layout.
        &*(self as *const Self as *const HandlerInfo)
    }

    /// # Safety
    ///
    /// - Pointer must be valid.
    /// - Aliasing rules must be followed.
    pub(crate) unsafe fn as_info_mut(&mut self) -> &mut HandlerInfo {
        // SAFETY: Both `` and `Ptr` have non-null pointer layout.
        &mut *(self as *mut Self as *mut HandlerInfo)
    }
}

impl PartialEq for HandlerInfoPtr {
    fn eq(&self, other: &Self) -> bool {
        self.cmp(other).is_eq()
    }
}

impl Eq for HandlerInfoPtr {}

impl PartialOrd for HandlerInfoPtr {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for HandlerInfoPtr {
    fn cmp(&self, other: &Self) -> Ordering {
        // Ignore ptr metadata by casting to thin pointer.
        self.0.cast::<()>().cmp(&other.0.cast::<()>())
    }
}

impl Hash for HandlerInfoPtr {
    fn hash<H: Hasher>(&self, state: &mut H) {
        // Ignore ptr metadata by casting to thin pointer.
        self.0.cast::<()>().hash(state)
    }
}

// This is generic over `S` so that we can do an unsizing coercion.
pub(crate) struct HandlerInfoInner<H: ?Sized = dyn Handler> {
    pub(crate) name: Cow<'static, str>,
    pub(crate) id: HandlerId,
    pub(crate) type_id: Option<TypeId>,
    pub(crate) order: u64,
    pub(crate) received_event: EventId,
    pub(crate) received_event_access: Access,
    pub(crate) targeted_event_component_access: ComponentAccess,
    pub(crate) sent_untargeted_events: BitSet<GlobalEventIdx>,
    pub(crate) sent_targeted_events: BitSet<TargetedEventIdx>,
    pub(crate) event_queue_access: Access,
    pub(crate) component_access: ComponentAccess,
    pub(crate) archetype_filter: ComponentAccess,
    pub(crate) referenced_components: BitSet<ComponentIdx>,
    pub(crate) priority: HandlerPriority,
    // SAFETY: There is intentionally no public accessor for this field as it would lead to mutable
    // aliasing.
    pub(crate) handler: H,
}

impl HandlerInfo {
    pub(crate) fn new<H: Handler>(inner: HandlerInfoInner<H>) -> Self {
        // Perform unsizing coercion using `Box`, then convert the box into an
        // `AliasedBox`.
        let b: Box<HandlerInfoInner> = Box::new(inner);
        Self(b.into())
    }

    /// Gets the name of the handler.
    ///
    /// This name is intended for debugging purposes and should not be relied
    /// upon for correctness.
    pub fn name(&self) -> &str {
        unsafe { &(*AliasedBox::as_ptr(&self.0)).name }
    }

    /// Gets the ID of this handler.
    pub fn id(&self) -> HandlerId {
        unsafe { (*AliasedBox::as_ptr(&self.0)).id }
    }

    /// Gets the [`TypeId`] of this handler, if any.
    pub fn type_id(&self) -> Option<TypeId> {
        unsafe { (*AliasedBox::as_ptr(&self.0)).type_id }
    }

    pub(crate) fn order(&self) -> u64 {
        unsafe { (*AliasedBox::as_ptr(&self.0)).order }
    }

    /// Gets the [`EventId`] of the event this handler listens for.
    pub fn received_event(&self) -> EventId {
        unsafe { (*AliasedBox::as_ptr(&self.0)).received_event }
    }

    /// Gets the handler's [`Access`] to the event it listens for.
    pub fn received_event_access(&self) -> Access {
        unsafe { (*AliasedBox::as_ptr(&self.0)).received_event_access }
    }

    /// Gets the expression describing the handler's targeted event query, or
    /// `None` if this handler is not targeted.
    pub fn targeted_event_component_access(&self) -> Option<&ComponentAccess> {
        self.received_event()
            .is_targeted()
            .then(|| unsafe { &(*AliasedBox::as_ptr(&self.0)).targeted_event_component_access })
    }

    /// Returns an iterator over the set of global events this handler sends.
    pub fn sent_global_events(
        &self,
    ) -> impl Iterator<Item = GlobalEventIdx> + Clone + fmt::Debug + '_ {
        self.sent_global_events_bitset().iter()
    }

    pub(crate) fn sent_global_events_bitset(&self) -> &BitSet<GlobalEventIdx> {
        unsafe { &(*AliasedBox::as_ptr(&self.0)).sent_untargeted_events }
    }

    /// Returns an iterator over the set of targeted events this handler sends.
    pub fn sent_targeted_events(
        &self,
    ) -> impl Iterator<Item = TargetedEventIdx> + Clone + fmt::Debug + '_ {
        self.sent_targeted_events_bitset().iter()
    }

    pub(crate) fn sent_targeted_events_bitset(&self) -> &BitSet<TargetedEventIdx> {
        unsafe { &(*AliasedBox::as_ptr(&self.0)).sent_targeted_events }
    }

    /// Gets this handler's [`Access`] to the event queue.
    pub fn event_queue_access(&self) -> Access {
        unsafe { (*AliasedBox::as_ptr(&self.0)).event_queue_access }
    }

    /// Gets the expression describing this handler's access
    pub fn component_access(&self) -> &ComponentAccess {
        unsafe { &(*AliasedBox::as_ptr(&self.0)).component_access }
    }

    /// Returns the [`ComponentAccess`] used for matching archetypes.
    pub fn archetype_filter(&self) -> &ComponentAccess {
        unsafe { &(*AliasedBox::as_ptr(&self.0)).archetype_filter }
    }

    /// Gets the set of components referenced by this handler.
    ///
    /// Referenced components are components used by the handler in any way.
    /// Used for cleanup when removing components.
    pub fn referenced_components(
        &self,
    ) -> impl Iterator<Item = ComponentIdx> + Clone + fmt::Debug + '_ {
        unsafe { &(*AliasedBox::as_ptr(&self.0)).referenced_components }.iter()
    }

    /// Does this handler reference the given component?
    pub fn references_component(&self, idx: ComponentIdx) -> bool {
        unsafe { &(*AliasedBox::as_ptr(&self.0)).referenced_components }.contains(idx)
    }

    /// Gets the [`HandlerPriority`] of this handler.
    pub fn priority(&self) -> HandlerPriority {
        unsafe { (*AliasedBox::as_ptr(&self.0)).priority }
    }

    pub(crate) fn ptr(&self) -> HandlerInfoPtr {
        HandlerInfoPtr(AliasedBox::as_non_null(&self.0))
    }

    pub(crate) fn handler_mut(&mut self) -> &mut dyn Handler {
        unsafe { &mut (*AliasedBox::as_mut_ptr(&mut self.0)).handler }
    }
}

impl fmt::Debug for HandlerInfo {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("")
            .field("name", &self.name())
            .field("id", &self.id())
            .field("type_id", &self.type_id())
            .field("order", &self.order())
            .field("received_event", &self.received_event())
            .field("received_event_access", &self.received_event_access())
            .field("targeted_event_component_access", &self.targeted_event_component_access())
            .field("sent_global_events", &self.sent_global_events())
            .field("sent_targeted_events", &self.sent_targeted_events())
            .field("event_queue_access", &self.event_queue_access())
            .field("component_access", &self.component_access())
            .field("archetype_filter", &self.archetype_filter())
            .field("referenced_components", &self.referenced_components())
            .field("priority", &self.priority())
            // Don't access the `handler` field.
            .finish_non_exhaustive()
    }
}

#[derive(Debug, Default)]
pub(crate) struct HandlerList {
    before: u32,
    after: u32,
    entries: Vec<HandlerInfoPtr>,
}

unsafe impl Sync for HandlerList {}

impl HandlerList {
    pub(crate) const fn new() -> HandlerList {
        Self {
            before: 0,
            after: 0,
            entries: vec![],
        }
    }

    pub(crate) fn insert(&mut self, ptr: HandlerInfoPtr, priority: HandlerPriority) {
        assert!(self.entries.len() < u32::MAX as usize);

        match priority {
            HandlerPriority::High => {
                self.entries.insert(self.before as usize, ptr);
                self.before += 1;
                self.after += 1;
            }
            HandlerPriority::Medium => {
                self.entries.insert(self.after as usize, ptr);
                self.after += 1;
            }
            HandlerPriority::Low => {
                self.entries.push(ptr);
            }
        }
    }

    pub(crate) fn remove(&mut self, ptr: HandlerInfoPtr) -> bool {
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

    pub(crate) fn slice(&self) -> &[HandlerInfoPtr] {
        &self.entries
    }
}

/// Lightweight identifier for a handler.
///
/// Handler identifiers are implemented using an [index] and a generation count.
/// The generation count ensures that IDs from removed handlers are not reused
/// by new handlers.
///
/// A handler identifier is only meaningful in the [`World`] it was created
/// from. Attempting to use a handler ID in a different world will have
/// unexpected results.
///
/// [index]: HandlerIdx
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Default, Debug)]
pub struct HandlerId(Key);

impl HandlerId {
    /// The handler ID which never identifies a live handler. This is the
    /// default value for `HandlerId`.
    pub const NULL: Self = Self(Key::NULL);

    /// Returns the index of this ID.
    pub const fn index(self) -> HandlerIdx {
        HandlerIdx(self.0.index())
    }

    /// Returns the generation count of this ID.
    pub const fn generation(self) -> u32 {
        self.0.generation().get()
    }
}

/// A [`HandlerId`] with the generation count stripped out.
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
pub struct HandlerIdx(pub u32);

unsafe impl SparseIndex for HandlerIdx {
    const MAX: Self = Self(u32::MAX);

    fn index(self) -> usize {
        self.0.index()
    }

    fn from_index(idx: usize) -> Self {
        Self(u32::from_index(idx))
    }
}

/// Types which can be converted into [`Handler`]s.
///
/// This trait is implemented for all functions that return `()` and whose
/// arguments are all [`HandlerParam`]s.
#[diagnostic::on_unimplemented(
    message = "the type `{Self}` is not a valid handler",
    note = "it is likely that one of the handler's function parameters is not a valid \
            `HandlerParam`",
    note = "try removing parameters individually until `IntoHandler<_>` is implemented"
)]
pub trait IntoHandler<Marker>: Sized {
    /// The handler type to convert to.
    type Handler: Handler;

    /// Performs the conversion into a [`Handler`].
    fn into_handler(self) -> Self::Handler;

    /// Ignore this handler's reported [`TypeId`]. This can be used to add a
    /// specific handler to the world more than once.
    ///
    /// # Examples
    ///
    /// ```
    /// use evenio::prelude::*;
    ///
    /// let mut world = World::new();
    ///
    /// let id_1 = world.add_handler(my_handler);
    /// let id_2 = world.add_handler(my_handler.no_type_id());
    /// let id_3 = world.add_handler(my_handler);
    ///
    /// assert_ne!(id_1, id_2);
    /// assert_eq!(id_1, id_3);
    /// #
    /// # fn my_handler(_: Receiver<E>) {}
    /// #
    /// # #[derive(GlobalEvent)]
    /// # struct E;
    /// ```
    fn no_type_id(self) -> NoTypeId<Self::Handler> {
        NoTypeId(self.into_handler())
    }

    /// Returns a wrapper which sets the priority of this handler to
    /// [`HandlerPriority::High`].
    fn high(self) -> HighPriority<Self::Handler> {
        HighPriority(self.into_handler())
    }

    /// Returns a wrapper which sets the priority of this handler to
    /// [`HandlerPriority::Low`].
    fn low(self) -> LowPriority<Self::Handler> {
        LowPriority(self.into_handler())
    }
}

#[doc(hidden)]
#[derive(Debug)]
pub enum FunctionHandlerMarker {}

impl<Marker, F> IntoHandler<(FunctionHandlerMarker, Marker)> for F
where
    Marker: 'static,
    F: HandlerParamFunction<Marker>,
{
    type Handler = FunctionHandler<Marker, F>;

    fn into_handler(self) -> Self::Handler {
        FunctionHandler::new(self)
    }
}

impl<H: Handler> IntoHandler<()> for H {
    type Handler = Self;

    fn into_handler(self) -> Self::Handler {
        self
    }
}

/// The wrapper handler returned by [`IntoHandler::no_type_id`].
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Default, Debug)]
pub struct NoTypeId<S>(pub S);

impl<H: Handler> Handler for NoTypeId<H> {
    fn type_id(&self) -> Option<TypeId> {
        None
    }

    fn name(&self) -> Cow<'static, str> {
        self.0.name()
    }

    fn init(&mut self, world: &mut World, config: &mut HandlerConfig) -> Result<(), InitError> {
        self.0.init(world, config)
    }

    unsafe fn run(
        &mut self,
        info: &HandlerInfo,
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

/// The wrapper handler returned by [`IntoHandler::high`].
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Default, Debug)]
pub struct HighPriority<S>(pub S);

impl<H: Handler> Handler for HighPriority<H> {
    fn type_id(&self) -> Option<TypeId> {
        self.0.type_id()
    }

    fn name(&self) -> Cow<'static, str> {
        self.0.name()
    }

    fn init(&mut self, world: &mut World, config: &mut HandlerConfig) -> Result<(), InitError> {
        let res = self.0.init(world, config);
        config.set_priority(HandlerPriority::High);
        res
    }

    unsafe fn run(
        &mut self,
        info: &HandlerInfo,
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

/// The wrapper handler returned by [`IntoHandler::low`].
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Default, Debug)]
pub struct LowPriority<S>(pub S);

impl<H: Handler> Handler for LowPriority<H> {
    fn type_id(&self) -> Option<TypeId> {
        self.0.type_id()
    }

    fn name(&self) -> Cow<'static, str> {
        self.0.name()
    }

    fn init(&mut self, world: &mut World, config: &mut HandlerConfig) -> Result<(), InitError> {
        let res = self.0.init(world, config);
        config.set_priority(HandlerPriority::Low);
        res
    }

    unsafe fn run(
        &mut self,
        info: &HandlerInfo,
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

/// A callback function that listens for events.
///
/// handlers are added to a world using the [`World::add_handler`] method.
///
/// For more information about handlers, see the [tutorial](crate::tutorial).
pub trait Handler: 'static {
    /// Returns the [`TypeId`] which uniquely identifies this handler, or `None`
    /// if there is none.
    ///
    /// No two handlers with the same [`TypeId`] will exist in the [`World`] at
    /// the same time.
    fn type_id(&self) -> Option<TypeId>;

    /// Returns the name of this handler for debugging purposes.
    fn name(&self) -> Cow<'static, str>;

    /// Initializes the handler. Returns [`InitError`] on failure.
    fn init(&mut self, world: &mut World, config: &mut HandlerConfig) -> Result<(), InitError>;

    /// Execute the handler by passing in the handler's metadata, a pointer to
    /// the received event of the configured type, the entity location of
    /// the event's target, and an [`UnsafeWorldCell`] with permission to
    /// access the data described in the configuration.
    ///
    /// # Safety
    ///
    /// - handler must be initialized via [`init`].
    /// - `info` must be the correct information for this handler.
    /// - `event_ptr` must point to the correct type of event configured by this
    ///   handler in [`init`].
    /// - `target_location` must be a valid location to an entity matching the
    ///   component access set by [`set_targeted_event_component_access`],
    ///   unless the event is not targeted.
    /// - `world` must have permission to access all data configured by this
    ///   handler in [`init`].
    ///
    /// [`init`]: Self::init
    /// [`set_targeted_event_component_access`]: HandlerConfig::set_targeted_event_component_access
    unsafe fn run(
        &mut self,
        info: &HandlerInfo,
        event_ptr: EventPtr,
        target_location: EntityLocation,
        world: UnsafeWorldCell,
    );

    /// Notifies the handler that an archetype it might care about had its
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

    /// Notifies the handler that an archetype it might care about is no longer
    /// available.
    ///
    /// This is invoked in the following scenarios:
    /// - The archetype was removed from the world.
    /// - The archetype previously had entities in it, but is now empty.
    ///
    /// In either case, the handler must assume that the archetype is no longer
    /// available. Attempting to read the component data from a removed
    /// archetype is illegal.
    fn remove_archetype(&mut self, arch: &Archetype);
}

/// An error returned when handler initialization fails. Contains an error
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

/// The configuration of a handler. Accessible during handler initialization.
#[derive(Clone, Default, Debug)]
pub struct HandlerConfig {
    pub(crate) priority: HandlerPriority,
    pub(crate) received_event: ReceivedEventId,
    pub(crate) received_event_access: MaybeInvalidAccess,
    pub(crate) targeted_event_component_access: ComponentAccess,
    pub(crate) sent_global_events: BitSet<GlobalEventIdx>,
    pub(crate) sent_targeted_events: BitSet<TargetedEventIdx>,
    pub(crate) event_queue_access: MaybeInvalidAccess,
    pub(crate) component_accesses: Vec<ComponentAccess>,
    pub(crate) referenced_components: BitSet<ComponentIdx>,
}

impl HandlerConfig {
    /// Creates the default configuration.
    pub fn new() -> Self {
        Self::default()
    }

    /// Overwrites the [`HandlerPriority`] of this handler.
    ///
    /// Handlers default to [`HandlerPriority::Medium`].
    pub fn set_priority(&mut self, priority: HandlerPriority) {
        self.priority = priority;
    }

    /// Sets the event sent by this handler. Causes an initialization error
    /// if a different event was previously set.
    pub fn set_received_event<E: Into<EventId>>(&mut self, event: E) {
        let event = event.into();
        self.received_event = match self.received_event {
            ReceivedEventId::None => ReceivedEventId::Ok(event),
            ReceivedEventId::Ok(old_event) => {
                if old_event == event {
                    ReceivedEventId::Ok(event)
                } else {
                    ReceivedEventId::Invalid
                }
            }
            ReceivedEventId::Invalid => ReceivedEventId::Invalid,
        };
    }

    /// Sets the [`Access`]
    pub fn set_received_event_access(&mut self, access: Access) {
        self.received_event_access = match self.received_event_access {
            MaybeInvalidAccess::Ok(old_access) => access
                .join(old_access)
                .map_or(MaybeInvalidAccess::Invalid, MaybeInvalidAccess::Ok),
            MaybeInvalidAccess::Invalid => MaybeInvalidAccess::Invalid,
        };
    }

    /// Sets the [`ComponentAccess`] describing the data accessed on the target
    /// of the event received by this handler. This should be a subset of the
    /// component access given to [`Self::push_component_access`].
    ///
    /// Has no effect if the received event is untargeted. Defaults to
    /// [`ComponentAccess::new_false`].
    pub fn set_targeted_event_component_access(&mut self, component_access: ComponentAccess) {
        self.targeted_event_component_access = component_access;
    }

    /// Inserts a global event into the set of events this handler is able to
    /// send.
    ///
    /// Returns whether the given event was already configured to be sent.
    pub fn insert_sent_global_event(&mut self, event: GlobalEventIdx) -> bool {
        self.sent_global_events.insert(event)
    }

    /// Inserts a targeted event into the set of events this handler is able to
    /// send.
    ///
    /// Returns whether the given event was already configured to be sent.
    pub fn insert_sent_targeted_event(&mut self, event: TargetedEventIdx) -> bool {
        self.sent_targeted_events.insert(event)
    }

    /// Sets the handler's access to the event queue. Produces a configuration
    /// failure if the given access conflicts with the previously set access.
    pub fn set_event_queue_access(&mut self, access: Access) {
        self.event_queue_access = match self.event_queue_access {
            MaybeInvalidAccess::Ok(old_access) => access
                .join(old_access)
                .map_or(MaybeInvalidAccess::Invalid, MaybeInvalidAccess::Ok),
            MaybeInvalidAccess::Invalid => MaybeInvalidAccess::Invalid,
        };
    }

    /// Pushes a [`ComponentAccess`] to the list of [`ComponentAccess`] for this
    /// handler. The conjunction of the accesses determines the component access
    /// of the whole handler, while the disjunction determines which archetypes
    /// are matched.
    ///
    /// Generally, this means that there should be one [`ComponentAccess`]
    /// pushed per handler param that accesses components.
    pub fn push_component_access(&mut self, component_access: ComponentAccess) {
        self.component_accesses.push(component_access);
    }

    /// Inserts a component into the set of components referenced by this
    /// handler. The set is used for cleanup when a component type is removed
    /// from the world.
    pub fn insert_referenced_components(&mut self, comp: ComponentIdx) {
        self.referenced_components.insert(comp);
    }
}

/// The priority of a handler relative to other handlers that handle the same
/// event.
///
/// If multiple handlers have the same priority, then the order they were added
/// to the [`World`] is used as a fallback.
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Default, Debug)]
pub enum HandlerPriority {
    /// The handler runs before other handlers.
    High,
    /// The default handler priority.
    #[default]
    Medium,
    /// The handler runs after other handlers.
    Low,
}

#[derive(Copy, Clone, Default, Debug)]
pub(crate) enum ReceivedEventId {
    #[default]
    None,
    Ok(EventId),
    Invalid,
}

#[derive(Copy, Clone, Debug)]
pub(crate) enum MaybeInvalidAccess {
    Ok(Access),
    Invalid,
}

impl Default for MaybeInvalidAccess {
    fn default() -> Self {
        Self::Ok(Access::default())
    }
}

/// A parameter in a [`Handler`].
///
/// # Deriving
///
/// This trait can be implemented automatically by using the associated derive
/// macro. The macro works if every field of the struct is also a handler param.
///
/// ```
/// use std::marker::PhantomData;
///
/// use evenio::prelude::*;
///
/// #[derive(Component)]
/// struct C;
///
/// #[derive(GlobalEvent)]
/// struct E;
///
/// #[derive(HandlerParam)]
/// struct MyHandlerParam<'a> {
///     foo: Fetcher<'a, &'static C>,
///     bar: Sender<'a, E>,
/// }
///
/// let mut world = World::new();
///
/// // Add handler which uses our custom handler param.
/// world.add_handler(|_: Receiver<E>, my_param: MyHandlerParam| {
///     // ...
/// });
/// ```
///
/// # Safety
///
/// Implementors must ensure that [`HandlerParam::init`] correctly registers the
/// data accessed by [`HandlerParam::get`].
pub unsafe trait HandlerParam {
    /// Persistent data stored in the handler.
    type State: 'static;

    /// The type produced by this handler param. This must be the type
    /// of `Self` but with the lifetime of `'a`.
    type This<'a>;

    /// Initializes the handler using the input [`World`] and [`HandlerConfig`].
    ///
    /// If initialization fails, [`InitError`] is returned and the handler is
    /// not considered initialized.
    fn init(world: &mut World, config: &mut HandlerConfig) -> Result<Self::State, InitError>;

    /// Obtains a new instance of the handler parameter.
    ///
    /// # Safety
    ///
    /// - `state` must be up to date and originate from [`HandlerParam::init`].
    /// - `info` must be correct for the handler which this is invoked from.
    /// - `event_ptr`: See [`Handler::run`].
    /// - `target_location`: See [`Handler::run`].
    /// - `world` must have permission to access the data configured in
    ///   [`HandlerParam::init`].
    unsafe fn get<'a>(
        state: &'a mut Self::State,
        info: &'a HandlerInfo,
        event_ptr: EventPtr<'a>,
        target_location: EntityLocation,
        world: UnsafeWorldCell<'a>,
    ) -> Self::This<'a>;

    /// Refresh an archetype for this handler param. Called whenever
    /// [`Handler::refresh_archetype`] is called.
    fn refresh_archetype(state: &mut Self::State, arch: &Archetype);

    /// Remove the given archetype for this handler param. Called whenever
    /// [`Handler::remove_archetype`] is called.
    fn remove_archetype(state: &mut Self::State, arch: &Archetype);
}

unsafe impl<T> HandlerParam for PhantomData<T> {
    type State = ();

    type This<'a> = Self;

    fn init(_world: &mut World, _config: &mut HandlerConfig) -> Result<Self::State, InitError> {
        Ok(())
    }

    unsafe fn get<'a>(
        _state: &'a mut Self::State,
        _info: &'a HandlerInfo,
        _event_ptr: EventPtr<'a>,
        _target_location: EntityLocation,
        _world: UnsafeWorldCell<'a>,
    ) -> Self::This<'a> {
        Self
    }

    fn refresh_archetype(_state: &mut Self::State, _arch: &Archetype) {}

    fn remove_archetype(_state: &mut Self::State, _arch: &Archetype) {}
}

macro_rules! impl_handler_param_tuple {
    ($(($P:ident, $s:ident)),*) => {
        #[allow(unused_variables, clippy::unused_unit)]
        unsafe impl<$($P: HandlerParam),*> HandlerParam for ($($P,)*) {
            type State = ($($P::State,)*);

            type This<'a> = ($($P::This<'a>,)*);

            #[inline]
            fn init(world: &mut World, config: &mut HandlerConfig) -> Result<Self::State, InitError> {
                Ok((
                    $(
                        $P::init(world, config)?,
                    )*
                ))
            }

            #[inline]
            unsafe fn get<'a>(
                ($($s,)*): &'a mut Self::State,
                info: &'a HandlerInfo,
                event_ptr: EventPtr<'a>,
                target_location: EntityLocation,
                world: UnsafeWorldCell<'a>,
            ) -> Self::This<'a> {
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

all_tuples!(impl_handler_param_tuple, 0, 15, P, s);

/// The [`Handler`] implementation for ordinary functions.
///
/// This is obtained by using the [`IntoHandler`] impl on functions which accept
/// only [`HandlerParam`]s.
pub struct FunctionHandler<Marker, F: HandlerParamFunction<Marker>> {
    func: F,
    state: Option<<F::Param as HandlerParam>::State>,
}

impl<Marker, F> FunctionHandler<Marker, F>
where
    F: HandlerParamFunction<Marker>,
{
    /// Create a new uninitialized function handler.
    pub fn new(func: F) -> Self {
        Self { func, state: None }
    }
}

impl<Marker, F> fmt::Debug for FunctionHandler<Marker, F>
where
    F: HandlerParamFunction<Marker> + fmt::Debug,
    <F::Param as HandlerParam>::State: fmt::Debug,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("FunctionHandler")
            .field("func", &self.func)
            .field("state", &self.state)
            .finish()
    }
}

impl<Marker, F> Handler for FunctionHandler<Marker, F>
where
    F: HandlerParamFunction<Marker>,
    Marker: 'static,
{
    fn type_id(&self) -> Option<TypeId> {
        Some(TypeId::of::<F>())
    }

    fn name(&self) -> Cow<'static, str> {
        Cow::Borrowed(any::type_name::<F>())
    }

    fn init(&mut self, world: &mut World, config: &mut HandlerConfig) -> Result<(), InitError> {
        self.state = Some(<F::Param as HandlerParam>::init(world, config)?);
        Ok(())
    }

    unsafe fn run(
        &mut self,
        handler_info: &HandlerInfo,
        event_ptr: EventPtr,
        target_location: EntityLocation,
        world: UnsafeWorldCell,
    ) {
        // Handler must be initialized.
        let state = unsafe { self.state.as_mut().unwrap_unchecked() };

        let param =
            <F::Param as HandlerParam>::get(state, handler_info, event_ptr, target_location, world);
        self.func.run(param);
    }

    fn refresh_archetype(&mut self, arch: &Archetype) {
        // Handler must be initialized.
        let state = unsafe { self.state.as_mut().unwrap_unchecked() };

        F::Param::refresh_archetype(state, arch)
    }

    fn remove_archetype(&mut self, arch: &Archetype) {
        // Handler must be initialized.
        let state = unsafe { self.state.as_mut().unwrap_unchecked() };

        F::Param::remove_archetype(state, arch)
    }
}

/// Trait for functions whose parameters are [`HandlerParam`]s.
pub trait HandlerParamFunction<Marker>: 'static {
    /// The handler params used by this function, combined into a single type.
    type Param: HandlerParam;

    /// Call the function.
    fn run(&mut self, param: <Self::Param as HandlerParam>::This<'_>);
}

macro_rules! impl_handler_param_function {
    ($(($P:ident, $p:ident)),*) => {
        impl<F, $($P: HandlerParam),*> HandlerParamFunction<fn($($P),*)> for F
        where
            F: FnMut($($P),*) + FnMut($($P::This<'_>),*) + 'static,
        {
            type Param = ($($P,)*);

            fn run(
                &mut self,
                ($($p,)*): <Self::Param as HandlerParam>::This<'_>
            ) {
                (self)($($p),*)
            }
        }
    }
}

all_tuples!(impl_handler_param_function, 0, 15, P, p);

/// A [`HandlerParam`] for storing handler-local state.
///
/// Any type that implements [`Default`] can be wrapped in a `Local`.
///
/// # Examples
///
/// ```
/// use evenio::handler::Local;
/// use evenio::prelude::*;
///
/// #[derive(GlobalEvent)]
/// struct E;
///
/// let mut world = World::new();
///
/// fn my_handler(_: Receiver<E>, mut counter: Local<u32>) {
///     println!("counter is {counter}");
///     *counter += 1;
/// }
///
/// let handler = world.add_handler(my_handler);
/// world.send(E);
/// world.send(E);
/// world.send(E);
///
/// // Handler is destroyed and re-added, so the local is reset.
/// println!("refreshing handler...");
/// world.remove_handler(handler);
/// world.add_handler(my_handler);
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
/// refreshing handler...
/// counter is 0
/// counter is 1
/// ```
#[derive(Debug)]
pub struct Local<'a, T> {
    state: &'a mut T,
}

unsafe impl<T: Default + 'static> HandlerParam for Local<'_, T> {
    type State = T;

    type This<'a> = Local<'a, T>;

    fn init(_world: &mut World, _config: &mut HandlerConfig) -> Result<Self::State, InitError> {
        Ok(T::default())
    }

    unsafe fn get<'a>(
        state: &'a mut Self::State,
        _info: &'a HandlerInfo,
        _event_ptr: EventPtr<'a>,
        _target_location: EntityLocation,
        _world: UnsafeWorldCell<'a>,
    ) -> Self::This<'a> {
        Local { state }
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

/// Obtains the [`HandlerInfo`] for the running handler.
unsafe impl HandlerParam for &'_ HandlerInfo {
    type State = ();

    type This<'a> = &'a HandlerInfo;

    fn init(_world: &mut World, _config: &mut HandlerConfig) -> Result<Self::State, InitError> {
        Ok(())
    }

    unsafe fn get<'a>(
        _state: &'a mut Self::State,
        info: &'a HandlerInfo,
        _event_ptr: EventPtr<'a>,
        _target_location: EntityLocation,
        _world: UnsafeWorldCell<'a>,
    ) -> Self::This<'a> {
        info
    }

    fn refresh_archetype(_state: &mut Self::State, _arch: &Archetype) {}

    fn remove_archetype(_state: &mut Self::State, _arch: &Archetype) {}
}

#[cfg(feature = "std")]
#[cfg_attr(docsrs, doc(cfg(feature = "std")))]
unsafe impl<P: HandlerParam> HandlerParam for std::sync::Mutex<P> {
    type State = P::State;

    type This<'a> = std::sync::Mutex<P::This<'a>>;

    fn init(world: &mut World, config: &mut HandlerConfig) -> Result<Self::State, InitError> {
        P::init(world, config)
    }

    unsafe fn get<'a>(
        state: &'a mut Self::State,
        info: &'a HandlerInfo,
        event_ptr: EventPtr<'a>,
        target_location: EntityLocation,
        world: UnsafeWorldCell<'a>,
    ) -> Self::This<'a> {
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
unsafe impl<P: HandlerParam> HandlerParam for std::sync::RwLock<P> {
    type State = P::State;

    type This<'a> = std::sync::RwLock<P::This<'a>>;

    fn init(world: &mut World, config: &mut HandlerConfig) -> Result<Self::State, InitError> {
        P::init(world, config)
    }

    unsafe fn get<'a>(
        state: &'a mut Self::State,
        info: &'a HandlerInfo,
        event_ptr: EventPtr<'a>,
        target_location: EntityLocation,
        world: UnsafeWorldCell<'a>,
    ) -> Self::This<'a> {
        std::sync::RwLock::new(P::get(state, info, event_ptr, target_location, world))
    }

    fn refresh_archetype(state: &mut Self::State, arch: &Archetype) {
        P::refresh_archetype(state, arch)
    }

    fn remove_archetype(state: &mut Self::State, arch: &Archetype) {
        P::remove_archetype(state, arch)
    }
}

/// An event sent immediately after a new handler is added to the world.
/// Contains the ID of the added handler.
#[derive(GlobalEvent, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
pub struct AddHandler(pub HandlerId);

/// An event sent immediately before a handler is removed from the world.
/// Contains the ID of the handler to be removed.
#[derive(GlobalEvent, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
pub struct RemoveHandler(pub HandlerId);

#[cfg(test)]
mod tests {
    use evenio::prelude::*;

    use super::*;
    use crate::event::GlobalEvents;

    #[test]
    #[allow(dead_code)]
    fn derive_handler_param() {
        #[derive(HandlerParam)]
        struct UnitParam;

        #[derive(HandlerParam)]
        struct ParamWithLifetime<'a> {
            foo: &'a Handlers,
        }

        #[derive(HandlerParam)]
        struct ParamWithTwoLifetimes<'a, 'b> {
            foo: &'a Handlers,
            bar: &'b GlobalEvents,
        }

        #[derive(HandlerParam)]
        struct ParamWithTypeParam<'a, T> {
            foo: &'a Handlers,
            bar: T,
        }

        #[derive(HandlerParam)]
        struct TupleStructParam<'a>(&'a Handlers, &'a GlobalEvents);

        assert_handler_param::<UnitParam>();
        assert_handler_param::<ParamWithLifetime>();
        assert_handler_param::<ParamWithTwoLifetimes>();
        assert_handler_param::<ParamWithTypeParam<()>>();

        fn assert_handler_param<P: HandlerParam>() {}
    }

    #[test]
    fn handler_run_order() {
        let mut world = World::new();

        #[derive(TargetedEvent)]
        struct E;

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

        let a_id = world.add_handler(a);
        world.add_handler(b);

        world.remove_handler(a_id);

        world.add_handler(c);

        let e = world.spawn();
        world.insert(e, Tracker(String::new()));

        world.send_to(e, E);
    }

    #[test]
    fn handler_info_aliasing() {
        let mut world = World::new();

        #[derive(GlobalEvent)]
        struct E;

        world.add_handler(|_: Receiver<E>, info: &HandlerInfo| {
            // For Miri.
            let _foo = info.name();
            let _bar = info.received_event();
            let _baz = info.referenced_components();
            let _ = format!("{info:?}");
        });

        world.send(E);
    }
}
