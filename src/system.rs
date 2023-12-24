use alloc::collections::BTreeMap;
use core::any::TypeId;
use core::fmt;
use core::ops::{Deref, DerefMut};
use core::ptr::NonNull;
use std::any;
use std::borrow::Cow;
use std::ops::Index;

use evenio_macros::all_tuples;

use crate::access::{Access, ComponentAccessExpr};
use crate::archetype::Archetype;
use crate::bit_set::BitSet;
use crate::bool_expr::BoolExpr;
use crate::component::ComponentIdx;
use crate::debug_checked::UnwrapDebugChecked;
use crate::event::{EntityEventIdx, EventId, EventIdEnum, EventIdx, EventPtr, GlobalEventIdx};
use crate::exclusive::Exclusive;
use crate::slot_map::{Key, SlotMap};
use crate::sparse::SparseIndex;
use crate::world::{UnsafeWorldCell, World};

#[derive(Debug)]
pub struct Systems {
    sm: SlotMap<SystemInfo>,
    /// Maps global event indices to the ordered list of systems that handle the
    /// event.
    by_global_event: Vec<SystemList>,
    by_type_id: BTreeMap<TypeId, SystemInfoPtr>,
}

impl Systems {
    pub(crate) fn new() -> Self {
        Self {
            sm: SlotMap::new(),
            by_global_event: vec![],
            by_type_id: BTreeMap::new(),
        }
    }

    pub(crate) fn add(&mut self, info: SystemInfo) -> SystemId {
        let ptr = info.ptr();

        if let Some(type_id) = info.type_id() {
            assert!(self.by_type_id.insert(type_id, ptr).is_none());
        }

        let Some(k) = self.sm.insert_with(|k| {
            let id = SystemId(k);

            unsafe { (*ptr.as_ptr()).id = id };

            if let EventIdEnum::Global(id) = info.received_event().to_enum() {
                let idx = id.index().0 as usize;

                if idx >= self.by_global_event.len() {
                    self.by_global_event
                        .resize_with(idx + 1, SystemList::default);
                }

                self.by_global_event[idx].insert(ptr, info.priority())
            }

            info
        }) else {
            panic!("too many systems")
        };

        SystemId(k)
    }

    pub(crate) fn remove(&mut self, id: SystemId) -> Option<SystemInfo> {
        todo!()
    }

    pub(crate) fn register_event(&mut self, event_idx: EventIdx) {
        if let EventIdx::Global(GlobalEventIdx(idx)) = event_idx {
            if idx as usize >= self.by_global_event.len() {
                self.by_global_event
                    .resize_with(idx as usize + 1, SystemList::default);
            }
        }
    }

    pub(crate) fn get_global_list(&self, idx: GlobalEventIdx) -> Option<&SystemList> {
        self.by_global_event.get(idx.0 as usize)
    }

    pub fn get(&self, id: SystemId) -> Option<&SystemInfo> {
        self.sm.get(id.0)
    }

    pub(crate) fn get_mut(&mut self, id: SystemId) -> Option<&mut SystemInfo> {
        self.sm.get_mut(id.0)
    }

    pub fn by_index(&self, idx: SystemIdx) -> Option<&SystemInfo> {
        self.sm.by_index(idx.0).map(|(_, v)| v)
    }

    pub fn by_type_id(&self, id: TypeId) -> Option<&SystemInfo> {
        self.by_type_id
            .get(&id)
            .map(|p| unsafe { SystemInfo::ref_from_ptr(p) })
    }
}

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
        if let Some(info) = self.by_index(index) {
            info
        } else {
            panic!("no such system with index of {index:?} exists")
        }
    }
}

impl Index<TypeId> for Systems {
    type Output = SystemInfo;

    fn index(&self, index: TypeId) -> &Self::Output {
        if let Some(info) = self.by_type_id(index) {
            info
        } else {
            panic!("no such system with type ID of {index:?} exists")
        }
    }
}

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
    pub(crate) received_event: EventId,
    pub(crate) received_event_access: Access,
    pub(crate) entity_event_expr: BoolExpr<ComponentIdx>,
    pub(crate) sent_global_events: BitSet<GlobalEventIdx>,
    pub(crate) sent_entity_events: BitSet<EntityEventIdx>,
    pub(crate) event_queue_access: Access,
    pub(crate) reserve_entity_access: Access,
    pub(crate) component_access: ComponentAccessExpr,
    pub(crate) priority: Priority,
    pub(crate) id: SystemId,
    pub(crate) type_id: Option<TypeId>,
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

    pub fn name(&self) -> &str {
        unsafe { &(*self.inner.as_ptr()).name }
    }

    pub fn received_event(&self) -> EventId {
        // SAFETY: Type ensures inner pointer is valid.
        unsafe { (*self.inner.as_ptr()).received_event }
    }

    pub fn received_event_access(&self) -> Access {
        unsafe { (*self.inner.as_ptr()).received_event_access }
    }

    pub fn entity_event_expr(&self) -> Option<&BoolExpr<ComponentIdx>> {
        match self.received_event().to_enum() {
            EventIdEnum::Global(_) => None,
            EventIdEnum::Entity(_) => Some(unsafe { &(*self.inner.as_ptr()).entity_event_expr }),
        }
    }

    pub fn sent_global_events(&self) -> &BitSet<GlobalEventIdx> {
        unsafe { &(*self.inner.as_ptr()).sent_global_events }
    }

    pub fn sent_entity_events(&self) -> &BitSet<EntityEventIdx> {
        unsafe { &(*self.inner.as_ptr()).sent_entity_events }
    }

    pub fn event_queue_access(&self) -> Access {
        unsafe { (*self.inner.as_ptr()).event_queue_access }
    }

    pub fn reserve_entity_access(&self) -> Access {
        unsafe { (*self.inner.as_ptr()).reserve_entity_access }
    }

    pub fn component_access(&self) -> &ComponentAccessExpr {
        unsafe { &(*self.inner.as_ptr()).component_access }
    }

    pub fn priority(&self) -> Priority {
        // SAFETY: Type ensures inner pointer is valid.
        unsafe { (*self.inner.as_ptr()).priority }
    }

    pub fn id(&self) -> SystemId {
        // SAFETY: Type ensures inner pointer is valid.
        unsafe { (*self.inner.as_ptr()).id }
    }

    pub fn type_id(&self) -> Option<TypeId> {
        // SAFETY: Type ensures inner pointer is valid.
        unsafe { (*self.inner.as_ptr()).type_id }
    }

    pub(crate) fn ptr(&self) -> SystemInfoPtr {
        self.inner
    }

    /// # Safety
    ///
    /// Pointer must be valid.
    pub(crate) unsafe fn ref_from_ptr(this: &SystemInfoPtr) -> &Self {
        // SAFETY: `SystemInfo` is `#[repr(transparent)]`.
        &*(this as *const _ as *const Self)
    }

    pub(crate) fn system_mut(&mut self) -> &mut dyn System {
        unsafe { &mut (*self.inner.as_ptr()).system }
    }
}

unsafe impl Send for SystemInfo {}
unsafe impl Sync for SystemInfo {}

impl fmt::Debug for SystemInfo {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("SystemInfo")
            .field("name", &self.name())
            .field("received_event", &self.received_event())
            .field("received_event_access", &self.received_event_access())
            .field("entity_event_expr", &self.entity_event_expr())
            .field("sent_global_events", &self.sent_global_events())
            .field("sent_entity_events", &self.sent_entity_events())
            .field("event_queue_access", &self.event_queue_access())
            .field("reserve_entity_access", &self.reserve_entity_access())
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
    before_divider: u32,
    after_divider: u32,
    entries: Vec<SystemInfoPtr>,
}

impl SystemList {
    pub(crate) fn new() -> SystemList {
        Self::default()
    }

    pub fn insert(&mut self, ptr: SystemInfoPtr, priority: Priority) {
        assert!(self.entries.len() < u32::MAX as usize);

        match priority {
            Priority::Before => {
                self.entries.insert(self.before_divider as usize, ptr);
                self.before_divider += 1;
                self.after_divider += 1;
            }
            Priority::Normal => {
                self.entries.insert(self.after_divider as usize, ptr);
                self.after_divider += 1;
            }
            Priority::After => {
                self.entries.push(ptr);
            }
        }
    }

    pub fn remove(&mut self, ptr: SystemInfoPtr) -> bool {
        if let Some(idx) = self.entries.iter().position(|&p| p == ptr) {
            self.entries.remove(idx);

            let idx = idx as u32;

            if idx < self.after_divider {
                self.after_divider -= 1;

                if idx < self.before_divider {
                    self.before_divider -= 1;
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

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Default, Debug)]
pub struct SystemId(Key);

impl SystemId {
    pub const NULL: Self = Self(Key::NULL);

    pub const fn index(self) -> SystemIdx {
        SystemIdx(self.0.index())
    }
}

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

pub trait IntoSystem<Marker>: Sized {
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

    fn before(self) -> Before<Self::System> {
        Before(self.into_system())
    }

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

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Default, Debug)]
pub struct NoTypeId<S>(pub S);

unsafe impl<S: System> System for NoTypeId<S> {
    fn type_id(&self) -> Option<TypeId> {
        None
    }

    fn name(&self) -> Cow<'static, str> {
        self.0.name()
    }

    fn init(&mut self, world: &mut World, config: &mut Config) -> Result<(), InitError> {
        self.0.init(world, config)
    }

    unsafe fn run(&mut self, info: &SystemInfo, event_ptr: EventPtr, world: UnsafeWorldCell) {
        self.0.run(info, event_ptr, world)
    }

    unsafe fn refresh_archetype(
        &mut self,
        reason: RefreshArchetypeReason,
        arch: &Archetype,
    ) -> bool {
        self.0.refresh_archetype(reason, arch)
    }
}

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Default, Debug)]
pub struct Before<S>(pub S);

unsafe impl<S: System> System for Before<S> {
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

    unsafe fn run(&mut self, info: &SystemInfo, event_ptr: EventPtr, world: UnsafeWorldCell) {
        self.0.run(info, event_ptr, world)
    }

    unsafe fn refresh_archetype(
        &mut self,
        reason: RefreshArchetypeReason,
        arch: &Archetype,
    ) -> bool {
        self.0.refresh_archetype(reason, arch)
    }
}

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Default, Debug)]
pub struct After<S>(pub S);

unsafe impl<S: System> System for After<S> {
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

    unsafe fn run(&mut self, info: &SystemInfo, event_ptr: EventPtr, world: UnsafeWorldCell) {
        self.0.run(info, event_ptr, world)
    }

    unsafe fn refresh_archetype(
        &mut self,
        reason: RefreshArchetypeReason,
        arch: &Archetype,
    ) -> bool {
        self.0.refresh_archetype(reason, arch)
    }
}

pub unsafe trait System: Send + Sync + 'static {
    /// Returns the [`TypeId`] which uniquely identifies this system, or `None`
    /// if there is none.
    ///
    /// No two systems with the same [`TypeId`] will exist in the [`World`] at
    /// the same time.
    fn type_id(&self) -> Option<TypeId>;

    fn name(&self) -> Cow<'static, str>;

    fn init(&mut self, world: &mut World, config: &mut Config) -> Result<(), InitError>;

    unsafe fn run(&mut self, info: &SystemInfo, event_ptr: EventPtr, world: UnsafeWorldCell);

    unsafe fn refresh_archetype(
        &mut self,
        reason: RefreshArchetypeReason,
        arch: &Archetype,
    ) -> bool;
}

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum RefreshArchetypeReason {
    /// The archetype is newly created.
    New,
    /// The archetype columns were reallocated and any pointers to them are now
    /// invalid.
    InvalidColumns,
    /// The archetype previously had entities in it, but is now empty.
    Empty,
    /// The archetype was previously empty, but has gained at least one entity.
    Nonempty,
}

/// The error message is not stable and should not be used for distinguishing
/// between errors.
#[derive(Clone, Debug)]
pub struct InitError(pub Box<str>);

impl fmt::Display for InitError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", &self.0)
    }
}

#[cfg(feature = "std")]
impl std::error::Error for InitError {}

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Default, Debug)]
pub enum Priority {
    Before,
    #[default]
    Normal,
    After,
}

/// Configuration for a system, accessible during system initialization.
#[derive(Clone, Debug)]
#[non_exhaustive]
pub struct Config {
    pub priority: Priority,
    /// The event type to be received by the system. Is `None` when the value
    /// has not yet been assigned.
    pub received_event: Option<EventId>,
    pub received_event_access: Access,
    pub entity_event_expr: BoolExpr<ComponentIdx>,
    pub sent_global_events: BitSet<GlobalEventIdx>,
    pub sent_entity_events: BitSet<EntityEventIdx>,
    pub event_queue_access: Access,
    pub reserve_entity_access: Access,
    pub component_access: ComponentAccessExpr,
}

impl Config {
    pub fn new() -> Self {
        Self {
            priority: Default::default(),
            received_event: Default::default(),
            received_event_access: Default::default(),
            entity_event_expr: BoolExpr::one(),
            sent_global_events: Default::default(),
            sent_entity_events: Default::default(),
            event_queue_access: Default::default(),
            reserve_entity_access: Default::default(),
            component_access: Default::default(),
        }
    }
}

impl Default for Config {
    fn default() -> Self {
        Self::new()
    }
}

pub trait SystemParam {
    type State: Send + Sync + 'static;
    type Item<'a>: SystemParam<State = Self::State>;

    fn init(world: &mut World, config: &mut Config) -> Result<Self::State, InitError>;

    unsafe fn get_param<'a>(
        state: &'a mut Self::State,
        system_info: &'a SystemInfo,
        event_ptr: EventPtr<'a>,
        world: UnsafeWorldCell<'a>,
    ) -> Self::Item<'a>;

    unsafe fn refresh_archetype(
        state: &mut Self::State,
        reason: RefreshArchetypeReason,
        arch: &Archetype,
    ) -> bool {
        let _ = (state, reason, arch);
        false
    }
}

macro_rules! impl_system_param_tuple {
    ($(($P:ident, $s:ident)),*) => {
        impl<$($P: SystemParam),*> SystemParam for ($($P,)*) {
            type State = ($($P::State,)*);

            type Item<'a> = ($($P::Item<'a>,)*);

            #[inline]
            fn init(_world: &mut World, _config: &mut Config) -> Result<Self::State, InitError> {
                Ok((
                    $(
                        $P::init(_world, _config)?,
                    )*
                ))
            }

            #[inline]
            unsafe fn get_param<'a>(
                ($($s,)*): &'a mut Self::State,
                _info: &'a SystemInfo,
                _event_ptr: EventPtr<'a>,
                _world: UnsafeWorldCell<'a>,
            ) -> Self::Item<'a> {
                (
                    $(
                        $P::get_param($s, _info, _event_ptr, _world),
                    )*
                )
            }

            unsafe fn refresh_archetype(
                ($($s,)*): &mut Self::State,
                reason: RefreshArchetypeReason,
                arch: &Archetype
            ) -> bool
            {
                #[allow(unused_mut)]
                let mut res = false;

                $(
                    if $P::refresh_archetype($s, reason, arch) {
                        res = true;
                    }
                )*

                res
            }
        }
    }
}

all_tuples!(impl_system_param_tuple, 0, 15, P, s);

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

unsafe impl<Marker, F> System for FunctionSystem<Marker, F>
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
        world: UnsafeWorldCell,
    ) {
        let state = unsafe {
            self.state
                .as_mut()
                .expect_debug_checked("system must be initialized")
        };

        let param = <F::Param as SystemParam>::get_param(state, system_info, event_ptr, world);
        self.func.run(param);
    }

    unsafe fn refresh_archetype(
        &mut self,
        reason: RefreshArchetypeReason,
        arch: &Archetype,
    ) -> bool {
        let state = unsafe {
            self.state
                .as_mut()
                .expect_debug_checked("system must be initialized")
        };

        F::Param::refresh_archetype(state, reason, arch)
    }
}

/// This trait is sealed and cannot be implemented by downstream crates.
pub trait SystemParamFunction<Marker>: Send + Sync + 'static {
    type Param: SystemParam;

    unsafe fn run(&mut self, param: <Self::Param as SystemParam>::Item<'_>);
}

macro_rules! impl_system_param_function {
    ($(($P:ident, $p:ident)),*) => {
        impl<F, $($P: SystemParam),*> SystemParamFunction<fn($($P),*)> for F
        where
            F: FnMut($($P),*) + FnMut($($P::Item<'_>),*) + Send + Sync + 'static,
        {
            type Param = ($($P,)*);

            unsafe fn run(
                &mut self,
                ($($p,)*): <Self::Param as SystemParam>::Item<'_>
            ) {
                (self)($($p),*)
            }
        }
    }
}

all_tuples!(impl_system_param_function, 0, 15, P, p);

#[derive(Debug)]
pub struct Local<'a, T> {
    state: &'a mut T,
}

impl<T: Default + Send + 'static> SystemParam for Local<'_, T> {
    type State = Exclusive<T>;

    type Item<'a> = Local<'a, T>;

    fn init(_world: &mut World, _config: &mut Config) -> Result<Self::State, InitError> {
        Ok(Exclusive::new(T::default()))
    }

    unsafe fn get_param<'a>(
        state: &'a mut Self::State,
        _info: &'a SystemInfo,
        _event_ptr: EventPtr<'a>,
        _world: UnsafeWorldCell<'a>,
    ) -> Self::Item<'a> {
        Local {
            state: state.get_mut(),
        }
    }
}

impl<T> Deref for Local<'_, T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        self.state
    }
}

impl<T> DerefMut for Local<'_, T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.state
    }
}

/// Obtains the [`SystemInfo`] for the running system.
impl SystemParam for &'_ SystemInfo {
    type State = ();

    type Item<'a> = &'a SystemInfo;

    fn init(_world: &mut World, _config: &mut Config) -> Result<Self::State, InitError> {
        Ok(())
    }

    unsafe fn get_param<'a>(
        _state: &'a mut Self::State,
        info: &'a SystemInfo,
        _event_ptr: EventPtr<'a>,
        _world: UnsafeWorldCell<'a>,
    ) -> Self::Item<'a> {
        info
    }
}

#[cfg(feature = "std")]
impl<P: SystemParam> SystemParam for std::sync::Mutex<P> {
    type State = P::State;

    type Item<'a> = std::sync::Mutex<P::Item<'a>>;

    fn init(world: &mut World, config: &mut Config) -> Result<Self::State, InitError> {
        P::init(world, config)
    }

    unsafe fn get_param<'a>(
        state: &'a mut Self::State,
        system_info: &'a SystemInfo,
        event_ptr: EventPtr<'a>,
        world: UnsafeWorldCell<'a>,
    ) -> Self::Item<'a> {
        std::sync::Mutex::new(P::get_param(state, system_info, event_ptr, world))
    }
}

#[cfg(feature = "std")]
impl<P: SystemParam> SystemParam for std::sync::RwLock<P> {
    type State = P::State;

    type Item<'a> = std::sync::RwLock<P::Item<'a>>;

    fn init(world: &mut World, config: &mut Config) -> Result<Self::State, InitError> {
        P::init(world, config)
    }

    unsafe fn get_param<'a>(
        state: &'a mut Self::State,
        system_info: &'a SystemInfo,
        event_ptr: EventPtr<'a>,
        world: UnsafeWorldCell<'a>,
    ) -> Self::Item<'a> {
        std::sync::RwLock::new(P::get_param(state, system_info, event_ptr, world))
    }
}
