use alloc::collections::BTreeMap;
use core::any::TypeId;
use core::fmt;
use core::ops::{Deref, DerefMut, Range};
use core::ptr::NonNull;

use evenio_macros::all_tuples;

use crate::access::SystemAccess;
use crate::archetype::{Archetype, ArchetypeIdx};
use crate::bit_set::{BitSet, SparseSetIndex};
use crate::debug_checked::{GetDebugChecked, UnwrapDebugChecked};
use crate::event::{EntityEventIdx, EventId, EventPtr, GlobalEventIdx, EventIdEnum};
use crate::exclusive::Exclusive;
use crate::slot_map::{Key, SlotMap};
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

    pub(crate) fn insert(&mut self, info: SystemInfo) -> SystemId {
        let ptr = info.ptr();

        if let EventIdEnum::Global(id) = info.received_event().to_enum() {
            if id.index().0 >= self.by_global_event.len() as u32 {
                self.by_global_event
                    .resize_with((id.index().0 + 1) as usize, SystemList::default);

                self.by_global_event[id.index().0 as usize].insert(ptr, info.priority())
            }
        }

        SystemId(self.sm.insert(info))
    }

    pub fn get(&self, id: SystemId) -> Option<&SystemInfo> {
        self.sm.get(id.0)
    }

    pub fn by_type_id(&self, id: TypeId) -> Option<&SystemInfo> {
        self.by_type_id
            .get(&id)
            .map(|p| unsafe { SystemInfo::ref_from_ptr(p) })
    }

    #[inline]
    pub(crate) unsafe fn systems_for_event_debug_checked(
        &mut self,
        idx: GlobalEventIdx,
    ) -> Range<*mut SystemInfoPtr> {
        self.by_global_event
            .get_debug_checked_mut(idx.0 as usize)
            .ptr_range_mut()
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

impl SystemInfo {
    pub(crate) fn new<S: System>(inner: SystemInfoInner<S>) -> Self {
        // SAFETY: `Box::into_raw` guarantees the returned pointer is non-null.
        let ptr = unsafe { NonNull::new(Box::into_raw(Box::new(inner))).unwrap_debug_checked() };

        Self { inner: ptr }
    }

    pub fn received_event(&self) -> EventId {
        // SAFETY: Type ensures inner pointer is valid.
        unsafe { (*self.inner.as_ptr()).received_event }
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
}

// This is generic over `S` so that we can do an unsizing coercion.
#[derive(Debug)]
pub(crate) struct SystemInfoInner<S: ?Sized = dyn System> {
    received_event: EventId,
    priority: Priority,
    access: SystemAccess,
    sent_global_events: BitSet<GlobalEventIdx>,
    sent_entity_events: BitSet<EntityEventIdx>,
    id: SystemId,
    type_id: Option<TypeId>,
    // SAFETY: There is intentionally no public accessor for this field as it would lead to mutable
    // aliasing.
    pub(crate) system: S,
}

unsafe impl Send for SystemInfo {}
unsafe impl Sync for SystemInfo {}

impl fmt::Debug for SystemInfo {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("SystemInfo")
            .field("received_event", &self.received_event())
            .field("priority", &self.priority())
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
struct SystemList {
    before_divider: u32,
    after_divider: u32,
    entries: Vec<SystemInfoPtr>,
}

impl SystemList {
    fn insert(&mut self, ptr: SystemInfoPtr, priority: Priority) {
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

        self.entries.insert(self.after_divider as usize, ptr);
        self.after_divider += 1;
    }

    fn remove(&mut self, ptr: SystemInfoPtr) -> bool {
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

    /// Returns a pointer range containing all the elements in this system list.
    ///
    /// For the purposes of the aliasing model, this function does not
    /// invalidate any existing pointers into the system list. Calling
    /// [`insert`](Self::insert), [`remove`](Self::remove), or dropping the
    /// system list will, however, invalidate any pointers derived from this
    /// function.
    #[inline]
    fn ptr_range_mut(&mut self) -> Range<*mut SystemInfoPtr> {
        // Don't use `as_mut_ptr_range` to avoid materializing a reference to the Vec's
        // slice.
        let start = self.entries.as_mut_ptr();
        // SAFETY: `end` points to one past the end of the buffer, which is in bounds.
        let end = unsafe { start.add(self.entries.len()) };
        Range { start, end }
    }
}

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Default, Debug)]
pub struct SystemId(Key);

impl SystemId {
    pub const NULL: Self = Self(Key::NULL);

    pub const fn index(self) -> SystemIdx {
        SystemIdx(self.0.index())
    }
}

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Debug)]
pub struct SystemIdx(pub u32);

impl SparseSetIndex for SystemIdx {
    fn index(self) -> usize {
        self.0 as usize
    }

    fn from_index(idx: usize) -> Self {
        Self(idx as u32)
    }
}

pub trait IntoSystem<Marker> {
    type System: System;

    fn into_system(self) -> Self::System;
}

impl<S: System, M> IntoSystem<M> for S {
    type System = Self;

    fn into_system(self) -> Self::System {
        self
    }
}

pub unsafe trait System: Send + Sync + 'static {
    /// Returns the [`TypeId`] which uniquely identifies this system, or `None`
    /// if there is none.
    ///
    /// No two systems with the same [`TypeId`] will exist in the [`World`] at
    /// the same time.
    fn type_id(&self) -> Option<TypeId> {
        None
    }

    fn init(&mut self, world: &mut World, config: &mut Config) -> Result<(), InitError>;

    unsafe fn run(&mut self, info: &SystemInfo, event_ptr: EventPtr, world: UnsafeWorldCell);

    unsafe fn refresh_archetype(
        &mut self,
        reason: RefreshArchetypeReason,
        idx: ArchetypeIdx,
        arch: &Archetype,
    ) -> bool;
}

#[derive(Clone, Copy, PartialEq, Eq)]
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
        write!(f, "failed to initialize system: {}", &self.0)
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
#[derive(Clone, Default, Debug)]
#[non_exhaustive]
pub struct Config {
    pub access: SystemAccess,
    pub priority: Priority,
    /// The event type to be received by the system. Is `None` when the value
    /// has not yet been assigned.
    pub received_event: Option<EventId>,
    pub sent_global_events: BitSet<GlobalEventIdx>,
    pub sent_entity_events: BitSet<EntityEventIdx>,
    pub is_parallel: bool,
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
        idx: ArchetypeIdx,
        arch: &Archetype,
    ) -> bool {
        let _ = (state, reason, idx, arch);
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
                idx: ArchetypeIdx,
                arch: &Archetype
            ) -> bool
            {
                #[allow(unused_mut)]
                let mut res = false;

                $(
                    if $P::refresh_archetype($s, reason, idx, arch) {
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

unsafe impl<Marker, F> System for FunctionSystem<Marker, F>
where
    F: SystemParamFunction<Marker>,
    Marker: 'static,
{
    fn type_id(&self) -> Option<TypeId> {
        Some(TypeId::of::<F>())
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
        idx: ArchetypeIdx,
        arch: &Archetype,
    ) -> bool {
        let state = unsafe {
            self.state
                .as_mut()
                .expect_debug_checked("system must be initialized")
        };

        F::Param::refresh_archetype(state, reason, idx, arch)
    }
}

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
