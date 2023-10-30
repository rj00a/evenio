use std::any::Any;
use std::borrow::Cow;
use std::error::Error;
use std::fmt;
use std::ops::{Deref, DerefMut};
use std::sync::{Mutex, RwLock};

use evenio_macros::all_tuples;
use slab::Slab;

use crate::event::EventId;
use crate::exclusive::Exclusive;
use crate::util::GetDebugChecked;
use crate::world::{FromWorld, SystemRunArgs, World};

#[derive(Debug)]
pub(crate) struct SystemRegistry {
    /// Maps event IDs to the list of all systems that handle the event.
    event_to_systems: Vec<SystemList>,
    /// Maps [`SystemId`]s to the system's location in `event_to_systems`.
    locations: Slab<SystemLocation>,
}

impl SystemRegistry {
    #[track_caller]
    pub(crate) fn add_system(&mut self, system: Box<dyn System>, mut info: SystemInfo) -> SystemId {
        if self.locations.len() >= (SystemId::NULL.0 - 1) as usize {
            panic!("too many systems added");
        }

        let system_id = SystemId(self.locations.insert(SystemLocation {
            event_id: info.event_id,
            index: u32::MAX,
        }) as u32);

        info.system_id = system_id;

        self.init_event(info.event_id);

        let list = &mut self.event_to_systems[info.event_id.0 as usize];

        let entry = SystemListEntry {
            system,
            info: Box::new(info),
        };

        let update_locations_start = match entry.info.priority {
            SystemPriority::Before => {
                list.entries.insert(list.before_index as usize, entry);
                list.before_index += 1;
                list.after_index += 1;
                list.before_index as usize - 1
            }
            SystemPriority::Normal => {
                list.entries.insert(list.after_index as usize, entry);
                list.after_index += 1;
                list.after_index as usize - 1
            }
            SystemPriority::After => {
                list.entries.push(entry);
                list.entries.len() - 1
            }
        };

        for list_idx in update_locations_start..list.entries.len() {
            let location_idx = list.entries[list_idx].info.system_id.0 as usize;
            self.locations[location_idx].index = list_idx as u32;
        }

        system_id
    }

    /// Ensures `event_id` is registered in the `event_to_systems` map.
    pub(crate) fn init_event(&mut self, event_id: EventId) {
        let event_idx = event_id.0 as usize;

        if event_idx >= self.event_to_systems.len() {
            self.event_to_systems.extend(
                (0..event_idx - self.event_to_systems.len() + 1).map(|_| Default::default()),
            );
        }
    }

    pub(crate) fn remove_system(&mut self, id: SystemId) -> Option<(Box<dyn System>, SystemInfo)> {
        self.locations.try_remove(id.0 as usize).map(|loc| {
            let list = &mut self.event_to_systems[loc.event_id.0 as usize];

            let entry = list.entries.remove(loc.index as usize);

            match entry.info.priority {
                SystemPriority::Before => {
                    list.before_index -= 1;
                    list.after_index -= 1;
                }
                SystemPriority::Normal => {
                    list.after_index -= 1;
                }
                SystemPriority::After => {}
            }

            for entry in &mut list.entries[loc.index as usize..] {
                self.locations[entry.info.system_id.0 as usize].index -= 1;
            }

            (entry.system, *entry.info)
        })
    }

    pub(crate) fn systems_for_event(
        &self,
        event_id: EventId,
    ) -> impl Iterator<Item = (&dyn System, &SystemInfo)> {
        let it = match self.event_to_systems.get(event_id.0 as usize) {
            Some(list) => list.entries.iter(),
            None => Default::default(),
        };

        it.map(|entry| (entry.system.as_ref(), entry.info.as_ref()))
    }

    pub(crate) fn systems_for_event_mut(
        &mut self,
        event_id: EventId,
    ) -> impl Iterator<Item = (&mut dyn System, &SystemInfo)> {
        let it = match self.event_to_systems.get_mut(event_id.0 as usize) {
            Some(list) => list.entries.iter_mut(),
            None => Default::default(),
        };

        it.map(|entry| (entry.system.as_mut(), entry.info.as_ref()))
    }

    #[inline]
    pub(crate) unsafe fn systems_for_event_unchecked_mut(
        &mut self,
        event_id: EventId,
    ) -> &mut [SystemListEntry] {
        &mut self
            .event_to_systems
            .get_debug_checked_mut(event_id.0 as usize)
            .entries
    }

    #[inline]
    pub(crate) fn system(&self, system_id: SystemId) -> Option<(&dyn System, &SystemInfo)> {
        self.locations.get(system_id.0 as usize).map(|loc| {
            let entry = &self.event_to_systems[loc.event_id.0 as usize].entries[loc.index as usize];
            (entry.system.as_ref(), entry.info.as_ref())
        })
    }

    #[inline]
    pub(crate) fn system_mut(
        &mut self,
        system_id: SystemId,
    ) -> Option<(&mut dyn System, &SystemInfo)> {
        self.locations.get_mut(system_id.0 as usize).map(|loc| {
            let entry =
                &mut self.event_to_systems[loc.event_id.0 as usize].entries[loc.index as usize];
            (entry.system.as_mut(), entry.info.as_ref())
        })
    }

    pub(crate) fn len(&self) -> usize {
        self.locations.len()
    }
}

impl SystemRegistry {
    pub(crate) fn new() -> Self {
        Self {
            event_to_systems: vec![],
            locations: Slab::new(),
        }
    }
}

#[derive(Debug, Default)]
struct SystemList {
    before_index: u32,
    after_index: u32,
    entries: Vec<SystemListEntry>,
}

#[derive(Debug)]
pub(crate) struct SystemListEntry {
    pub(crate) system: Box<dyn System>,
    pub(crate) info: Box<SystemInfo>,
}

#[derive(Debug)]
#[non_exhaustive]
pub struct SystemInfo {
    pub priority: SystemPriority,
    pub event_id: EventId,
    pub event_access: Option<Access>,
    pub system_id: SystemId,
}

impl SystemInfo {
    pub(crate) fn new() -> Self {
        Self {
            priority: SystemPriority::default(),
            event_id: EventId::NULL,
            event_access: None,
            system_id: SystemId::NULL,
        }
    }
}

/// The location of a system in [`Systems::event_to_systems`].
#[derive(Debug)]
struct SystemLocation {
    event_id: EventId,
    /// Index into the [`SystemList`].
    index: u32,
}

#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
pub struct SystemId(u32);

impl SystemId {
    pub const NULL: Self = Self(u32::MAX);

    pub fn to_bits(self) -> u64 {
        self.0 as u64
    }

    pub fn from_bits(bits: u64) -> Self {
        Self(bits as u32)
    }
}

impl Default for SystemId {
    fn default() -> Self {
        Self::NULL
    }
}

pub trait InitSystem<Marker> {
    type System: System;

    fn init_system(self, args: &mut SystemInitArgs<'_>) -> Result<Self::System, Box<dyn Error>>;
}

pub trait System: Send + Sync + Any + fmt::Debug {
    unsafe fn run(&mut self, args: SystemRunArgs<'_>);
}

#[derive(Debug)]
#[non_exhaustive]
pub struct SystemInitArgs<'a> {
    pub world: &'a mut World,
    pub name: Cow<'static, str>,
    pub priority: SystemPriority,
    pub event_id: EventId,
    pub event_access: Option<Access>,
    pub event_queue_access: Option<Access>,
}

impl<'a> SystemInitArgs<'a> {
    pub fn new(world: &'a mut World) -> Self {
        Self {
            world,
            name: Default::default(),
            priority: Default::default(),
            event_id: Default::default(),
            event_access: Default::default(),
            event_queue_access: Default::default(),
        }
    }
}

#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
pub enum Access {
    Read,
    ReadWrite,
}

#[derive(Clone, Debug)]
pub enum SystemInitError {
    InvalidEventId(EventId),
    ConflictingEventType,
    ConflictingEventAccess,
    ConflictingEventQueueAccess,
}

impl fmt::Display for SystemInitError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let msg = match self {
            SystemInitError::InvalidEventId(id) => {
                if *id == EventId::NULL {
                    "system did not specify an event to handle"
                } else {
                    "system specified an invalid event ID to handle"
                }
            }
            SystemInitError::ConflictingEventType => {
                "system parameters specify differing event types"
            }
            SystemInitError::ConflictingEventAccess => {
                "system parameters have conflicting access to the handled event"
            }
            SystemInitError::ConflictingEventQueueAccess => {
                "system parameters have conflicting access to the event queue"
            }
        };

        write!(f, "{msg}")
    }
}

impl Error for SystemInitError {}

/// Describes when a system will run relative to other systems that handle the
/// same event.
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Debug, Default)]
pub enum SystemPriority {
    Before,
    #[default]
    Normal,
    After,
}

pub trait SystemParam {
    type State: Send + Sync + 'static;
    type Item<'s, 'a>: SystemParam<State = Self::State>;

    fn init(args: &mut SystemInitArgs<'_>) -> Result<Self::State, Box<dyn Error>>;

    unsafe fn get_param<'s, 'a>(
        state: &'s mut Self::State,
        args: SystemRunArgs<'a>,
    ) -> Option<Self::Item<'s, 'a>>;
}

macro_rules! impl_system_param_tuple {
    ($(($P:ident, $s:ident)),*) => {
        impl<$($P: SystemParam),*> SystemParam for ($($P,)*) {
            type State = ($($P::State,)*);

            type Item<'s, 'a> = ($($P::Item<'s, 'a>,)*);

            #[inline]
            fn init(_args: &mut SystemInitArgs<'_>) -> Result<Self::State, Box<dyn Error>> {
                Ok((
                    $(
                        $P::init(_args)?,
                    )*
                ))
            }

            #[inline]
            unsafe fn get_param<'s, 'a>(
                ($($s,)*): &'s mut Self::State,
                _args: SystemRunArgs<'a>,
            ) -> Option<Self::Item<'s, 'a>> {
                Some((
                    $(
                        $P::get_param($s, _args)?,
                    )*
                ))
            }
        }
    }
}

all_tuples!(impl_system_param_tuple, 0, 15, P, s);

impl<Marker, F> InitSystem<Marker> for F
where
    F: SystemParamFunction<Marker>,
    <F::Param as SystemParam>::State: fmt::Debug,
    Marker: 'static,
{
    type System = FunctionSystem<Marker, F>;

    fn init_system(self, args: &mut SystemInitArgs<'_>) -> Result<Self::System, Box<dyn Error>> {
        Ok(FunctionSystem {
            func: self,
            state: <F::Param as SystemParam>::init(args)?,
        })
    }
}

pub struct FunctionSystem<Marker, F: SystemParamFunction<Marker>> {
    func: F,
    state: <F::Param as SystemParam>::State,
}

impl<Marker, F> FunctionSystem<Marker, F>
where
    F: SystemParamFunction<Marker>,
{
    pub fn into_inner(self) -> (F, <F::Param as SystemParam>::State) {
        (self.func, self.state)
    }
}

impl<Marker, F> System for FunctionSystem<Marker, F>
where
    F: SystemParamFunction<Marker>,
    <F::Param as SystemParam>::State: fmt::Debug,
    Marker: 'static,
{
    unsafe fn run(&mut self, args: SystemRunArgs<'_>) {
        if let Some(param) = <F::Param as SystemParam>::get_param(&mut self.state, args) {
            self.func.run(param);
        }
    }
}

impl<Marker, F> fmt::Debug for FunctionSystem<Marker, F>
where
    F: SystemParamFunction<Marker>,
    <F::Param as SystemParam>::State: fmt::Debug,
    Marker: 'static,
{
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.debug_struct("FunctionSystem")
            // Functions don't usually implement Debug, so use the type name instead.
            .field("func", &std::any::type_name::<F>())
            .field("state", &self.state)
            .finish()
    }
}

pub trait SystemParamFunction<Marker>: Send + Sync + 'static {
    type Param: SystemParam;

    unsafe fn run(&mut self, param: <Self::Param as SystemParam>::Item<'_, '_>);
}

macro_rules! impl_system_param_function {
    ($(($P:ident, $p:ident)),*) => {
        impl<F, $($P: SystemParam),*> SystemParamFunction<fn($($P),*)> for F
        where
            F: FnMut($($P),*) + FnMut($($P::Item<'_, '_>),*) + Send + Sync + 'static,
        {
            type Param = ($($P,)*);

            unsafe fn run(
                &mut self,
                ($($p,)*): <Self::Param as SystemParam>::Item<'_, '_>
            ) {
                (self)($($p),*)
            }
        }
    }
}

all_tuples!(impl_system_param_function, 0, 15, P, p);

#[derive(Debug)]
pub struct Local<'s, T>
where
    T: FromWorld + Send + 'static,
{
    state: &'s mut T,
}

impl<T: FromWorld + Send + 'static> SystemParam for Local<'_, T> {
    type State = Exclusive<T>;

    type Item<'s, 'a> = Local<'s, T>;

    fn init(args: &mut SystemInitArgs<'_>) -> Result<Self::State, Box<dyn Error>> {
        Ok(Exclusive::new(T::from_world(args.world)))
    }

    unsafe fn get_param<'s, 'a>(
        state: &'s mut Self::State,
        _args: SystemRunArgs<'a>,
    ) -> Option<Self::Item<'s, 'a>> {
        Some(Local {
            state: state.get_mut(),
        })
    }
}

impl<T: FromWorld + Send + 'static> Deref for Local<'_, T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        self.state
    }
}

impl<T: FromWorld + Send + 'static> DerefMut for Local<'_, T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.state
    }
}

impl SystemParam for SystemId {
    type State = ();

    type Item<'s, 'a> = SystemId;

    fn init(_args: &mut SystemInitArgs<'_>) -> Result<Self::State, Box<dyn Error>> {
        Ok(())
    }

    unsafe fn get_param<'s, 'a>(
        _state: &'s mut Self::State,
        args: SystemRunArgs<'a>,
    ) -> Option<Self::Item<'s, 'a>> {
        Some(args.system_info().system_id)
    }
}

impl SystemParam for &'_ SystemInfo {
    type State = ();

    type Item<'s, 'a> = &'a SystemInfo;

    fn init(_args: &mut SystemInitArgs<'_>) -> Result<Self::State, Box<dyn Error>> {
        Ok(())
    }

    unsafe fn get_param<'s, 'a>(
        _state: &'s mut Self::State,
        args: SystemRunArgs<'a>,
    ) -> Option<Self::Item<'s, 'a>> {
        Some(args.system_info())
    }
}

impl<P: SystemParam> SystemParam for Mutex<P> {
    type State = P::State;

    type Item<'s, 'a> = Mutex<P::Item<'s, 'a>>;

    fn init(args: &mut SystemInitArgs<'_>) -> Result<Self::State, Box<dyn Error>> {
        P::init(args)
    }

    unsafe fn get_param<'s, 'a>(
        state: &'s mut Self::State,
        args: SystemRunArgs<'a>,
    ) -> Option<Self::Item<'s, 'a>> {
        P::get_param(state, args).map(Mutex::new)
    }
}

impl<P: SystemParam> SystemParam for RwLock<P> {
    type State = P::State;

    type Item<'s, 'a> = RwLock<P::Item<'s, 'a>>;

    fn init(args: &mut SystemInitArgs<'_>) -> Result<Self::State, Box<dyn Error>> {
        P::init(args)
    }

    unsafe fn get_param<'s, 'a>(
        state: &'s mut Self::State,
        args: SystemRunArgs<'a>,
    ) -> Option<Self::Item<'s, 'a>> {
        P::get_param(state, args).map(RwLock::new)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[derive(Debug)]
    struct FakeSystem;

    impl System for FakeSystem {
        unsafe fn run(&mut self, _args: SystemRunArgs<'_>) {}
    }

    #[test]
    fn add_remove_system() {
        let mut systems = SystemRegistry::new();
        let event_id = EventId(1);

        let mut info_1 = SystemInfo::new();
        info_1.event_id = event_id;
        let id_1 = systems.add_system(Box::new(FakeSystem), info_1);

        let mut info_2 = SystemInfo::new();
        info_2.event_id = event_id;
        info_2.priority = SystemPriority::After;
        let id_2 = systems.add_system(Box::new(FakeSystem), info_2);

        let mut info_3 = SystemInfo::new();
        info_3.event_id = event_id;
        info_3.priority = SystemPriority::Before;
        let id_3 = systems.add_system(Box::new(FakeSystem), info_3);

        let mut it = systems
            .systems_for_event(event_id)
            .map(|(_, info)| info.system_id);
        assert_eq!(it.next(), Some(id_3));
        assert_eq!(it.next(), Some(id_1));
        assert_eq!(it.next(), Some(id_2));

        assert_eq!(
            systems.system(id_2).map(|(_, info)| info.system_id),
            Some(id_2)
        );

        drop(it);

        assert_eq!(
            systems.remove_system(id_3).map(|(_, info)| info.system_id),
            Some(id_3)
        );
        assert!(systems.remove_system(id_3).is_none());

        assert_eq!(
            systems.remove_system(id_1).map(|(_, info)| info.system_id),
            Some(id_1)
        );
        assert!(systems.remove_system(id_1).is_none());

        assert_eq!(
            systems.remove_system(id_2).map(|(_, info)| info.system_id),
            Some(id_2)
        );
        assert!(systems.remove_system(id_2).is_none());

        assert_eq!(systems.len(), 0);
    }
}
