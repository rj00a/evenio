use alloc::collections::BTreeMap;
use core::any::TypeId;
use core::ops::Index;

use super::list::HandlerList;
use super::{HandlerConfig, HandlerId, HandlerIdx, HandlerInfo, HandlerInfoPtr, InitError};
use crate::archetype::Archetype;
use crate::entity::EntityLocation;
use crate::event::{EventIdx, EventPtr, UntargetedEventIdx};
use crate::map::TypeIdMap;
use crate::slot_map::SlotMap;
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
/// # #[derive(Event)] struct E;
/// #
/// # let mut world = World::new();
/// world.add_handler(|_: Receiver<E>, handlers: &Handlers| {});
/// ```
#[derive(Debug)]
pub struct Handlers {
    infos: SlotMap<HandlerInfo>,
    /// Maps untargeted event indices to the ordered list of handlers that
    /// handle the event.
    by_untargeted_event: Vec<HandlerList>,
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
            by_untargeted_event: vec![],
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

            if let EventIdx::Untargeted(idx) = info.received_event().index() {
                let idx = idx.0 as usize;

                if idx >= self.by_untargeted_event.len() {
                    self.by_untargeted_event
                        .resize_with(idx + 1, HandlerList::default);
                }

                self.by_untargeted_event[idx].insert(ptr, info.priority())
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
                    .resize_with(idx as usize + 1, HandlerList::default);
            }
        }
    }

    pub(crate) fn get_untargeted_list(&self, idx: UntargetedEventIdx) -> Option<&HandlerList> {
        self.by_untargeted_event.get(idx.0 as usize)
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

unsafe impl Send for Handlers {}
unsafe impl Sync for Handlers {}

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

    type Item<'a> = &'a Handlers;

    fn init(_world: &mut World, _config: &mut HandlerConfig) -> Result<Self::State, InitError> {
        Ok(())
    }

    unsafe fn get<'a>(
        _state: &'a mut Self::State,
        _info: &'a HandlerInfo,
        _event_ptr: EventPtr<'a>,
        _target_location: EntityLocation,
        world: UnsafeWorldCell<'a>,
    ) -> Self::Item<'a> {
        world.handlers()
    }

    fn refresh_archetype(_state: &mut Self::State, _arch: &Archetype) {}

    fn remove_archetype(_state: &mut Self::State, _arch: &Archetype) {}
}
