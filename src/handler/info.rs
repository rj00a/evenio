use core::any::TypeId;
use core::cmp::Ordering;
use core::fmt;
use core::hash::{Hash, Hasher};
use core::panic::{RefUnwindSafe, UnwindSafe};
use core::ptr::NonNull;

use alloc::borrow::Cow;

use crate::access::{Access, ComponentAccess};
use crate::aliased_box::AliasedBox;
use crate::archetype::Archetype;
use crate::bit_set::BitSet;
use crate::component::ComponentIdx;
use crate::entity::EntityLocation;
use crate::event::{EventId, EventPtr, TargetedEventIdx, UntargetedEventIdx};
use crate::world::{UnsafeWorldCell, World};

use super::{Handler, HandlerConfig, HandlerId, HandlerParam, HandlerPriority, InitError};

/// Metadata for a handler.
#[repr(transparent)]
pub struct HandlerInfo(AliasedBox<HandlerInfoInner>);

/// Pointer to a handler.
#[derive(Clone, Copy, Debug)]
#[repr(transparent)]
pub(crate) struct HandlerInfoPtr(pub(super) NonNull<HandlerInfoInner>);

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
    pub(crate) targeted_event_filter: ArchetypeFilter,
    pub(crate) sent_untargeted_events: BitSet<UntargetedEventIdx>,
    pub(crate) sent_targeted_events: BitSet<TargetedEventIdx>,
    pub(crate) event_queue_access: Access,
    pub(crate) component_access: ComponentAccess,
    pub(crate) archetype_filter: ArchetypeFilter,
    pub(crate) referenced_components: BitSet<ComponentIdx>,
    pub(crate) priority: HandlerPriority,
    // SAFETY: There is intentionally no public accessor for this field as it would lead to mutable
    // aliasing.
    pub(crate) handler: H,
}

impl<H: ?Sized> UnwindSafe for HandlerInfoInner<H> {}
impl<H: ?Sized> RefUnwindSafe for HandlerInfoInner<H> {}

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

    /// Gets the [`EventId`] of the event is handler listens for.
    pub fn received_event(&self) -> EventId {
        unsafe { (*AliasedBox::as_ptr(&self.0)).received_event }
    }

    /// Gets the handler's [`Access`] to the event it listens for.
    pub fn received_event_access(&self) -> Access {
        unsafe { (*AliasedBox::as_ptr(&self.0)).received_event_access }
    }

    /// Gets the expression describing the handler's targeted event query, or
    /// `None` if this handler is not targeted.
    pub fn targeted_event_filter(&self) -> Option<&ArchetypeFilter> {
        self.received_event()
            .is_targeted()
            .then(|| unsafe { &(*AliasedBox::as_ptr(&self.0)).targeted_event_filter })
    }

    /// Returns the set of untargeted events this handler sends.
    pub fn sent_untargeted_events(&self) -> &BitSet<UntargetedEventIdx> {
        unsafe { &(*AliasedBox::as_ptr(&self.0)).sent_untargeted_events }
    }

    /// Returns the set of targeted events this handler sends.
    pub fn sent_targeted_events(&self) -> &BitSet<TargetedEventIdx> {
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

    pub fn archetype_filter(&self) -> &ArchetypeFilter {
        unsafe { &(*AliasedBox::as_ptr(&self.0)).archetype_filter }
    }

    /// Gets the set of components referenced by this handler.
    ///
    /// Referenced components are components used by the handler in any way.
    /// Used for cleanup when removing components.
    pub fn referenced_components(&self) -> &BitSet<ComponentIdx> {
        unsafe { &(*AliasedBox::as_ptr(&self.0)).referenced_components }
    }

    /// Gets the [`Priority`] of this handler.
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
            .field("targeted_event_filter", &self.targeted_event_filter())
            .field("sent_untargeted_events", &self.sent_untargeted_events())
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

/// Obtains the [`HandlerInfo`] for the running handler.
unsafe impl HandlerParam for &'_ HandlerInfo {
    type State = ();

    type Item<'a> = &'a HandlerInfo;

    fn init(_world: &mut World, _config: &mut HandlerConfig) -> Result<Self::State, InitError> {
        Ok(())
    }

    unsafe fn get<'a>(
        _state: &'a mut Self::State,
        info: &'a HandlerInfo,
        _event_ptr: EventPtr<'a>,
        _target_location: EntityLocation,
        _world: UnsafeWorldCell<'a>,
    ) -> Self::Item<'a> {
        info
    }

    fn refresh_archetype(_state: &mut Self::State, _arch: &Archetype) {}

    fn remove_archetype(_state: &mut Self::State, _arch: &Archetype) {}
}
