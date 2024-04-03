use alloc::borrow::Cow;
use core::any::TypeId;

use super::{Handler, HandlerInfo, InitError};
use crate::access::{Access, ComponentAccess};
use crate::archetype::Archetype;
use crate::bit_set::BitSet;
use crate::component::ComponentIdx;
use crate::entity::EntityLocation;
use crate::event::{EventId, EventIdx, EventPtr, TargetedEventIdx, UntargetedEventIdx};
use crate::world::{UnsafeWorldCell, World};

/// The configuration of a handler. Accessible during handler initialization.
#[derive(Clone, Default, Debug)]
pub struct HandlerConfig {
    pub(crate) priority: HandlerPriority,
    pub(crate) received_event: ReceivedEventId,
    pub(crate) received_event_access: MaybeInvalidAccess,
    pub(crate) targeted_event_component_access: ComponentAccess,
    pub(crate) sent_untargeted_events: BitSet<UntargetedEventIdx>,
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

    pub fn set_received_event(&mut self, event: EventId) {
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

    /// Inserts an event type to the set of event types this handler is able to
    /// send.
    ///
    /// Returns whether the given event was already configured to be sent.
    pub fn insert_sent_event(&mut self, event: EventId) -> bool {
        match event.index() {
            EventIdx::Targeted(e) => self.sent_targeted_events.insert(e),
            EventIdx::Untargeted(e) => self.sent_untargeted_events.insert(e),
        }
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
    /// handler. The accesses are ANDed together in order to determine the
    /// component access of the whole handler.
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
pub struct High<S>(pub S);

impl<H: Handler> Handler for High<H> {
    fn type_id(&self) -> Option<TypeId> {
        self.0.type_id()
    }

    fn name(&self) -> Cow<'static, str> {
        self.0.name()
    }

    fn init(&mut self, world: &mut World, config: &mut HandlerConfig) -> Result<(), InitError> {
        let res = self.0.init(world, config);
        config.priority = HandlerPriority::High;
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
pub struct Low<S>(pub S);

impl<H: Handler> Handler for Low<H> {
    fn type_id(&self) -> Option<TypeId> {
        self.0.type_id()
    }

    fn name(&self) -> Cow<'static, str> {
        self.0.name()
    }

    fn init(&mut self, world: &mut World, config: &mut HandlerConfig) -> Result<(), InitError> {
        let res = self.0.init(world, config);
        config.priority = HandlerPriority::Low;
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
