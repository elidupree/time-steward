use serde::{de::DeserializeOwned, ser, Deserialize, Serialize};
use std::any::Any;
use std::fmt::Debug;
use std::hash::Hash;
use std::io::Read;
use std::ops::{Deref, DerefMut};

use crate::type_utils::list_of_types::ListOfTypes;
use crate::type_utils::{ChoiceOfObjectContainedIn, PersistentlyIdentifiedType};
use crate::EntityId;
use type_utils::GetContained;

/// Data used for a TimeSteward simulation, such as times, entities, and events.
///
/// We used to require `Send + Sync` for SimulationStateData, but now that EntityHandles can be part of SimulationStateData, we have to omit that to support TimeSteward types that have !Send/!Sync handles (like Rc)
///
/// Requiring DeserializeOwned is improper because you can't deserialize EntityHandles without more
///  information; the current approach is a hack where Deserialize
/// uses a thread-local context for that; it may later be replaced with a proper custom derive of my own.
pub trait SimulationStateData:
  Any + Clone + Eq + ser::Serialize + DeserializeOwned + Debug
{
}
impl<T: Any + Clone + Eq + ser::Serialize + DeserializeOwned + Debug> SimulationStateData for T {}

/// Data handles where clones point to the same data, and Eq and Hash by object identity, and Serialize by id.
pub trait EntityHandle: Clone + Ord + Hash + ser::Serialize + Debug {
  fn id(&self) -> EntityId;
}
pub trait TypedEntityHandle<E: EntityKind, H: EntityHandleKind>: EntityHandle {
  fn get_immutable(&self) -> &ImmutableData<E, H>
  where
    H: EntityHandleKindDeref;
}
pub trait OwnedEntityHandle: EntityHandle + SimulationStateData {}
impl<E: EntityHandle + SimulationStateData> OwnedEntityHandle for E {}
pub trait BorrowedEntityHandle: EntityHandle + Copy {}
impl<E: EntityHandle + Copy> BorrowedEntityHandle for E {}
pub trait OwnedTypedEntityHandle<E: EntityKind, H: EntityHandleKind>:
  OwnedEntityHandle + TypedEntityHandle<E, H>
{
  fn erase(self) -> DynHandle<H>;
  fn borrow(&self) -> TypedHandleRef<E, H>
  where
    H: EntityHandleKindDeref;
  fn raw_read<'a, A: Accessor<EntityHandleKind = H>>(
    &'a self,
    accessor: &'a A,
  ) -> A::ReadGuard<'a, MutableData<E, A::EntityHandleKind>>
  where
    H: EntityHandleKindDeref,
  {
    accessor.raw_read(self.borrow())
  }
}
pub trait OwnedDynEntityHandle<H: EntityHandleKind>: OwnedEntityHandle {
  // fn downcast<E: EntityKind>(self) -> Result<TypedHandle<E, H>, Self>
  // where
  //   H: EntityHandleKindDeref;
  fn borrow(&self) -> DynHandleRef<H>
  where
    H: EntityHandleKindDeref;
}
pub trait BorrowedTypedEntityHandle<'a, E: EntityKind, H: EntityHandleKindDeref>:
  BorrowedEntityHandle + TypedEntityHandle<E, H>
{
  fn erase(self) -> DynHandleRef<'a, H>;
  fn to_owned(self) -> TypedHandle<E, H>;
  fn query<A: Accessor<EntityHandleKind = H>>(
    self,
    accessor: &mut A,
  ) -> A::ReadGuard<'a, MutableData<E, A::EntityHandleKind>>
  where
    H: EntityHandleKindDeref,
  {
    todo!() //accessor.query(self)
  }
}
pub trait BorrowedDynEntityHandle<'a, H: EntityHandleKindDeref>: BorrowedEntityHandle {
  // fn downcast<E: EntityKind>(self) -> Option<TypedHandleRef<'a, E, H>>;
  // fn to_owned(self) -> DynHandle<H>;
}

pub trait EntityKind: Any + Sized + PersistentlyIdentifiedType {
  type ImmutableData: SimulationStateDataKind;
  type MutableData: SimulationStateDataKind;
}

pub type ImmutableData<E, H> =
  <<E as EntityKind>::ImmutableData as SimulationStateDataKind>::Data<H>;
pub type MutableData<E, H> = <<E as EntityKind>::MutableData as SimulationStateDataKind>::Data<H>;
pub type Globals<S, H> = <<S as SimulationSpec>::Globals as SimulationStateDataKind>::Data<H>;

pub trait EntityHandleKind: Sized {
  type TypedHandle<E: EntityKind>: OwnedTypedEntityHandle<E, Self>;
  type DynHandle: OwnedDynEntityHandle<Self>;
}
pub trait EntityHandleKindDeref: EntityHandleKind {
  type TypedHandleRef<'a, E: EntityKind>: BorrowedTypedEntityHandle<'a, E, Self>;
  type DynHandleRef<'a>: BorrowedDynEntityHandle<'a, Self>;
}

pub type TypedHandle<E, H> = <H as EntityHandleKind>::TypedHandle<E>;
pub type DynHandle<H> = <H as EntityHandleKind>::DynHandle;
pub type TypedHandleRef<'a, E, H> = <H as EntityHandleKindDeref>::TypedHandleRef<'a, E>;
pub type DynHandleRef<'a, H> = <H as EntityHandleKindDeref>::DynHandleRef<'a>;

pub trait SimulationStateDataKind {
  type Data<E: EntityHandleKind>: SimulationStateData;
}

// Model: events interact with the physics only through queries at their exact time (which are forbidden to query other timelines or have any side effects) and modifications at their exact time (which are forbidden to return any information). Those modifications, in practice, change the state *going forward from* that time.

/**
This is intended to be implemented on an empty struct.
*/
pub trait SimulationSpec: Any {
  type Time: SimulationStateData + Send + Sync + Ord;
  type Globals: SimulationStateDataKind;
  type Types: ListOfTypes;
}

pub trait ConstructGlobals<S: SimulationSpec> {
  fn construct_globals<Accessor: GlobalsConstructionAccessor<SimulationSpec = S>>(
    self,
    accessor: &mut Accessor,
  ) -> Globals<S, Accessor::EntityHandleKind>;
}

pub trait Wake<S: SimulationSpec>: EntityKind {
  fn wake<Accessor: EventAccessor<SimulationSpec = S>>(
    accessor: &mut Accessor,
    this: TypedHandleRef<Self, Accessor::EntityHandleKind>,
  );
}

#[derive(Copy, Clone, PartialEq, Eq, Debug)]
pub enum FiatEventOperationError {
  InvalidInput,
  InvalidTime,
}

// This exists to support a variety of time stewards
// along with allowing BaseTime to be dense (e.g. a
// rational number rather than an integer).
// It is an acceptable peculiarity that even for integer times,
// After(2) < Before(3).
// #[derive (Copy, Clone, PartialEq, Eq, Hash)]
#[derive(Clone, PartialEq, Eq, Hash, Serialize, Deserialize, Debug)]
pub enum ValidSince<BaseTime> {
  TheBeginning,
  Before(BaseTime),
  After(BaseTime),
}

/**
A view into a TimeSteward simulation, allowing immutable access to entities.
*/
pub trait Accessor {
  type SimulationSpec: SimulationSpec;
  type EntityHandleKind: EntityHandleKindDeref;

  /// The guard type returned by `raw_read`. In EventAccessors for optimized TimeStewards, you can assume that this is equivalent to `&'a T`. Simpler implementations may use std::cell::Ref.
  type ReadGuard<'a, T: 'a>: Deref<Target = T>;

  /**
  Raw read access for entities, perhaps simply taking a reference to the entity contents. This is not undo-safe, but is a building block for building undo-safe wrappers.
  
  This requires `&'a self` as well as `'a` entity, so that we can statically guarantee that there is no overlap with mutable access to entities. As a result, you can't hold a reference to *any* entity while any other entity is being modified. This restriction could theoretically be relaxed, but it is the simplest way uphold "shared XOR mutable" without runtime checks.
  */
  fn raw_read<'a, 'b: 'a, E: EntityKind>(
    &'a self,
    // at the time of this writing, we cannot use the type alias TypedHandleRef due to
    // https://github.com/rust-lang/rust/issues/85533
    entity: <Self::EntityHandleKind as EntityHandleKindDeref>::TypedHandleRef<'b, E>,
  ) -> Self::ReadGuard<'a, MutableData<E, Self::EntityHandleKind>>;
  
  /**
  Store a record that the current event (if any) accessed this entity.
  
  `raw_read` and `raw_read_schedule` are undo-safe if `record_read` is called for the same entity at any time within the same event, regardless of which call happened first.
  
  This is a no-op in accessors that are not EventAccessors, but it's part of this trait so that code can be generic over whether it is happening within an event or not.
  
  `record_read` is always undo-safe to call. The only downside of false-positives is a runtime cost.
  */
  fn record_read<E: EntityKind>(
    &self,
    // at the time of this writing, we cannot use the type alias TypedHandleRef due to
    // https://github.com/rust-lang/rust/issues/85533
    entity: <Self::EntityHandleKind as EntityHandleKindDeref>::TypedHandleRef<'_, E>,
  ) {
  }

  /**
  Raw read access for an entity's current schedule, perhaps simply copying the value. This is not undo-safe, but is a building block for building undo-safe wrappers.
  */
  fn raw_read_schedule<E: Wake<Self::SimulationSpec>>(
    &self,
    entity: TypedHandleRef<E, Self::EntityHandleKind>,
  ) -> Option<<Self::SimulationSpec as SimulationSpec>::Time>;
}

/**
An Accessor that exists at a specific time, after the globals have been constructed.

Most Accessors are InitializedAccessors. The exception are GlobalsConstructionAccessors, which are used to construct the globals and thus exist before the globals are available, and don't happen at a specific time.
*/
pub trait InitializedAccessor: Accessor {
  fn globals(&self) -> &Globals<Self::SimulationSpec, Self::EntityHandleKind>;
  fn now(&self) -> &<Self::SimulationSpec as SimulationSpec>::Time;
}

/**
An Accessor that can create entities.
*/
pub trait CreateEntityAccessor: Accessor {
  /**
  Create a new entity with the given initial data.
  
  Newly created entities are given a unique id by the system.
  
  Newly created entities are not scheduled to wake yet, but can be scheduled later with `EventAccessor::schedule`.
  */
  fn create_entity<E: EntityKind>(
    &mut self,
    immutable: ImmutableData<E, Self::EntityHandleKind>,
    mutable: MutableData<E, Self::EntityHandleKind>,
  ) -> TypedHandle<E, Self::EntityHandleKind>;
}

/**
An Accessor with all abilities that can be used during the construction of the globals. This allows creating entities, but currently does not allow scheduling them.
*/
pub trait GlobalsConstructionAccessor: CreateEntityAccessor {}

pub trait Modify<T>: SimulationStateData {
  type UndoData: SimulationStateData;
  fn modify(self, entity: &mut T) -> Self::UndoData;
  fn undo(entity: &mut T, undo_data: &Self::UndoData);
}

#[derive(Clone, PartialEq, Eq, Hash, Serialize, Deserialize, Debug, Default)]
pub struct ReplaceWith<T>(pub T);

/**
An Accessor with all abilities that can be used during an event.

EventAccessors can create and modify entities, and modify entity schedules.
*/
pub trait EventAccessor: InitializedAccessor + CreateEntityAccessor {
  /// The guard type returned by `raw_write`. In EventAccessors for optimized TimeStewards, you can assume that this is equivalent to `&'a mut T`. Simpler implementations may use `std::cell::RefMut`.
  type WriteGuard<'a, T: 'a>: DerefMut<Target = T>;
  
  /// Make a new WriteGuard for a component of the borrowed data, similar to `std::cell::RefMut::map`. If the WriteGuard type is `&mut T`, this is a trivial application of `f`.
  fn map_write_guard<'a, T, U>(
    guard: Self::WriteGuard<'a, T>,
    f: impl FnOnce(&mut T) -> &mut U,
  ) -> Self::WriteGuard<'a, U>;
  
  /**
  Raw write access for entities, perhaps simply taking a mutable reference to the entity contents. This is not undo-safe, but is a building block for building undo-safe wrappers.
  
  This requires `&'a mut self` as well as `'a` entity, so that we can statically guarantee that there is no overlap with shared access to entities. As a result, you can't hold a reference to *any* entity while any other entity is being modified. This restriction could theoretically be relaxed, but it is the simplest way uphold "shared XOR mutable" without runtime checks.
  */
  fn raw_write<'a, 'b: 'a, E: EntityKind>(
    &'a mut self,
    // at the time of this writing, we cannot use the type alias TypedHandleRef due to
    // https://github.com/rust-lang/rust/issues/85533
    entity: <Self::EntityHandleKind as EntityHandleKindDeref>::TypedHandleRef<'b, E>,
  ) -> Self::WriteGuard<'a, MutableData<E, Self::EntityHandleKind>>;
  
  /**
  Store a record of how to undo a modification we made to this entity in this event. This is not undo-safe, but is a building block for building undo-safe wrappers.
  
  `raw_write` and `record_undo` are undo-safe if, at the end of the event, running all of the undo functions for this entity in reverse order leaves you with the same mutable data the entity started with at the beginning of the event. The simplest way to fulfill this condition is to begin by copying the original state of the entity and recording an undo function that overwrites the entity with a copy of that copy.
  
  Any call to `record_undo` also makes `Accessor::raw_read` undo-safe in the same way as `Accessor::record_read` does.
  */
  fn record_undo<E: EntityKind>(
    &self,
    // at the time of this writing, we cannot use the type alias TypedHandleRef due to
    // https://github.com/rust-lang/rust/issues/85533
    entity: <Self::EntityHandleKind as EntityHandleKindDeref>::TypedHandleRef<'_, E>,
    undo: impl Fn(&mut MutableData<E, Self::EntityHandleKind>) + 'static,
  );
  
  /**
  Schedule this entity to wake at the given time.
  
  This is equivalent to `set_schedule` with Some as the argument.
  */
  fn schedule<E: Wake<Self::SimulationSpec>>(
    &mut self,
    entity: TypedHandleRef<E, Self::EntityHandleKind>,
    time: <Self::SimulationSpec as SimulationSpec>::Time,
  ) {
    self.set_schedule(entity, Some(time))
  }
  
  /**
  Cancel any current scheduled for this entity to wake.
  
  This is equivalent to `set_schedule` with None as the argument.
  */
  fn unschedule<E: Wake<Self::SimulationSpec>>(
    &mut self,
    entity: TypedHandleRef<E, Self::EntityHandleKind>,
  ) {
    self.set_schedule(entity, None)
  }

  /**
  Schedule this entity to wake at the given time.
  
  This is always undo-safe, and always has some runtime overhead. (Unlike with `raw_write`, it's not worth complicating the API to optimize this. Any such optimization would be tiny at best.)
  
  Any call to `set_schedule` also makes `Accessor::raw_read` undo-safe in the same way as `Accessor::record_read` does.
  */
  fn set_schedule<E: Wake<Self::SimulationSpec>>(
    &mut self,
    entity: TypedHandleRef<E, Self::EntityHandleKind>,
    time: Option<<Self::SimulationSpec as SimulationSpec>::Time>,
  ) {
    match time {
      None => self.unschedule(entity),
      Some(time) => self.schedule(entity, time),
    }
  }
}

/**
An Accessor representing a snapshot of a complete simulation state.
*/
pub trait SnapshotAccessor: InitializedAccessor {
  /// The iterator type returned by `scheduled_events`.
  type ScheduledEvents<'a>: Iterator<Item = DynHandle<Self::EntityHandleKind>> + 'a;
  
  /**
  Take an iterator over all entities that are currently scheduled to wake in the future.
  
  Library users usually won't call this explicitly, but it's necessary for serializing a complete snapshot, because it's possible for there to be scheduled entities that are not accessible the following other entity handles starting at the globals. The globals and scheduled_events taken together provide all of the "roots" from which other entities can be observed.
  */
  fn scheduled_events(&self) -> Self::ScheduledEvents<'_>;
}

/**
The core TimeSteward trait: An object that can run simulations.
*/
pub trait TimeSteward: Any + Sized + Debug {
  /**
  The SimulationSpec this TimeSteward is for.
  
  A typical TimeSteward implementor will be generic over all possible SimulationSpecs, but the concrete type is associated with a specific one.
  */
  type SimulationSpec: SimulationSpec;
  
  /// The kind of EntityHandles used by this TimeSteward.
  type EntityHandleKind: EntityHandleKindDeref;
  
  /// The Accessor type returned by `snapshot_before`.
  type SnapshotAccessor: SnapshotAccessor<
    SimulationSpec = Self::SimulationSpec,
    EntityHandleKind = Self::EntityHandleKind,
  >;
  
  /**
  Create an event by fiat.

  A fiat event is an entity that is postulated to have existed since the beginning of the simulation, and to have always been scheduled to wake at the given time.
  
  (TODO: notes about what times you can alter fiat events at)
  
  Each fiat event must have a unique id. These ids must be globally unique across all fiat events that exist within the TimeSteward. Attempting to insert a second fiat event with the same id will return `Err(FiatEventOperationError::InvalidInput)`.
  
  Fiat event ids are also forbidden from colliding with the ids of other entities within the simulation. This isn't something you normally have to worry about, because the ids of other entities are generated by hashing with keys internal to `time_steward`, so ids will only collide if you explicitly use those keys or copy the id of an entity that was generated that way. However, it is trivial to construct a colliding id maliciously, and TimeSteward implementors are not required to detect this situation (meaning a malicious id may result in incorrect behavior or panics rather than a simple Err). Thus, simulations that accept untrusted inputs over a network may not accept arbitrary ids, but must require inputs that are used to *generate* ids.
  
  A fiat event cannot contain handles to any other entity. (Thus, when it wakes, it can only act upon entities accessible through the simulation globals.) This isn't a specific restriction on fiat events, but simply a consequence of the fact that it is a logic error to use any entity handle outside of the Accessor you received it from.
  */
  fn insert_fiat_event<E: Wake<Self::SimulationSpec>>(
    &mut self,
    time: <Self::SimulationSpec as SimulationSpec>::Time,
    id: EntityId,
    immutable: ImmutableData<E, Self::EntityHandleKind>,
    mutable: MutableData<E, Self::EntityHandleKind>,
  ) -> Result<(), FiatEventOperationError>;  
  
  /**
  Remove a fiat event that was previously created.
  
  After removing a fiat event, you can create another with the same id. Attempting to remove a fiat event that doesn't currently exist will return `Err(FiatEventOperationError::InvalidInput)`.
    
  (TODO: notes about what times you can alter fiat events at)
  
  */
  fn remove_fiat_event(
    &mut self,
    time: &<Self::SimulationSpec as SimulationSpec>::Time,
    id: EntityId,
  ) -> Result<(), FiatEventOperationError>;
  fn snapshot_before(
    &mut self,
    time: <Self::SimulationSpec as SimulationSpec>::Time,
  ) -> Option<Self::SnapshotAccessor>;

  fn valid_since(&self) -> ValidSince<<Self::SimulationSpec as SimulationSpec>::Time>;
  fn forget_before(&mut self, time: &<Self::SimulationSpec as SimulationSpec>::Time);
}

/// A trait for TimeSteward types that can be initialized from just the initial physics data.
/// Most TimeSteward types should implement ConstructibleTimeSteward<()>. Exceptions are types that can't function without certain extra runtime metadata
pub trait ConstructibleTimeSteward<Metadata = ()>: TimeSteward {
  fn from_globals(
    metadata: Metadata,
    globals: Globals<Self::SimulationSpec, Self::EntityHandleKind>,
  ) -> Self;

  fn from_construct_globals<G: ConstructGlobals<Self::SimulationSpec>>(
    metadata: Metadata,
    construct_globals: G,
  ) -> Self;

  fn from_serialized<R: Read>(metadata: Metadata, data: &mut R) -> ::bincode::Result<Self>;
}

pub trait IncrementalTimeSteward: TimeSteward {
  fn step(&mut self);
  fn updated_until_before(&self) -> Option<<Self::SimulationSpec as SimulationSpec>::Time>;
}

/// A marker trait indicating a TimeSteward that implements the canonical behavior.
///
/// All CanonicalTimeStewards must behave the same way given the same inputs.
/// Non-canonical TimeStewards use the same API, but may behave differently.
///
/// The motivating example of a non-canonical TimeSteward is a wrapper that
/// shares fiat events across the network.
/// Like other TimeStewards, you interact with it by inserting fiat events,
/// taking snapshots, etc – but it's doesn't behave according to
/// just the fiat events you put in, but also fiat events it received
/// from remote clients.
pub trait CanonicalTimeSteward: TimeSteward {}
