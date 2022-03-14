use serde::{de::DeserializeOwned, Serialize};
use std::any::Any;
use std::fmt::Debug;
use std::io::Read;
use std::ops::{Deref, DerefMut};

use crate::type_utils::list_of_types::ListOfTypes;
use crate::{
  DynHandle, EntityHandleKind, EntityHandleKindDeref, EntityId, EntityKind, Globals, ImmutableData,
  MutableData, TypedHandle, TypedHandleRef,
};

/// Data used for a TimeSteward simulation, such as times, entities, and events.
///
/// We used to require `Send + Sync` for SimulationStateData, but now that EntityHandles can be part of SimulationStateData, we have to omit that to support TimeSteward types that have !Send/!Sync handles (like Rc)
///
/// Requiring DeserializeOwned is improper because you can't deserialize EntityHandles without more
///  information; the current approach is a hack where Deserialize
/// uses a thread-local context for that; it may later be replaced with a proper custom derive of my own.
pub trait SimulationStateData: Any + Clone + Eq + Serialize + DeserializeOwned + Debug {}
impl<T: Any + Clone + Eq + Serialize + DeserializeOwned + Debug> SimulationStateData for T {}

// Model: events interact with the physics only through queries at their exact time (which are forbidden to query other timelines or have any side effects) and modifications at their exact time (which are forbidden to return any information). Those modifications, in practice, change the state *going forward from* that time.

/**
This is intended to be implemented on an empty struct.
*/
pub trait SimulationSpec: Any {
  type Time: SimulationStateData + Send + Sync + Ord;
  type Globals<E: EntityHandleKind>: SimulationStateData;
  type Types: ListOfTypes;
}

pub trait ConstructGlobals<S: SimulationSpec> {
  fn construct_globals<A: GlobalsConstructionAccessor<SimulationSpec = S>>(
    self,
    accessor: &mut A,
  ) -> Globals<S, A::EntityHandleKind>;
}

pub trait Wake<S: SimulationSpec>: EntityKind {
  fn wake<'a, A: EventAccessor<'a, SimulationSpec = S>>(
    accessor: &mut A,
    this: TypedHandleRef<Self, A::EntityHandleKind>,
  );
}

#[derive(Copy, Clone, PartialEq, Eq, Debug)]
pub enum FiatEventOperationError {
  InvalidInput,
  InvalidTime,
}

/**
A view into a TimeSteward simulation, allowing immutable access to entities.
*/
pub trait Accessor {
  type SimulationSpec: SimulationSpec;
  type EntityHandleKind: EntityHandleKindDeref;

  /// The guard type returned by `raw_read`. In EventAccessors for optimized TimeStewards, you can assume that this is equivalent to `&'a T`. Simpler implementations may use std::cell::Ref.
  type ReadGuard<'a, T: 'a>: Deref<Target = T>
  where
    Self: 'a;

  /**
  Raw read access for entities, perhaps simply taking a reference to the entity contents. This is not undo-safe, but is a building block for building undo-safe wrappers.

  This requires `&'a self` as well as `'a` entity, so that we can statically guarantee that there is no overlap with mutable access to entities. As a result, you can't hold a reference to *any* entity while any other entity is being modified. This restriction could theoretically be relaxed, but it is the simplest way uphold "shared XOR mutable" without runtime checks.

  `raw_read` is undo-safe if **any** of the following conditions are met:
  * You call `record_read` or `record_undo` on the same entity at any time within the same event, regardless of which call happened first.
  * The data you read from the entity never influences any entity changes (for example, if it's only used for assertions or logging).
  */
  fn raw_read<'a, E: EntityKind>(
    &'a self,
    entity: TypedHandleRef<'a, E, Self::EntityHandleKind>,
  ) -> Self::ReadGuard<'a, MutableData<E, Self::EntityHandleKind>>;

  /**
  Store a record that the current event (if any) accessed this entity.

  `raw_read` and `raw_read_schedule` are undo-safe if `record_read` is called for the same entity at any time within the same event, regardless of which call happened first.

  This is a no-op in accessors that are not EventAccessors, but it's part of this trait so that code can be generic over whether it is happening within an event or not.

  `record_read` is always undo-safe to call. The only downside of false-positives is a runtime cost.
  */
  #[allow(unused)]
  fn record_read<E: EntityKind>(&self, entity: TypedHandleRef<E, Self::EntityHandleKind>) {}

  /**
  Raw read access for an entity's current schedule, perhaps simply copying the value. This is not undo-safe, but is a building block for building undo-safe wrappers.

  `raw_read_schedule` is undo-safe in the same situations as `raw_read`.
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
pub trait InitializedAccessor<'acc>: Accessor {
  fn globals(&self) -> &'acc Globals<Self::SimulationSpec, Self::EntityHandleKind>;
  fn now(&self) -> &'acc <Self::SimulationSpec as SimulationSpec>::Time;
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

pub trait PerformUndo<T>: 'static {
  type UndoData: DeserializeOwned;
  /**
  Perform the undo operation, using the deserialized data.

  See `RecordUndo` and `EventAccessor::record_undo` for more details about how to use this.
  */
  fn perform_undo(data: &mut T, undo_data: Self::UndoData);
}

/**
An object that can record undo data.

See `EventAccessor::record_undo` for more details.
*/
pub trait RecordUndo<T> {
  /**
  Record a single undo operation.

  `undo_data` will be serialized and stored somewhere. If and when `perform_undo` is called, it will be given a deserializer containing the same data.

  For performance, we expect that a large majority of the time, `perform_undo` will never be called, so it's okay to make `perform_undo` somewhat expensive in order to minimize the cost of serializing and storing `undo_data`. You can expect that it's being serialized into a raw byte buffer.

  See `EventAccessor::record_undo` for more details about when you should call this.
  */
  fn record_undo<S: Serialize, P: PerformUndo<T>>(&self, undo_data: &S);
}

/**
An Accessor with all abilities that can be used during an event.

EventAccessors can create and modify entities, and modify entity schedules.
*/
pub trait EventAccessor<'acc>: InitializedAccessor<'acc> + CreateEntityAccessor {
  /// The guard type returned by `raw_write`. In EventAccessors for optimized TimeStewards, you can assume that this is equivalent to `&'a mut T`. Simpler implementations may use `std::cell::RefMut`.
  type WriteGuard<'a, T: 'a>: DerefMut<Target = T>;
  type UndoRecorder<'a, T: SimulationStateData>: RecordUndo<T> + Clone
  where
    Self: 'a;

  /// Make a new WriteGuard for a component of the borrowed data, similar to `std::cell::RefMut::map`. If the WriteGuard type is `&mut T`, this is a trivial application of `f`.
  #[allow(clippy::needless_lifetimes)] // Clippy is currently wrong about GATs
  fn map_write_guard<'a, T, U>(
    guard: Self::WriteGuard<'a, T>,
    f: impl FnOnce(&mut T) -> &mut U,
  ) -> Self::WriteGuard<'a, U>;

  /**
  Raw write access for entities. This is not undo-safe, but is a building block for building undo-safe wrappers.

  This requires `&'a mut self` as well as `'a` entity, so that we can statically guarantee that there is no overlap with shared access to entities. As a result, you can't hold a reference to *any* entity while any other entity is being modified. This restriction could theoretically be relaxed, but it is the simplest way uphold "shared XOR mutable" without runtime checks.

  Undo safety is accomplished by calling `record_undo` on the returned `UndoRecorder` (see the `RecordUndo` trait). `raw_write` is undo-safe if, after you drop the WriteGuard and UndoRecorder, the undo operations you recorded exactly revert the changes you made. That is, running all the `perform_undo` functions you recorded, in reverse order, on the entity's mutable data, leaves it in the same state that it was in at the start of the `raw_write` call.

  This condition is typically fulfilled through a wrapper that allows you to perform a sequence of individual mutating operations while recording the undo data for each operation as it is performed.

  Any call to `record_undo` also makes `Accessor::raw_read` undo-safe in the same way as `Accessor::record_read` does. If a write makes *no* changes, it doesn't need to call `record_undo` any times, but must still record a read according to the same rules as `Accessor::raw_read`.
  */
  fn raw_write<'a, E: EntityKind>(
    &'a mut self,
    entity: TypedHandleRef<'a, E, Self::EntityHandleKind>,
  ) -> (
    Self::WriteGuard<'a, MutableData<E, Self::EntityHandleKind>>,
    Self::UndoRecorder<'a, MutableData<E, Self::EntityHandleKind>>,
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
pub trait SnapshotAccessor<'acc>: InitializedAccessor<'acc> {
  /// The iterator type returned by `scheduled_events`.
  type ScheduledEvents<'a>: Iterator<Item = DynHandle<Self::EntityHandleKind>> + 'a
  where
    Self: 'a;

  /**
  Take an iterator over all entities that are currently scheduled to wake in the future.

  Library users usually won't call this explicitly, but it's necessary for serializing a complete snapshot, because it's possible for there to be scheduled entities that are not accessible the following other entity handles starting at the globals. The globals and scheduled_events taken together provide all of the "roots" from which other entities can be observed.
  */
  fn scheduled_events(&self) -> Self::ScheduledEvents<'_>;
}

pub trait Snapshot {
  type SimulationSpec: SimulationSpec;
  type EntityHandleKind: EntityHandleKindDeref;
  type SnapshotAccessor<'acc>: SnapshotAccessor<
    'acc,
    SimulationSpec = Self::SimulationSpec,
    EntityHandleKind = Self::EntityHandleKind,
  >;

  fn with_accessor<'a, R>(&'a self, f: impl FnOnce(&Self::SnapshotAccessor<'a>) -> R) -> R;
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
  type Snapshot: Snapshot<
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

  /**
  Obtain a snapshot of the current state of all entities at a specific time.

  This is based on whatever fiat events currently exist. If you obtain a snapshot and then change fiat events while holding onto the snapshot, the snapshot will still report the

  The "before" denotes that from the perspective of the snapshot, events whose time is *exactly* the snapshot time have not been run yet.

  In flat TimeStewards, this may prevent you from adding or removing any fiat events before the time of the snapshot, i.e. this may increase `earliest_mutable_time` to the time specified. In full TimeStewards, it does not have such side effects.

  # Runtime cost

  A call to `snapshot_before` will immediately run the full simulation all the way to the given time. If the TimeSteward has already run the simulation up to the given time (or further), `snapshot_before` can return immediately.

  Calling `snapshot_before` does *not* immediately copy all of the entity data. It *may* eventually incur the full cost of copying all the entity data lazily, as copies become needed due the entity data changing, but it doesn't introduce any high worst-case time costs.
  */
  fn snapshot_before(
    &mut self,
    time: <Self::SimulationSpec as SimulationSpec>::Time,
  ) -> Option<Self::Snapshot>;

  /**
  Do a small amount of simulation work.

  This trait doesn't impose any particular real-time requirements on the implementor. Simple TimeStewards may implement this by running one event at a time, but may still use amortized data structures like `Vec` and `HashMap`, leading to high worst-case time costs. Optimized TimeStewards should avoid high worst-case time costs in `step`. Regardless of optimization level, a step involves calling user-defined `wake` implementations, so if there is a slow `wake` method, the TimeSteward can't preempt that.

  This function won't run any events at or after `limit`. Thus, even in a flat TimeSteward, a call to `step` cannot increase `earliest_mutable_time` further than `limit`.
  */
  fn step(&mut self, limit: Option<<Self::SimulationSpec as SimulationSpec>::Time>);

  /**
  Gets the latest time when a snapshot can be returned without doing more work.

  `None` means that we can immediately take a snapshot at any time for the entire future. This only happens if the simulation has become completely static (no more events are scheduled).

  Calls to `snapshot_before` necessarily increase `latest_time_ready_for_snapshot` to at least the snapshot time.
  */
  fn latest_time_ready_for_snapshot(
    &self,
  ) -> Option<<Self::SimulationSpec as SimulationSpec>::Time>;

  /**
  Inform the TimeSteward that you will not add or remove any more fiat events before the given time.

  This may increase `earliest_mutable_time` to the time specified.
  */
  fn freeze_before(&mut self, time: <Self::SimulationSpec as SimulationSpec>::Time);

  /**
  Gets the earliest time when fiat events may be added or removed.

  `None` means that fiat events may be added or removed at any time.

  In a full TimeSteward, this may only increase when explicitly increased by `freeze_before` (or `forget_before`, which implies `freeze_before`). In a flat TimeSteward, it may also increase up to the time specified in a call to `snapshot_before` or `step`.
  */
  fn earliest_mutable_time(&self) -> Option<<Self::SimulationSpec as SimulationSpec>::Time>;

  /**
  Allow the TimeSteward to save memory by discarding old data.

  This call informs the TimeSteward that you will not request any new snapshots before times less than the given time. This may increase `earliest_remembered_time` to the time specified. If you never call this function, then any TimeSteward (even a flat TimeSteward) must retain the entire history indefinitely.

  Snapshots obtained before calling `forget_before` are still required to work.

  `forget_before` also implies `freeze_before`
  */
  fn forget_before(&mut self, time: <Self::SimulationSpec as SimulationSpec>::Time);

  /**
  Gets the earliest time for which `snapshot_before` can be called.

  `None` means that we currently remember the entire history.

  This may only increase when explicitly increased by `forget_before`.
  */
  fn earliest_remembered_time(&self) -> Option<<Self::SimulationSpec as SimulationSpec>::Time>;
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
