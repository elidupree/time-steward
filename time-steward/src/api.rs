use serde::{de::DeserializeOwned, ser, Deserialize, Serialize};
use std::any::Any;
use std::fmt::Debug;
use std::hash::Hash;
use std::io::Read;
use std::ops::Deref;

use crate::type_utils::list_of_types::ListOfTypes;
use crate::type_utils::PersistentlyIdentifiedType;
use crate::EntityId;

/// Data used for a TimeSteward simulation, such as times, entities, and events.
///
/// We used to require `Send + Sync` for SimulationStateData, but now that EntityHandles can be part of SimulationStateData, we have to omit that to support TimeSteward types that have !Send/!Sync handles (like Rc)
///
/// Requiring DeserializeOwned is improper because you can't deserialize EntityHandles without more
///  information; the current approach is a hack where Deserialize
/// uses a thread-local context for that; it may later be replaced with a proper custom derive of my own.
pub trait SimulationStateData:
  Any + Clone + Eq + Hash + ser::Serialize + DeserializeOwned + Debug
{
}
impl<T: Any + Clone + Eq + Hash + ser::Serialize + DeserializeOwned + Debug> SimulationStateData
  for T
{
}

/// Data handles where clones point to the same data, and Eq and Hash by object identity, and Serialize by id.
pub trait EntityHandle: SimulationStateData + Ord {
  fn id(&self) -> EntityId;
}

pub trait EntityKind: Any + Sized + PersistentlyIdentifiedType {
  type ImmutableData: SimulationStateDataKind;
  type MutableData: SimulationStateDataKind;
}

pub type ImmutableData<E, H> =
  <<E as EntityKind>::ImmutableData as SimulationStateDataKind>::Data<H>;
pub type MutableData<E, H> = <<E as EntityKind>::MutableData as SimulationStateDataKind>::Data<H>;
pub type Globals<S, H> = <<S as SimulationSpec>::Globals as SimulationStateDataKind>::Data<H>;

pub trait EntityHandleKind {
  type TypedHandle<E: EntityKind>: EntityHandle;
  type DynHandle: EntityHandle;
}

pub type TypedHandle<E, H> = <H as EntityHandleKind>::TypedHandle<E>;
pub type DynHandle<H> = <H as EntityHandleKind>::DynHandle;

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
    this: TypedHandle<Self, Accessor::EntityHandleKind>,
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

pub trait Accessor {
  type SimulationSpec: SimulationSpec;
  type EntityHandleKind: EntityHandleKind;

  // this would prefer to be a method of the EntityHandle types, but I can't make the traits work for that
  fn get_immutable<E: EntityKind>(
    entity: &TypedHandle<E, Self::EntityHandleKind>,
  ) -> &ImmutableData<E, Self::EntityHandleKind>;

  type QueryGuard<'a, E: EntityKind>: Deref<Target = MutableData<E, Self::EntityHandleKind>>;
  // TODO: see if I can change the lifetimes here to make it more practical for accessors to have mutable methods. Perhaps by giving the accessor trait a lifetime?
  fn query<'a, E: EntityKind>(
    &'a self,
    entity: &'a TypedHandle<E, Self::EntityHandleKind>,
  ) -> Self::QueryGuard<'a, E>;

  fn query_schedule<E: Wake<Self::SimulationSpec>>(
    &self,
    entity: &TypedHandle<E, Self::EntityHandleKind>,
  ) -> Option<<Self::SimulationSpec as SimulationSpec>::Time>;
}

pub trait InitializedAccessor: Accessor {
  fn globals(&self) -> &Globals<Self::SimulationSpec, Self::EntityHandleKind>;
  fn now(&self) -> &<Self::SimulationSpec as SimulationSpec>::Time;
}

pub trait CreateEntityAccessor: Accessor {
  fn create_entity<E: EntityKind>(
    &self,
    immutable: ImmutableData<E, Self::EntityHandleKind>,
    mutable: MutableData<E, Self::EntityHandleKind>,
  ) -> TypedHandle<E, Self::EntityHandleKind>;
}

pub trait GlobalsConstructionAccessor: CreateEntityAccessor {}

pub trait Modify<T>: SimulationStateData {
  type UndoData: SimulationStateData;
  fn modify(self, entity: &mut T) -> Self::UndoData;
  fn undo(entity: &mut T, undo_data: &Self::UndoData);
}

#[derive(Clone, PartialEq, Eq, Hash, Serialize, Deserialize, Debug, Default)]
pub struct ReplaceWith<T>(pub T);

pub trait EventAccessor: InitializedAccessor + CreateEntityAccessor {
  fn modify<'a, E: EntityKind, M: Modify<MutableData<E, Self::EntityHandleKind>>>(
    &'a self,
    entity: &'a TypedHandle<E, Self::EntityHandleKind>,
    modification: M,
  );

  fn schedule<E: Wake<Self::SimulationSpec>>(
    &self,
    entity: &TypedHandle<E, Self::EntityHandleKind>,
    time: <Self::SimulationSpec as SimulationSpec>::Time,
  ) {
    self.set_schedule(entity, Some(time))
  }

  fn unschedule<E: Wake<Self::SimulationSpec>>(
    &self,
    entity: &TypedHandle<E, Self::EntityHandleKind>,
  ) {
    self.set_schedule(entity, None)
  }

  fn set_schedule<E: Wake<Self::SimulationSpec>>(
    &self,
    entity: &TypedHandle<E, Self::EntityHandleKind>,
    time: Option<<Self::SimulationSpec as SimulationSpec>::Time>,
  ) {
    match time {
      None => self.unschedule(entity),
      Some(time) => self.schedule(entity, time),
    }
  }
}

pub trait SnapshotAccessor: Accessor {
  type ScheduledEvents: Iterator<Item = DynHandle<Self::EntityHandleKind>>;
  fn scheduled_events(&self) -> Self::ScheduledEvents;
}

pub trait TimeSteward: Any + Sized + Debug {
  type SimulationSpec: SimulationSpec;
  type EntityHandleKind: EntityHandleKind;
  type SnapshotAccessor: SnapshotAccessor<
    SimulationSpec = Self::SimulationSpec,
    EntityHandleKind = Self::EntityHandleKind,
  >;

  fn insert_fiat_event<E: Wake<Self::SimulationSpec>>(
    &mut self,
    time: <Self::SimulationSpec as SimulationSpec>::Time,
    id: EntityId,
    immutable: ImmutableData<E, Self::EntityHandleKind>,
    mutable: MutableData<E, Self::EntityHandleKind>,
  ) -> Result<(), FiatEventOperationError>;
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
