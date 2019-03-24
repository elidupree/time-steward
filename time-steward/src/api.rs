use serde::de::DeserializeOwned;
use serde::Serialize;
use std::hash::Hash;
use crate::DeterministicRandomId;
use std::io::{Read, Write};
use std::any::Any;
use std::fmt::Debug;
//use std::cmp::Ordering;
use std::borrow::Borrow;
use std::ops::Deref;
use std::marker::PhantomData;

use crate::type_utils::{PersistentlyIdentifiedType, PersistentTypeId};
use crate::type_utils::list_of_types::{ListOfTypes};

/// Data used for a TimeSteward simulation, such as times, entities, and events.
///
/// We used to require `Send + Sync` for SimulationStateData, but now that EntityHandles can be part of SimulationStateData, we have to omit that to support TimeSteward types that have !Send/!Sync handles (like Rc)
///
/// Clone, Eq, and Hash are omitted because
pub trait SimulationStateData: Any + Clone + Eq + Hash + Serialize + DeserializeOwned + Debug {}
impl<T: Any + Clone + Eq + Hash + Serialize + DeserializeOwned + Debug> SimulationStateData for T {}

// Model: events interact with the physics only through queries at their exact time (which are forbidden to query other timelines or have any side effects) and modifications at their exact time (which are forbidden to return any information). Those modifications, in practice, change the state *going forward from* that time.

//These would be associated type constructors if Rust supported those: EntityHandle, EventHandle, DynamicEventHandle, PredictionHandle
/**
This is intended to be implemented on an empty struct. Requiring Clone etc. is a hack to work around [a compiler weakness](https://github.com/rust-lang/rust/issues/26925).
*/
pub trait SimulationSpec:
  Any + Send + Sync + Copy + Clone + Ord + Hash + Serialize + DeserializeOwned + Debug + Default
{
  type Time: SimulationStateData + Send + Sync + Clone + Ord + Hash;
  type Globals: SimulationStateData;
  type Types: ListOfTypes;
  const MAX_ITERATION: IterationType = 65535;
}

pub type IterationType = u32;
#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Debug, Serialize, Deserialize)]
pub struct ExtendedTime<S: SimulationSpec> {
  pub base: S::Time,
  pub iteration: IterationType,
  pub id: DeterministicRandomId,
}

impl<S: SimulationSpec> ExtendedTime<S> {
  pub fn beginning_of(time: S::Time) -> ExtendedTime<S> {
    ExtendedTime {
      base: time,
      iteration: 0,
      id: DeterministicRandomId::MIN,
    }
  }
  pub fn end_of(time: S::Time) -> ExtendedTime<S> {
    ExtendedTime {
      base: time,
      iteration: S::MAX_ITERATION,
      id: DeterministicRandomId::MAX,
    }
  }
}

#[derive(Copy, Clone, PartialEq, Eq, Debug)]
pub enum FiatEventOperationError {
  InvalidInput,
  InvalidTime,
}


/// Data handles where clones point to the same data, and Eq and Hash by object identity
pub trait DataHandleTrait: SimulationStateData {}

pub trait EventHandleTrait<S: SimulationSpec>: DataHandleTrait + Borrow<ExtendedTime<S>>
{
  fn extended_time(&self) -> &ExtendedTime<S>;
  fn time(&self) -> &S::Time {
    &self.extended_time().base
  }
  fn id(&self) -> &DeterministicRandomId {
    &self.extended_time().id
  }
}

pub trait EntityHandleTrait: DataHandleTrait + PersistentlyIdentifiedType {
  type ImmutableData: SimulationStateData + PersistentlyIdentifiedType;
  type MutableData: SimulationStateData + PersistentlyIdentifiedType;
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



pub trait Event: SimulationStateData + PersistentlyIdentifiedType {
  type Steward: TimeSteward;
  fn execute<Accessor: EventAccessor<Steward = Self::Steward>>(
    &self,
    accessor: &mut Accessor,
  );
}

pub trait Accessor {
  type Steward: TimeSteward;
  fn globals(&self) -> &<<Self::Steward as TimeSteward>::SimulationSpec as SimulationSpec>::Globals;
  fn extended_now(&self) -> &ExtendedTime<<Self::Steward as TimeSteward>::SimulationSpec>;
  fn now(&self) -> &<<Self::Steward as TimeSteward>::SimulationSpec as SimulationSpec>::Time {
    &self.extended_now().base
  }
  fn id(&self) -> &DeterministicRandomId {
    &self.extended_now().id
  }
  // TODO: see if I can change the lifetimes here to make it more practical for accessors to have mutable methods. Perhaps by giving the accessor trait a lifetime?
  fn query <'a, T: EntityHandleTrait> (&'a self, entity: &'a T)-><Self as AccessorQueryHack<'a, T>>::QueryGuard {self.query_hack(entity)}
}

pub trait AccessorQueryHack <'a, T: EntityHandleTrait> {
  type QueryGuard: Deref<Target = T::MutableData>;
  fn query_hack (&'a self, entity: &'a T)->Self::QueryGuard;
}

impl <'a, T: EntityHandleTrait, A: Accessor + ?Sized> AccessorQueryHack<'a, T> for A {
  type QueryGuard = &'a T::MutableData;
  fn query_hack (&'a self, _entity: &'a T)->Self::QueryGuard {panic!("query_hack() called when it shouldn't be")}
}


pub trait TimeStewardEntityHandleHack <ImmutableData: SimulationStateData + PersistentlyIdentifiedType, MutableData: SimulationStateData + PersistentlyIdentifiedType> {
  type EntityHandle: EntityHandleTrait + Deref<Target = ImmutableData>;
  fn new_entity_handle_nonreplicable_hack (immutable: ImmutableData, mutable: MutableData)->Self::EntityHandle;
}

type EntityHandle <Steward, ImmutableData, MutableData> = <Steward as TimeStewardEntityHandleHack <ImmutableData, MutableData>>::EntityHandle;

#[derive (Clone, PartialEq, Eq, Hash, Serialize, Deserialize, Debug)]
pub struct NeverEntityHandle <T, U> (PhantomData <*const (T, U)>);

impl<ImmutableData: SimulationStateData + PersistentlyIdentifiedType, MutableData: SimulationStateData + PersistentlyIdentifiedType> DataHandleTrait for NeverEntityHandle <ImmutableData, MutableData> {}
impl<ImmutableData: SimulationStateData + PersistentlyIdentifiedType, MutableData: SimulationStateData + PersistentlyIdentifiedType> EntityHandleTrait for NeverEntityHandle <ImmutableData, MutableData> {type ImmutableData = ImmutableData; type MutableData = MutableData;}
impl<ImmutableData: SimulationStateData + PersistentlyIdentifiedType, MutableData: SimulationStateData + PersistentlyIdentifiedType> PersistentlyIdentifiedType for NeverEntityHandle <ImmutableData, MutableData> {const ID: PersistentTypeId = PersistentTypeId (0x0793df01adf97abd);}
impl<ImmutableData: SimulationStateData + PersistentlyIdentifiedType, MutableData: SimulationStateData + PersistentlyIdentifiedType> Deref for NeverEntityHandle <ImmutableData, MutableData> {type Target = ImmutableData; fn deref (&self)->& ImmutableData {unreachable!()}}

impl <ImmutableData: SimulationStateData + PersistentlyIdentifiedType, MutableData: SimulationStateData + PersistentlyIdentifiedType, Steward: TimeSteward> TimeStewardEntityHandleHack <ImmutableData, MutableData> for Steward {
  type EntityHandle = NeverEntityHandle<ImmutableData, MutableData>;
  fn new_entity_handle_nonreplicable_hack (_immutable: ImmutableData, _mutable: MutableData)->Self::EntityHandle {
    panic!("new_entity_handle_nonreplicable_hack() called when it shouldn't be")
  }
}

pub trait TimeStewardEntityHandle <T: EntityHandleTrait>: TimeSteward {
  fn create_entity_handle_nonreplicable (immutable: T::ImmutableData, mutable: T::MutableData)->T;
}

pub trait Modify <T>: SimulationStateData {
  type UndoData: SimulationStateData;
  fn modify (self, entity: &mut T)->Self::UndoData;
  fn undo (entity: &mut T, undo_data: &Self::UndoData);
}

#[derive(Clone, PartialEq, Eq, Hash, Serialize, Deserialize, Debug, Default)]
pub struct ReplaceWith <T> (pub T);


pub trait EventAccessor: Accessor {
  fn this_event(&self) -> &<Self::Steward as TimeSteward>::EventHandle;

  fn new_entity_handle<ImmutableData: SimulationStateData + PersistentlyIdentifiedType, MutableData: SimulationStateData + PersistentlyIdentifiedType>(
    &self,
    immutable: ImmutableData, mutable: MutableData
  ) -> EntityHandle<Self::Steward, ImmutableData, MutableData>;

  fn modify <'a, T: EntityHandleTrait, M: Modify<T::MutableData>> (&'a self, entity: &'a T, modification: M);
    //where Self::Steward: TimeStewardEntityCell<'a, E, C>;

  fn create_prediction<E: Event<Steward = Self::Steward>>(
    &self,
    time: <<Self::Steward as TimeSteward>::SimulationSpec as SimulationSpec>::Time,
    event: E,
  ) -> <Self::Steward as TimeSteward>::EventHandle;
  
  // audit: you can't destroy a fiat event as if it's a prediction
  fn destroy_prediction(&self, prediction: &<Self::Steward as TimeSteward>::EventHandle);
}

pub trait SnapshotAccessor: Accessor {
  /// note: SnapshotAccessor::serialize() matches TimeSteward::deserialize()
  fn serialize_into<W: Write>(&self, writer: &mut W) -> ::bincode::Result<()>;
}

pub trait TimeSteward: Any + Sized + Debug {
  type SimulationSpec: SimulationSpec;
  type SnapshotAccessor: SnapshotAccessor<Steward = Self>;
  type EventHandle: EventHandleTrait<Self::SimulationSpec>;
  
  fn new_entity_handle_nonreplicable<ImmutableData: SimulationStateData + PersistentlyIdentifiedType, MutableData: SimulationStateData + PersistentlyIdentifiedType>(
    &self,
    immutable: ImmutableData, mutable: MutableData
  ) -> EntityHandle<Self, ImmutableData, MutableData>{Self::new_entity_handle_nonreplicable_hack(immutable, mutable)}
  
  fn insert_fiat_event<E: Event<Steward = Self>>(
    &mut self,
    time: <Self::SimulationSpec as SimulationSpec>::Time,
    id: DeterministicRandomId,
    event: E,
  ) -> Result<(), FiatEventOperationError>;
  fn remove_fiat_event(
    &mut self,
    time: &<Self::SimulationSpec as SimulationSpec>::Time,
    id: DeterministicRandomId,
  ) -> Result<(), FiatEventOperationError>;
  fn snapshot_before(
    &mut self,
    time: &<Self::SimulationSpec as SimulationSpec>::Time,
  ) -> Option<Self::SnapshotAccessor>;
  
  fn valid_since(&self) -> ValidSince<<Self::SimulationSpec as SimulationSpec>::Time>;
  fn forget_before(&mut self, time: &<Self::SimulationSpec as SimulationSpec>::Time);
}

/// A trait for TimeSteward types that can be initialized from just the initial physics data.
/// Most TimeSteward types should implement this. Exceptions are types that can't function without certain extra runtime metadata
pub trait ConstructibleTimeSteward: TimeSteward {
  fn from_globals(globals: <Self::SimulationSpec as SimulationSpec>::Globals) -> Self;
  /// note: SnapshotAccessor::serialize() matches TimeSteward::deserialize()
  fn deserialize_from<R: Read>(data: &mut R) -> ::bincode::Result<Self>;
}

pub trait IncrementalTimeSteward: TimeSteward {
  fn step(&mut self);
  fn updated_until_before(&self) -> Option<<Self::SimulationSpec as SimulationSpec>::Time>;
}

pub trait CanonicalTimeSteward: TimeSteward {}
