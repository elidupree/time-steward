use serde::{de::DeserializeOwned, ser, Deserialize, Serialize};
use std::any::Any;
use std::fmt::Debug;
use std::hash::Hash;
use std::io::Read;
use std::ops::{Deref, DerefMut};

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
  ) -> A::ReadGuard<'a, E>
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
  fn query<A: Accessor<EntityHandleKind = H>>(self, accessor: &mut A) -> A::ReadGuard<'a, E>
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

pub trait Accessor {
  type SimulationSpec: SimulationSpec;
  type EntityHandleKind: EntityHandleKindDeref;

  // maybe a &, maybe a cell::Ref, maybe something else...
  type ReadGuard<'a, E: EntityKind>: Deref<Target = MutableData<E, Self::EntityHandleKind>>;

  // requires &'a self as well as 'a entity, so that we can statically guarantee
  // no overlap with writes (hypothetically allowing TimeSteward implementors to use UnsafeCell
  // and return & instead of RefCell and return Ref)
  //
  // this function isn't undo-safe, it's to build undo-safe wrappers around
  fn raw_read<'a, 'b: 'a, E: EntityKind>(
    &'a self,
    // at the time of this writing, we cannot use the type alias TypedHandleRef due to
    // https://github.com/rust-lang/rust/issues/85533
    entity: <Self::EntityHandleKind as EntityHandleKindDeref>::TypedHandleRef<'b, E>,
  ) -> Self::ReadGuard<'a, E>;

  // record that we accessed this entity; if record_read has been called for an entity, raw_read is undo-safe
  //
  // calling record_read is always undo-safe because false positives aren't a problem
  fn record_read<E: EntityKind>(
    &self,
    // at the time of this writing, we cannot use the type alias TypedHandleRef due to
    // https://github.com/rust-lang/rust/issues/85533
    entity: <Self::EntityHandleKind as EntityHandleKindDeref>::TypedHandleRef<'_, E>,
  ) {
  }

  // this function isn't undo-safe, it's to build undo-safe wrappers around
  fn raw_read_schedule<E: Wake<Self::SimulationSpec>>(
    &self,
    entity: TypedHandleRef<E, Self::EntityHandleKind>,
  ) -> Option<<Self::SimulationSpec as SimulationSpec>::Time>;
}

pub trait AccessorExt: Accessor {
  // read undo-safely by explicitly recording an access
  fn read<'a, E: EntityKind>(
    &'a self,
    // at the time of this writing, we cannot use the type alias TypedHandleRef due to
    // https://github.com/rust-lang/rust/issues/85533
    entity: <Self::EntityHandleKind as EntityHandleKindDeref>::TypedHandleRef<'a, E>,
  ) -> Self::ReadGuard<'a, E> {
    self.record_read(entity);
    self.raw_read(entity)
  }

  // read undo-safely by explicitly recording an access
  fn read_schedule<E: Wake<Self::SimulationSpec>>(
    &self,
    entity: TypedHandleRef<E, Self::EntityHandleKind>,
  ) -> Option<<Self::SimulationSpec as SimulationSpec>::Time> {
    self.record_read(entity);
    self.raw_read_schedule(entity)
  }
}
impl<A: Accessor> AccessorExt for A {}

pub trait InitializedAccessor: Accessor {
  fn globals(&self) -> &Globals<Self::SimulationSpec, Self::EntityHandleKind>;
  fn now(&self) -> &<Self::SimulationSpec as SimulationSpec>::Time;
}

pub trait CreateEntityAccessor: Accessor {
  fn create_entity<E: EntityKind>(
    &mut self,
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
  // maybe a &mut, maybe a cell::RefMut, maybe something else...
  type WriteGuard<'a, E: EntityKind>: DerefMut<Target = MutableData<E, Self::EntityHandleKind>>;

  // this function isn't undo-safe, it's to build undo-safe wrappers around; you need to record_undo
  fn raw_write<'a, 'b: 'a, E: EntityKind>(
    &'a mut self,
    // at the time of this writing, we cannot use the type alias TypedHandleRef due to
    // https://github.com/rust-lang/rust/issues/85533
    entity: <Self::EntityHandleKind as EntityHandleKindDeref>::TypedHandleRef<'b, E>,
  ) -> Self::WriteGuard<'a, E>;

  // record a way to undo something we did in this event; undo operations may later be run in reverse order
  //
  // calling record_undo isn't undo-safe because things are broken if you undo wrong
  // record_undo also serves the purpose of record_read?
  fn record_undo<E: EntityKind>(
    &self,
    // at the time of this writing, we cannot use the type alias TypedHandleRef due to
    // https://github.com/rust-lang/rust/issues/85533
    entity: <Self::EntityHandleKind as EntityHandleKindDeref>::TypedHandleRef<'_, E>,
    undo: impl Fn(&mut MutableData<E, Self::EntityHandleKind>) + 'static,
  );

  fn schedule<E: Wake<Self::SimulationSpec>>(
    &mut self,
    entity: TypedHandleRef<E, Self::EntityHandleKind>,
    time: <Self::SimulationSpec as SimulationSpec>::Time,
  ) {
    self.set_schedule(entity, Some(time))
  }

  fn unschedule<E: Wake<Self::SimulationSpec>>(
    &mut self,
    entity: TypedHandleRef<E, Self::EntityHandleKind>,
  ) {
    self.set_schedule(entity, None)
  }

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
pub trait EventAccessorExt: EventAccessor {
  // undo-safe write by recording the full old state before returning mutable reference
  fn write<'a, 'b: 'a, E: EntityKind>(
    &'a mut self,
    // at the time of this writing, we cannot use the type alias TypedHandleRef due to
    // https://github.com/rust-lang/rust/issues/85533
    entity: <Self::EntityHandleKind as EntityHandleKindDeref>::TypedHandleRef<'b, E>,
  ) -> Self::WriteGuard<'a, E> {
    let old_value = self.raw_read(entity).clone();
    self.record_undo(entity, move |m| *m = old_value.clone());
    self.raw_write(entity)
  }
}
impl<A: EventAccessor> EventAccessorExt for A {}

pub trait SnapshotAccessor: InitializedAccessor {
  type ScheduledEvents<'a>: Iterator<Item = DynHandle<Self::EntityHandleKind>> + 'a;
  fn scheduled_events(&self) -> Self::ScheduledEvents<'_>;
}

pub trait TimeSteward: Any + Sized + Debug {
  type SimulationSpec: SimulationSpec;
  type EntityHandleKind: EntityHandleKindDeref;
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
