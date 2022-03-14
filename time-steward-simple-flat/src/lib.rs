#![feature(
  generic_associated_types,
  specialization,
  type_alias_impl_trait,
  map_try_insert,
  map_first_last,
  never_type
)]
#![warn(unsafe_op_in_unsafe_fn)]
#![allow(incomplete_features)]

/*!

The simplest possible implementation of the TimeSteward interface.

This is intended as a reference implementation of the canonical TimeSteward simulation behavior, and its goal is to be as simple as possible, sacrificing such things as "being a FullTimeSteward", "being optimized", and even "preventing memory leaks".

This has 2 purposes:
1. To help the maintainer(s) of the TimeSteward library understand the behavior that the more-optimized TimeSteward implementors are trying to replicate.
2. To be used in automated tests of more-optimized TimeSteward implementors, to guarantee that they *do* in fact replicate the intended behavior.

*/

use derivative::Derivative;
use std::cell::RefCell;
use std::collections::{BTreeSet, HashMap};
use std::fmt::Debug;
use std::io::Read;
use std::marker::PhantomData;
use std::ops::Deref;
use std::rc::Rc;
use triomphe::{Arc, ArcBorrow};
use unsize::CoerceUnsize;

use serde::Serialize;
use time_steward_api::entity_handles::*;
use time_steward_api::*;
use time_steward_entity_views::RestoreOldValue;
use time_steward_implementation_support::accessor_cell::AccessorCell;
use time_steward_implementation_support::{
  accessor_cell, EventChildrenIdGenerator, GlobalsConstructionIdGenerator,
};

// ###################################################
// ############     Handle definitions    ############
// ###################################################
#[derive(Derivative)]
#[derivative(Clone(bound = ""), Debug(bound = ""))]
pub struct SfTypedHandle<S: SimulationSpec, E: EntityKind>(Arc<EntityInner<S, E>>);
#[derive(Derivative)]
#[derivative(Clone(bound = ""), Debug(bound = ""))]
pub struct SfDynHandle<S: SimulationSpec>(Arc<dyn AnyEntityInner<S>>);
#[repr(transparent)]
#[derive(Derivative)]
#[derivative(Copy(bound = ""), Clone(bound = ""), Debug(bound = ""))]
pub struct SfTypedHandleRef<'a, S: SimulationSpec, E: EntityKind>(ArcBorrow<'a, EntityInner<S, E>>);
#[repr(transparent)]
#[derive(Derivative)]
#[derivative(Copy(bound = ""), Clone(bound = ""), Debug(bound = ""))]
pub struct SfDynHandleRef<'a, S: SimulationSpec>(ArcBorrow<'a, dyn AnyEntityInner<S>>);

#[derive(Derivative)]
#[derivative(
  Copy(bound = ""),
  Clone(bound = ""),
  PartialEq(bound = ""),
  Eq(bound = ""),
  PartialOrd(bound = ""),
  Ord(bound = ""),
  Hash(bound = ""),
  Debug(bound = ""),
  Default(bound = "")
)]
pub struct SfEntityHandleKind<S>(PhantomData<*const S>);
impl<S: SimulationSpec> EntityHandleKind for SfEntityHandleKind<S> {
  type TypedHandle<E: EntityKind> = SfTypedHandle<S, E>;
  type DynHandle = SfDynHandle<S>;
}
// Safety: SfTypedHandleRef and SfDynHandleRef wrap ArcBorrow, which guarantees that it
// has the same representation as &T
unsafe impl<S: SimulationSpec> EntityHandleKindDeref for SfEntityHandleKind<S> {
  type TypedHandleRef<'a, E: EntityKind> = SfTypedHandleRef<'a, S, E>;
  type DynHandleRef<'a> = SfDynHandleRef<'a, S>;
}

#[derive(Derivative)]
#[derivative(Debug(bound = ""))]
struct EntityUniversal<S: SimulationSpec> {
  id: EntityId,
  schedule: History<S, Option<S::Time>>,
}
#[derive(Derivative)]
#[derivative(Debug(bound = ""))]
struct EntityInner<S: SimulationSpec, E: EntityKind> {
  universal: EntityUniversal<S>,
  immutable: ImmutableData<E, SfEntityHandleKind<S>>,
  mutable: History<S, MutableData<E, SfEntityHandleKind<S>>>,
}
trait AnyEntityInner<S: SimulationSpec>: Debug {
  fn universal(&self) -> &EntityUniversal<S>;
  /// Safety: Caller must only call this with a type stored in a triomphe Arc
  unsafe fn wake(&self, accessor: &mut SfEventAccessor<S>);
}

trait DynPerformUndo<T> {
  fn undo(&self, target: &mut T, undo_data: &[u8]);
}
impl<T, P: PerformUndo<T>> DynPerformUndo<T> for PhantomData<*const P> {
  fn undo(&self, target: &mut T, undo_data: &[u8]) {
    P::perform_undo(target, bincode::deserialize(undo_data).unwrap())
  }
}

#[derive(Derivative)]
#[derivative(Debug(bound = ""))]
struct UndoEntry<S: SimulationSpec, T> {
  time: S::Time,
  undo_data: Vec<u8>,
  #[derivative(Debug = "ignore")]
  undo_performer: Box<dyn DynPerformUndo<T>>,
}

#[derive(Derivative)]
#[derivative(Debug(bound = "T: Debug"))]
struct HistoryInner<S: SimulationSpec, T> {
  current_value: T,
  changes: Vec<UndoEntry<S, T>>,
}

#[derive(Derivative)]
#[derivative(Debug(bound = "T: Debug"))]
struct History<S: SimulationSpec, T> {
  current_value: AccessorCell<T>,
  changes: HistoryChanges<S, T>,
}
#[derive(Derivative)]
#[derivative(Debug(bound = "T: Debug"))]
struct HistoryChanges<S: SimulationSpec, T>(RefCell<Vec<UndoEntry<S, T>>>);

#[derive(Derivative)]
#[derivative(Clone(bound = "T: Clone"), Debug(bound = "T: Debug"))]
pub struct UndoRecorder<'a, S: SimulationSpec, T> {
  changes: &'a HistoryChanges<S, T>,
  time: S::Time,
}

impl<S: SimulationSpec, T: SimulationStateData> HistoryChanges<S, T> {
  fn record_undo<U: Serialize, P: PerformUndo<T>>(&self, time: S::Time, undo_data: &U) {
    self.0.borrow_mut().push(UndoEntry {
      time,
      undo_data: bincode::serialize(undo_data).unwrap(),
      undo_performer: Box::new(PhantomData::<*const P>),
    });
  }
}
impl<'a, S: SimulationSpec, T: SimulationStateData> RecordUndo<T> for UndoRecorder<'a, S, T> {
  fn record_undo<U: Serialize, P: PerformUndo<T>>(&self, undo_data: &U) {
    self
      .changes
      .record_undo::<U, P>(self.time.clone(), undo_data);
  }
}
impl<S: SimulationSpec, T: SimulationStateData> History<S, T> {
  fn new(starting_value: T) -> History<S, T> {
    History {
      current_value: AccessorCell::new(starting_value),
      changes: HistoryChanges(RefCell::new(Vec::new())),
    }
  }
  fn current_value<'a>(&'a self, guard: &'a impl accessor_cell::ReadAccess) -> &'a T {
    guard.read(&self.current_value)
  }
  fn raw_write<'a>(
    &'a self,
    guard: &'a mut accessor_cell::WriteGuard,
    time: S::Time,
  ) -> (&'a mut T, UndoRecorder<'a, S, T>) {
    (
      guard.write(&self.current_value),
      UndoRecorder {
        changes: &self.changes,
        time,
      },
    )
  }
  fn replace(&self, guard: &mut accessor_cell::WriteGuard, time: S::Time, new_value: T) {
    let (value, undo_recorder) = self.raw_write(guard, time);
    undo_recorder.record_undo::<T, RestoreOldValue<T>>(value);
    *value = new_value;
  }
  fn value_before(&self, guard: &impl accessor_cell::ReadAccess, time: &S::Time) -> T
  where
    T: Clone,
  {
    let mut value = self.current_value(guard).clone();
    for UndoEntry {
      time: change_time,
      undo_data,
      undo_performer,
    } in self.changes.0.borrow().iter().rev()
    {
      if change_time < time {
        break;
      }
      undo_performer.undo(&mut value, undo_data);
    }
    value
  }
}

// ###################################################
// ############       Handle impls        ############
// ###################################################

impl<S: SimulationSpec, E: EntityKind> EntityHandle for SfTypedHandle<S, E> {
  fn id(&self) -> EntityId {
    self.0.universal.id
  }
}

impl<S: SimulationSpec> EntityHandle for SfDynHandle<S> {
  fn id(&self) -> EntityId {
    self.0.universal().id
  }
}

impl<'a, S: SimulationSpec, E: EntityKind> EntityHandle for SfTypedHandleRef<'a, S, E> {
  fn id(&self) -> EntityId {
    self.0.universal.id
  }
}

impl<'a, S: SimulationSpec> EntityHandle for SfDynHandleRef<'a, S> {
  fn id(&self) -> EntityId {
    self.0.universal().id
  }
}

impl<S: SimulationSpec, E: EntityKind> TypedEntityHandle<E, SfEntityHandleKind<S>>
  for SfTypedHandle<S, E>
{
  fn get_immutable(&self) -> &ImmutableData<E, SfEntityHandleKind<S>> {
    &self.0.immutable
  }
}

impl<'a, S: SimulationSpec, E: EntityKind> TypedEntityHandle<E, SfEntityHandleKind<S>>
  for SfTypedHandleRef<'a, S, E>
{
  fn get_immutable(&self) -> &ImmutableData<E, SfEntityHandleKind<S>> {
    &self.0.immutable
  }
}

impl<S: SimulationSpec, E: EntityKind> OwnedTypedEntityHandle<E, SfEntityHandleKind<S>>
  for SfTypedHandle<S, E>
{
  fn erase(self) -> DynHandle<SfEntityHandleKind<S>> {
    DynHandle::from_wrapped_gat(SfDynHandle(self.0.unsize(
      // Safety: this is exactly the output of the Coercion macro from `unsize`, except only that I added the type parameter (the original macro doesn't support generic traits).
      unsafe {
        unsize::Coercion::new({
          fn coerce<'lt, S: SimulationSpec>(
            p: *const (impl AnyEntityInner<S> + 'lt),
          ) -> *const (dyn AnyEntityInner<S> + 'lt) {
            p
          }
          coerce
        })
      },
    )))
  }
  fn borrow(&self) -> TypedHandleRef<E, SfEntityHandleKind<S>> {
    TypedHandleRef::from_wrapped_gat(SfTypedHandleRef(self.0.borrow_arc()))
  }
}
impl<S: SimulationSpec> OwnedDynEntityHandle<SfEntityHandleKind<S>> for SfDynHandle<S> {
  //fn downcast<E: EntityKind>(self) -> Result<TypedHandle<E, SfEntityHandleKind<S>>, Self> {}
  fn borrow(&self) -> DynHandleRef<SfEntityHandleKind<S>> {
    todo!()
  }
}
impl<'a, S: SimulationSpec, E: EntityKind> BorrowedTypedEntityHandle<'a, E, SfEntityHandleKind<S>>
  for SfTypedHandleRef<'a, S, E>
{
  fn erase(self) -> DynHandleRef<'a, SfEntityHandleKind<S>> {
    todo!() //SfDynHandleRef(self.0.unsize(Coercion!(to dyn AnyEntityInner<S>)))
  }
  fn to_owned(self) -> TypedHandle<E, SfEntityHandleKind<S>> {
    TypedHandle::from_wrapped_gat(SfTypedHandle(self.0.clone_arc()))
  }
}
impl<'a, S: SimulationSpec> BorrowedDynEntityHandle<'a, SfEntityHandleKind<S>>
  for SfDynHandleRef<'a, S>
{
  //fn downcast<E: EntityKind>(self) -> Option<TypedHandleRef<'a, E, SfEntityHandleKind<S>>> {}
  // fn to_owned(self) -> DynHandle<SfEntityHandleKind<S>> {
  //   SfDynHandle(self.0.clone_arc())
  // }
}

impl<S: SimulationSpec, E: EntityKind> AnyEntityInner<S> for EntityInner<S, E> {
  fn universal(&self) -> &EntityUniversal<S> {
    &self.universal
  }

  default unsafe fn wake(&self, _accessor: &mut SfEventAccessor<S>) {
    unreachable!()
  }
}
impl<S: SimulationSpec, E: Wake<S>> AnyEntityInner<S> for EntityInner<S, E> {
  /// Safety: Caller must only call this with a type stored in a triomphe Arc
  unsafe fn wake(&self, accessor: &mut SfEventAccessor<S>) {
    <E as Wake<S>>::wake(
      accessor,
      TypedHandleRef::from_wrapped_gat(SfTypedHandleRef(unsafe { ArcBorrow::from_ref(self) })),
    );
  }
}

// ###################################################
// #########  Accessor/steward definitions  ##########
// ###################################################

#[derive(Derivative)]
#[derivative(Debug(bound = ""))]
pub struct SfEventAccessor<'a, S: SimulationSpec> {
  guard: accessor_cell::WriteGuard,
  now: &'a S::Time,
  first_waker_universal: &'a EntityUniversal<S>,
  globals: &'a Globals<S, SfEntityHandleKind<S>>,
  child_id_generator: EventChildrenIdGenerator,
  woken_now_stack: Vec<DynHandle<SfEntityHandleKind<S>>>,
  steward: &'a mut Steward<S>,
}

#[derive(Derivative)]
#[derivative(Debug(bound = ""))]
pub struct SfSnapshotAccessor<'a, S: SimulationSpec> {
  guard: accessor_cell::ReadGuard,
  snapshot: &'a SfSnapshot<S>,
}

#[derive(Derivative)]
#[derivative(Debug(bound = ""))]
pub struct SfSnapshot<S: SimulationSpec> {
  time: S::Time,
  globals: Rc<Globals<S, SfEntityHandleKind<S>>>,
  scheduled_events: Vec<DynHandle<SfEntityHandleKind<S>>>,
}

#[derive(Derivative)]
#[derivative(Debug(bound = ""))]
pub struct SfGlobalsConstructionAccessor<S: SimulationSpec> {
  guard: accessor_cell::WriteGuard,
  child_id_generator: GlobalsConstructionIdGenerator,
  _marker: PhantomData<*const S>,
}

#[derive(Derivative)]
#[derivative(
  Debug(bound = ""),
  Clone(bound = ""),
  PartialEq(bound = ""),
  Eq(bound = ""),
  PartialOrd(bound = ""),
  Ord(bound = "")
)]
struct ScheduledWake<S: SimulationSpec> {
  time: S::Time,
  entity: DynHandle<SfEntityHandleKind<S>>,
}

#[derive(Derivative)]
#[derivative(Debug(bound = ""))]
pub struct Steward<S: SimulationSpec> {
  globals: Rc<Globals<S, SfEntityHandleKind<S>>>,
  last_event: Option<S::Time>,
  schedule: BTreeSet<ScheduledWake<S>>,
  fiat_events: HashMap<EntityId, DynHandle<SfEntityHandleKind<S>>>,
}

// ###################################################
// ############           History         ############
// ###################################################
//
// mod history {
//
//   impl<S: SimulationSpec> History<S> {
//     pub fn rollback_to_before<T: SimulationStateData>(
//       &self,
//       head: HistoryIndex,
//       current_value: &T,
//       time: &ExtendedTime<S>,
//     ) -> T {
//       let mut value = current_value.clone();
//       let mut index = head;
//       while let Some(item) = self.get(index) {
//         if item.event.extended_time() < time {
//           break;
//         }
//         item.undo.undo(&mut value);
//         index = item.previous;
//       }
//       value
//     }
//   }
// }

// ###################################################
// ############      Accessor impls       ############
// ###################################################

impl<'acc, S: SimulationSpec> Accessor for SfEventAccessor<'acc, S> {
  type SimulationSpec = S;
  type EntityHandleKind = SfEntityHandleKind<S>;

  type ReadGuard<'a, T: 'a>
  where
    Self: 'a,
  = &'a T;
  fn raw_read<'a, E: EntityKind>(
    &'a self,
    entity: TypedHandleRef<'a, E, Self::EntityHandleKind>,
  ) -> Self::ReadGuard<'a, MutableData<E, Self::EntityHandleKind>> {
    entity
      .into_wrapped_gat()
      .0
      .get()
      .mutable
      .current_value(&self.guard)
  }

  fn raw_read_schedule<E: Wake<Self::SimulationSpec>>(
    &self,
    entity: TypedHandleRef<E, Self::EntityHandleKind>,
  ) -> Option<<Self::SimulationSpec as SimulationSpec>::Time> {
    entity
      .into_wrapped_gat()
      .0
      .universal
      .schedule
      .current_value(&self.guard)
      .clone()
  }
}

pub struct SnapshotQueryResult<T>(T);
impl<T> Deref for SnapshotQueryResult<T> {
  type Target = T;

  fn deref(&self) -> &Self::Target {
    &self.0
  }
}

impl<'acc, S: SimulationSpec> Accessor for SfSnapshotAccessor<'acc, S> {
  type SimulationSpec = S;
  type EntityHandleKind = SfEntityHandleKind<S>;

  type ReadGuard<'a, T: 'a>
  where
    Self: 'a,
  = SnapshotQueryResult<T>;
  fn raw_read<'a, E: EntityKind>(
    &'a self,
    entity: TypedHandleRef<'a, E, Self::EntityHandleKind>,
  ) -> Self::ReadGuard<'a, MutableData<E, Self::EntityHandleKind>> {
    SnapshotQueryResult(
      entity
        .into_wrapped_gat()
        .0
        .mutable
        .value_before(&self.guard, &self.snapshot.time),
    )
  }

  fn raw_read_schedule<E: Wake<Self::SimulationSpec>>(
    &self,
    entity: TypedHandleRef<E, Self::EntityHandleKind>,
  ) -> Option<<Self::SimulationSpec as SimulationSpec>::Time> {
    entity
      .into_wrapped_gat()
      .0
      .universal
      .schedule
      .value_before(&self.guard, &self.snapshot.time)
  }
}

impl<S: SimulationSpec> Accessor for SfGlobalsConstructionAccessor<S> {
  type SimulationSpec = S;
  type EntityHandleKind = SfEntityHandleKind<S>;

  type ReadGuard<'a, T: 'a> = &'a T;
  fn raw_read<'a, E: EntityKind>(
    &'a self,
    entity: TypedHandleRef<'a, E, Self::EntityHandleKind>,
  ) -> Self::ReadGuard<'a, MutableData<E, SfEntityHandleKind<S>>> {
    entity
      .into_wrapped_gat()
      .0
      .get()
      .mutable
      .current_value(&self.guard)
  }

  fn raw_read_schedule<E: Wake<Self::SimulationSpec>>(
    &self,
    entity: TypedHandleRef<E, Self::EntityHandleKind>,
  ) -> Option<<Self::SimulationSpec as SimulationSpec>::Time> {
    entity
      .into_wrapped_gat()
      .0
      .universal
      .schedule
      .current_value(&self.guard)
      .clone()
  }
}

impl<'acc, S: SimulationSpec> InitializedAccessor<'acc> for SfEventAccessor<'acc, S> {
  fn globals(&self) -> &'acc Globals<Self::SimulationSpec, Self::EntityHandleKind> {
    self.globals
  }
  fn now(&self) -> &'acc <Self::SimulationSpec as SimulationSpec>::Time {
    self.now
  }
}

impl<'acc, S: SimulationSpec> InitializedAccessor<'acc> for SfSnapshotAccessor<'acc, S> {
  fn globals(&self) -> &'acc Globals<Self::SimulationSpec, Self::EntityHandleKind> {
    &*self.snapshot.globals
  }
  fn now(&self) -> &'acc <Self::SimulationSpec as SimulationSpec>::Time {
    &self.snapshot.time
  }
}

impl<'acc, S: SimulationSpec> CreateEntityAccessor for SfEventAccessor<'acc, S> {
  fn create_entity<E: EntityKind>(
    &mut self,
    immutable: ImmutableData<E, Self::EntityHandleKind>,
    mutable: MutableData<E, Self::EntityHandleKind>,
  ) -> TypedHandle<E, Self::EntityHandleKind> {
    let id = self
      .child_id_generator
      .generate_id(&self.first_waker_universal.id, &self.now);
    TypedHandle::from_wrapped_gat(SfTypedHandle(Arc::new(EntityInner {
      universal: EntityUniversal {
        id,
        schedule: History::new(None),
      },
      immutable,
      mutable: History::new(mutable),
    })))
  }
}

impl<S: SimulationSpec> CreateEntityAccessor for SfGlobalsConstructionAccessor<S> {
  fn create_entity<E: EntityKind>(
    &mut self,
    immutable: ImmutableData<E, Self::EntityHandleKind>,
    mutable: MutableData<E, Self::EntityHandleKind>,
  ) -> TypedHandle<E, Self::EntityHandleKind> {
    let id = self.child_id_generator.generate_id();
    TypedHandle::from_wrapped_gat(SfTypedHandle(Arc::new(EntityInner {
      universal: EntityUniversal {
        id,
        schedule: History::new(None),
      },
      immutable,
      mutable: History::new(mutable),
    })))
  }
}

impl<'acc, S: SimulationSpec> EventAccessor<'acc> for SfEventAccessor<'acc, S> {
  type WriteGuard<'a, T: 'a> = &'a mut T;
  type UndoRecorder<'a, T: SimulationStateData>
  where
    Self: 'a,
  = UndoRecorder<'a, S, T>;
  #[allow(clippy::needless_lifetimes)] // Clippy is currently wrong about GATs
  fn map_write_guard<'a, T, U>(
    guard: Self::WriteGuard<'a, T>,
    f: impl FnOnce(&mut T) -> &mut U,
  ) -> Self::WriteGuard<'a, U> {
    f(guard)
  }

  fn raw_write<'a, E: EntityKind>(
    &'a mut self,
    entity: TypedHandleRef<'a, E, Self::EntityHandleKind>,
  ) -> (
    Self::WriteGuard<'a, MutableData<E, Self::EntityHandleKind>>,
    Self::UndoRecorder<'a, MutableData<E, Self::EntityHandleKind>>,
  ) {
    entity
      .into_wrapped_gat()
      .0
      .get()
      .mutable
      .raw_write(&mut self.guard, self.now.clone())
  }

  fn set_schedule<E: Wake<Self::SimulationSpec>>(
    &mut self,
    entity: TypedHandleRef<E, Self::EntityHandleKind>,
    time: Option<<Self::SimulationSpec as SimulationSpec>::Time>,
  ) {
    let old = entity
      .into_wrapped_gat()
      .0
      .universal
      .schedule
      .current_value(&self.guard)
      .clone();
    if old != time {
      entity.into_wrapped_gat().0.universal.schedule.replace(
        &mut self.guard,
        self.now.clone(),
        time.clone(),
      );
      if let Some(old_time) = old {
        assert!(
          self.steward.schedule.remove(&ScheduledWake {
            entity: entity.to_owned().erase(),
            time: old_time,
          }),
          "global schedule didn't match entity schedules"
        );
      }
      if let Some(new_time) = time.clone() {
        assert!(
          self.steward.schedule.insert(ScheduledWake {
            entity: entity.to_owned().erase(),
            time: new_time,
          }),
          "global schedule didn't match entity schedules"
        );
      }
    }
    if time == Some(self.now.clone()) && entity.id() <= self.first_waker_universal.id {
      self.woken_now_stack.push(entity.to_owned().erase());
    }
  }
}

type ScheduledEvents<'a, S: SimulationSpec> =
  impl Iterator<Item = DynHandle<SfEntityHandleKind<S>>> + 'a;

impl<'acc, S: SimulationSpec> SnapshotAccessor<'acc> for SfSnapshotAccessor<'acc, S> {
  type ScheduledEvents<'a>
  where
    Self: 'a,
  = ScheduledEvents<'a, S>;
  fn scheduled_events(&self) -> Self::ScheduledEvents<'_> {
    self.snapshot.scheduled_events.iter().cloned()
  }
}

impl<S: SimulationSpec> GlobalsConstructionAccessor for SfGlobalsConstructionAccessor<S> {}

impl<S: SimulationSpec> Snapshot for SfSnapshot<S> {
  type SimulationSpec = S;
  type EntityHandleKind = SfEntityHandleKind<S>;
  type SnapshotAccessor<'a> = SfSnapshotAccessor<'a, S>;

  fn with_accessor<'a, R>(&'a self, f: impl FnOnce(&Self::SnapshotAccessor<'a>) -> R) -> R {
    let accessor: Self::SnapshotAccessor<'_> = SfSnapshotAccessor {
      guard: accessor_cell::ReadGuard::claim(),
      snapshot: self,
    };
    f(&accessor)
  }
}

// ###################################################
// ############       Steward impls       ############
// ###################################################

impl<S: SimulationSpec> Steward<S> {
  fn wake_entity(&mut self, entity: DynHandle<SfEntityHandleKind<S>>) {
    let guard = accessor_cell::WriteGuard::claim();
    let now = entity
      .wrapped_gat()
      .0
      .universal()
      .schedule
      .current_value(&guard)
      .clone()
      .expect("tried to wake an entity that wasn't scheduled to wake");
    self.last_event = Some(now.clone());
    let globals = self.globals.clone();
    let mut accessor = SfEventAccessor {
      guard,
      now: &now,
      first_waker_universal: entity.wrapped_gat().0.universal(),
      globals: &*globals,
      child_id_generator: EventChildrenIdGenerator::new(),
      woken_now_stack: vec![entity.clone()],
      steward: self,
    };
    while let Some(top) = accessor.woken_now_stack.pop() {
      if top
        .wrapped_gat()
        .0
        .universal()
        .schedule
        .current_value(&accessor.guard)
        .as_ref()
        == Some(accessor.now)
      {
        top.wrapped_gat().0.universal().schedule.replace(
          &mut accessor.guard,
          accessor.now.clone(),
          None,
        );
        assert!(
          accessor.steward.schedule.remove(&ScheduledWake {
            entity: top.clone(),
            time: accessor.now.clone(),
          }),
          "global schedule didn't match entity schedules"
        );
        // Safety: `top` is known to be stored in a triomphe Arc
        unsafe {
          top.wrapped_gat().0.wake(&mut accessor);
        }
      }
    }
  }
}

impl<S: SimulationSpec> TimeSteward for Steward<S> {
  type SimulationSpec = S;
  type EntityHandleKind = SfEntityHandleKind<S>;
  type Snapshot = SfSnapshot<S>;

  fn insert_fiat_event<E: Wake<Self::SimulationSpec>>(
    &mut self,
    time: <Self::SimulationSpec as SimulationSpec>::Time,
    id: EntityId,
    immutable: ImmutableData<E, Self::EntityHandleKind>,
    mutable: MutableData<E, Self::EntityHandleKind>,
  ) -> Result<(), FiatEventOperationError> {
    if matches!(self.earliest_mutable_time(), Some(earliest_mutable) if time < earliest_mutable) {
      return Err(FiatEventOperationError::InvalidTime);
    }
    let entity: TypedHandle<E, SfEntityHandleKind<S>> =
      TypedHandle::from_wrapped_gat(SfTypedHandle::<_, E>(Arc::new(EntityInner {
        universal: EntityUniversal {
          id,
          schedule: History::new(Some(time.clone())),
        },
        immutable,
        mutable: History::new(mutable),
      })));
    let entity: DynHandle<SfEntityHandleKind<S>> = entity.erase();

    self
      .fiat_events
      .try_insert(id, entity.clone())
      .map_err(|_| FiatEventOperationError::InvalidInput)?;

    assert!(
      self.schedule.insert(ScheduledWake { time, entity }),
      "global schedule didn't match entity schedules"
    );
    Ok(())
  }
  fn remove_fiat_event(
    &mut self,
    time: &<Self::SimulationSpec as SimulationSpec>::Time,
    id: EntityId,
  ) -> Result<(), FiatEventOperationError> {
    if matches!(self.earliest_mutable_time(), Some(earliest_mutable) if *time < earliest_mutable) {
      return Err(FiatEventOperationError::InvalidTime);
    }

    let entity = self
      .fiat_events
      .remove(&id)
      .ok_or(FiatEventOperationError::InvalidInput)?;

    assert!(
      self.schedule.remove(&ScheduledWake {
        entity,
        time: time.clone(),
      }),
      "global schedule didn't match entity schedules"
    );
    Ok(())
  }
  fn snapshot_before(
    &mut self,
    time: <Self::SimulationSpec as SimulationSpec>::Time,
  ) -> Option<Self::Snapshot> {
    //println!("Getting snapshot at {:?}", time);
    // since this TimeSteward never discards history, it can always return Some for snapshots
    while let Some(ready_time) = self.latest_time_ready_for_snapshot() {
      //println!("Ready time is {:?}", ready_time);
      if ready_time >= time {
        break;
      }
      self.step(Some(time.clone()));
    }
    Some(SfSnapshot {
      time,
      globals: self.globals.clone(),
      scheduled_events: self.schedule.iter().map(|s| s.entity.clone()).collect(),
    })
  }
  fn step(&mut self, limit: Option<<Self::SimulationSpec as SimulationSpec>::Time>) {
    //println!("Step with limit {:?}", limit);
    if let Some(event) = self.schedule.first().cloned() {
      //println!("Considering event {:?}", event);
      if limit.as_ref().map_or(true, |limit| event.time < *limit) {
        //println!("Running event {:?}", event);
        self.wake_entity(event.entity);
      }
    }
  }
  fn latest_time_ready_for_snapshot(&self) -> Option<S::Time> {
    self.schedule.first().map(|s| s.time.clone())
  }

  fn freeze_before(&mut self, _time: <Self::SimulationSpec as SimulationSpec>::Time) {}
  fn earliest_mutable_time(&self) -> Option<<Self::SimulationSpec as SimulationSpec>::Time> {
    self.last_event.clone()
  }
  fn forget_before(&mut self, _time: <Self::SimulationSpec as SimulationSpec>::Time) {}
  fn earliest_remembered_time(&self) -> Option<<Self::SimulationSpec as SimulationSpec>::Time> {
    None
  }
}

impl<S: SimulationSpec> ConstructibleTimeSteward for Steward<S> {
  fn from_globals(
    _metadata: (),
    globals: Globals<Self::SimulationSpec, Self::EntityHandleKind>,
  ) -> Self {
    Steward {
      globals: Rc::new(globals),
      last_event: None,
      schedule: BTreeSet::new(),
      fiat_events: HashMap::new(),
    }
  }

  fn from_construct_globals<G: ConstructGlobals<Self::SimulationSpec>>(
    metadata: (),
    construct_globals: G,
  ) -> Self {
    Self::from_globals(
      metadata,
      construct_globals.construct_globals(&mut SfGlobalsConstructionAccessor {
        guard: accessor_cell::WriteGuard::claim(),
        child_id_generator: GlobalsConstructionIdGenerator::new(),
        _marker: PhantomData,
      }),
    )
  }

  fn from_serialized<R: Read>(_metadata: (), _data: &mut R) -> ::bincode::Result<Self> {
    unimplemented!()
  }
}

impl<S: SimulationSpec> CanonicalTimeSteward for Steward<S> {}
