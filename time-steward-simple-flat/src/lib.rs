#![feature(
  generic_associated_types,
  specialization,
  min_type_alias_impl_trait,
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
use std::cell::{Ref, RefCell, RefMut};
use std::collections::{BTreeSet, HashMap};
use std::fmt::Debug;
use std::io::Read;
use std::marker::PhantomData;
use std::ops::Deref;
use std::rc::Rc;
use triomphe::{Arc, ArcBorrow};
use unsize::CoerceUnsize;

use time_steward_api::entity_handles::*;
use time_steward_api::*;
use time_steward_implementation_support::{
  EventChildrenIdGenerator, GlobalsConstructionIdGenerator,
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

#[derive(Derivative)]
#[derivative(Debug(bound = ""))]
struct UndoEntry<S: SimulationSpec, T> {
  time: S::Time,
  #[derivative(Debug = "ignore")]
  undo: Box<dyn Fn(&mut T)>,
}

#[derive(Derivative)]
#[derivative(Debug(bound = "T: Debug"))]
struct HistoryInner<S: SimulationSpec, T> {
  current_value: T,
  changes: Vec<UndoEntry<S, T>>,
}

#[derive(Derivative)]
#[derivative(Debug(bound = "T: Debug"))]
struct History<S: SimulationSpec, T>(RefCell<HistoryInner<S, T>>);

impl<S: SimulationSpec, T: SimulationStateData> History<S, T> {
  fn new(starting_value: T) -> History<S, T> {
    History(RefCell::new(HistoryInner {
      current_value: starting_value,
      changes: Vec::new(),
    }))
  }
  fn current_value(&self) -> Ref<T> {
    Ref::map(self.0.borrow(), |history| &history.current_value)
  }
  fn raw_write(&self) -> RefMut<T> {
    RefMut::map(self.0.borrow_mut(), |history| &mut history.current_value)
  }
  fn record_undo(&self, time: S::Time, undo: impl Fn(&mut T) + 'static) {
    self.0.borrow_mut().changes.push(UndoEntry {
      time,
      undo: Box::new(undo),
    });
  }
  fn replace(&self, time: S::Time, new_value: T) {
    let old_value = self.current_value().clone();
    self.record_undo(time, move |m| *m = old_value.clone());
    *self.raw_write() = new_value;
  }
  fn value_before(&self, time: &S::Time) -> T
  where
    T: Clone,
  {
    let history = self.0.borrow();
    let mut value = history.current_value.clone();
    for UndoEntry {
      time: change_time,
      undo,
    } in history.changes.iter().rev()
    {
      if change_time < time {
        break;
      }
      (undo)(&mut value);
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
  now: S::Time,
  first_waker_universal: &'a EntityUniversal<S>,
  globals: Rc<Globals<S, SfEntityHandleKind<S>>>,
  child_id_generator: EventChildrenIdGenerator,
  woken_now_stack: Vec<DynHandle<SfEntityHandleKind<S>>>,
  steward: &'a mut Steward<S>,
}
#[derive(Derivative)]
#[derivative(Debug(bound = ""), Clone(bound = ""))]
pub struct Snapshot<S: SimulationSpec> {
  time: S::Time,
  globals: Rc<Globals<S, SfEntityHandleKind<S>>>,
  scheduled_events: Vec<DynHandle<SfEntityHandleKind<S>>>,
}

#[derive(Derivative)]
#[derivative(Debug(bound = ""))]
pub struct SfGlobalsConstructionAccessor<S: SimulationSpec> {
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
  entity: DynHandle<SfEntityHandleKind<S>>,
  time: S::Time,
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

impl<'c, S: SimulationSpec> Accessor for SfEventAccessor<'c, S> {
  type SimulationSpec = S;
  type EntityHandleKind = SfEntityHandleKind<S>;

  type ReadGuard<'a, T: 'a> = Ref<'a, T>;
  fn raw_read<'a, E: EntityKind>(
    &'a self,
    entity: TypedHandleRef<'a, E, Self::EntityHandleKind>,
  ) -> Self::ReadGuard<'a, MutableData<E, Self::EntityHandleKind>> {
    entity.into_wrapped_gat().0.get().mutable.current_value()
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
      .current_value()
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

impl<S: SimulationSpec> Accessor for Snapshot<S> {
  type SimulationSpec = S;
  type EntityHandleKind = SfEntityHandleKind<S>;

  type ReadGuard<'a, T: 'a> = SnapshotQueryResult<T>;
  fn raw_read<'a, E: EntityKind>(
    &'a self,
    entity: TypedHandleRef<'a, E, Self::EntityHandleKind>,
  ) -> Self::ReadGuard<'a, MutableData<E, Self::EntityHandleKind>> {
    SnapshotQueryResult(entity.into_wrapped_gat().0.mutable.value_before(&self.time))
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
      .value_before(&self.time)
  }
}

impl<S: SimulationSpec> Accessor for SfGlobalsConstructionAccessor<S> {
  type SimulationSpec = S;
  type EntityHandleKind = SfEntityHandleKind<S>;

  type ReadGuard<'a, T: 'a> = Ref<'a, T>;
  fn raw_read<'a, E: EntityKind>(
    &'a self,
    entity: TypedHandleRef<'a, E, Self::EntityHandleKind>,
  ) -> Self::ReadGuard<'a, MutableData<E, SfEntityHandleKind<S>>> {
    entity.into_wrapped_gat().0.get().mutable.current_value()
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
      .current_value()
      .clone()
  }
}

impl<'b, S: SimulationSpec> InitializedAccessor for SfEventAccessor<'b, S> {
  fn globals(&self) -> &Globals<Self::SimulationSpec, Self::EntityHandleKind> {
    &*self.globals
  }
  fn now(&self) -> &<Self::SimulationSpec as SimulationSpec>::Time {
    &self.now
  }
}

impl<S: SimulationSpec> InitializedAccessor for Snapshot<S> {
  fn globals(&self) -> &Globals<Self::SimulationSpec, Self::EntityHandleKind> {
    &*self.globals
  }
  fn now(&self) -> &<Self::SimulationSpec as SimulationSpec>::Time {
    &self.time
  }
}

impl<'b, S: SimulationSpec> CreateEntityAccessor for SfEventAccessor<'b, S> {
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

impl<'c, S: SimulationSpec> EventAccessor for SfEventAccessor<'c, S> {
  type WriteGuard<'a, T: 'a> = RefMut<'a, T>;
  #[allow(clippy::needless_lifetimes)] // Clippy is currently wrong about GATs
  fn map_write_guard<'a, T, U>(
    guard: Self::WriteGuard<'a, T>,
    f: impl FnOnce(&mut T) -> &mut U,
  ) -> Self::WriteGuard<'a, U> {
    RefMut::map(guard, f)
  }

  fn raw_write<'a, E: EntityKind>(
    &'a mut self,
    entity: TypedHandleRef<'a, E, Self::EntityHandleKind>,
  ) -> Self::WriteGuard<'a, MutableData<E, Self::EntityHandleKind>> {
    entity.into_wrapped_gat().0.get().mutable.raw_write()
  }

  // record a way to undo something we did in this event; undo operations may later be run in reverse order
  //
  // calling record_undo isn't undo-safe because things are broken if you undo wrong
  // record_undo also serves the purpose of record_read?
  fn record_undo<E: EntityKind>(
    &self,
    entity: TypedHandleRef<E, Self::EntityHandleKind>,
    undo: impl Fn(&mut MutableData<E, Self::EntityHandleKind>) + 'static,
  ) {
    entity
      .into_wrapped_gat()
      .0
      .mutable
      .record_undo(self.now.clone(), undo);
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
      .current_value()
      .clone();
    if old != time {
      entity
        .into_wrapped_gat()
        .0
        .universal
        .schedule
        .replace(self.now.clone(), time.clone());
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

impl<S: SimulationSpec> SnapshotAccessor for Snapshot<S> {
  type ScheduledEvents<'a> = ScheduledEvents<'a, S>;
  fn scheduled_events(&self) -> Self::ScheduledEvents<'_> {
    self.scheduled_events.iter().cloned()
  }
}

impl<S: SimulationSpec> GlobalsConstructionAccessor for SfGlobalsConstructionAccessor<S> {}

// ###################################################
// ############       Steward impls       ############
// ###################################################

impl<S: SimulationSpec> Steward<S> {
  fn wake_entity(&mut self, entity: DynHandle<SfEntityHandleKind<S>>) {
    let now = entity
      .wrapped_gat()
      .0
      .universal()
      .schedule
      .current_value()
      .clone()
      .expect("tried to wake an entity that wasn't scheduled to wake");
    self.last_event = Some(now.clone());
    let mut accessor = SfEventAccessor {
      now,
      first_waker_universal: entity.wrapped_gat().0.universal(),
      globals: self.globals.clone(),
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
        .current_value()
        .as_ref()
        == Some(&accessor.now)
      {
        top
          .wrapped_gat()
          .0
          .universal()
          .schedule
          .replace(accessor.now.clone(), None);
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
  type SnapshotAccessor = Snapshot<S>;

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
      self.schedule.insert(ScheduledWake { entity, time }),
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
  ) -> Option<Self::SnapshotAccessor> {
    // since this TimeSteward never discards history, it can always return Some for snapshots
    while let Some(ready_time) = self.latest_time_ready_for_snapshot() {
      if ready_time >= time {
        break;
      }
      self.step(Some(time.clone()));
    }
    Some(Snapshot {
      time,
      globals: self.globals.clone(),
      scheduled_events: self.schedule.iter().map(|s| s.entity.clone()).collect(),
    })
  }
  fn step(&mut self, limit: Option<<Self::SimulationSpec as SimulationSpec>::Time>) {
    if let Some(event) = self.schedule.first().cloned() {
      if limit.as_ref().map_or(true, |limit| event.time < *limit) {
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
