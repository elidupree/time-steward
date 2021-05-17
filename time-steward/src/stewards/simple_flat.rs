/*!

The simplest possible implementation of the TimeSteward interface.

This is intended as a reference implementation of the canonical TimeSteward simulation behavior, and its goal is to be as simple as possible, sacrificing such things as "being a FullTimeSteward", "being optimized", and even "preventing memory leaks".

This has 2 purposes:
1. To help the maintainer(s) of the TimeSteward library understand the behavior that the more-optimized TimeSteward implementors are trying to replicate.
2. To be used in automated tests of more-optimized TimeSteward implementors, to guarantee that they *do* in fact replicate the intended behavior.

*/

use derivative::Derivative;
use serde::{de, ser, Deserializer, Serializer};
use std::any::{Any, TypeId};
use std::borrow::Borrow;
use std::cell::{Cell, Ref, RefCell};
use std::cmp::{max, min};
use std::collections::{BTreeMap, BTreeSet};
use std::fmt::Debug;
use std::io::{Read, Write};
use std::rc::Rc;
use std::ops::Deref;
use std::marker::PhantomData;

use crate::api::*;
use crate::implementation_support::common::*;
use crate::implementation_support::insert_only;
use crate::type_utils::{
  DynamicPersistentlyIdentifiedType, PersistentTypeId, PersistentlyIdentifiedType,
};
use crate::EntityId;

// ###################################################
// ############     Handle definitions    ############
// ###################################################
#[derive(Derivative)]
#[derivative(Clone(bound = ""))]
struct SfTypedHandle<S: SimulationSpec, E: EntityKind>(Rc<EntityInner<S, E>>);
#[derive(Clone, PartialEq, Eq, Hash)]
struct SfDynHandle<S: SimulationSpec>(Rc<()>);
pub struct SfEntityHandleKind<S>;
impl<S> EntityHandleKind for SfEntityHandleKind<S> {
  type TypedHandle<E: EntityKind> = SfTypedHandle<S, E>;
  type DynHandle = SfDynHandle<S>;
}

struct EntityUniversal<S: SimulationSpec> {
  id: EntityId,
  schedule: History<S, Option<S::Time>>,
}
struct EntityInner<S: SimulationSpec, E: EntityKind> {
  universal: EntityUniversal<S>,
  immutable: ImmutableData<E, SfEntityHandleKind<S>>,
  mutable: History<S, MutableData<E, SfEntityHandleKind<S>>>,
}
trait AnyEntityInner<S: SimulationSpec> {
  fn universal(&self) -> &EntityUniversal<S>;
  fn wake(&self, self_handle: &SfDynHandle<S>, steward: &mut Steward<S>);
}

#[derive(Debug)]
struct HistoryInner<S: SimulationSpec, T> {
  current_value: T,
  changes: Vec<(S::Time, Box<dyn AnyUndo<T>>)>,
}

struct History<S: SimulationSpec, T>(RefCell<HistoryInner<S, T>>);

#[derive(Debug)]
struct Undo<T, M: Modify<T>> {
  data: M::UndoData,
  _marker: PhantomData<*const T>,
}

trait AnyUndo<T>: ::std::fmt::Debug {
  fn undo(&self, entity: &mut T);
}

impl<T: SimulationStateData, M: Modify<T>> AnyUndo<T> for Undo<T, M> {
  fn undo(&self, entity: &mut T) {
    M::undo(entity.downcast_mut().unwrap(), &self.data);
  }
}

impl<S: SimulationSpec, T> History<S, T> {
  fn new(starting_value: T) -> History<S, T> {
    History(RefCell::new(HistoryInner {
      current_value: starting_value,
      changes: Vec::new(),
    }))
  }
  fn current_value(&self) -> Ref<T> {
    self.0.borrow().map(|history| &history.current_value)
  }
  fn modify<M: Modify<T>>(&self, modification: M, time: S::Time) {
    let history = self.0.borrow_mut();
    history.changes.push((
      time,
      Box::new(modification.modify(&mut history.current_value)),
    ));
  }
  fn value_before(&self, time: S::Time) -> T {
    let history = self.0.borrow();
    let mut value = history.current_value.clone();
    for (time, undo) in history.changes.iter().rev() {
      if time < self.time {
        break;
      }
      undo.undo(value);
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

impl<S: SimulationSpec, E: EntityKind> AnyEntityInner<S> for EntityInner<S, E> {
  fn universal(&self) -> &EntityUniversal<S> {
    &self.0.universal
  }

  fn wake(&self, self_handle: &SfDynHandle<S>, accessor: &mut SfEventAccessor<S>) {
    debug_assert_eq!(
      &self as *const EntityInner<S, E> as *const (),
      &*self_handle.0 as *const AnyEntityInner<S> as *const ()
    );
    let self_handle: SfTypedHandle<S, E> = self_handle.downcast().unwrap();
    <T as Wake<S>>::wake(accessor, &self_handle);
  }
}

impl<S: SimulationSpec, E: EntityKind> EntityHandle for SfDynHandle<S> {
  fn id(&self) -> EntityId {
    self.0.universal().id
  }
}
impl<S: SimulationSpec, E: EntityKind> ser::Serialize for SfTypedHandle<S, E> {
  fn serialize<S: Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
    self.id().serialize(serializer)
  }
}
impl<S: SimulationSpec> ser::Serialize for SfDynHandle<S> {
  fn serialize<S: Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
    self.id().serialize(serializer)
  }
}
impl<'de, S: SimulationSpec, E: EntityKind> de::Deserialize<'de> for SfTypedHandle<S, E> {
  fn deserialize<D: Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
    unimplemented!()
  }
}
impl<'de, S: SimulationSpec> de::Deserialize<'de> for SfDynHandle<S> {
  fn deserialize<D: Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
    unimplemented!()
  }
}

impl<S: SimulationSpec, E: EntityKind> Debug for SfTypedHandle<S, E> {
  fn fmt(&self, f: &mut ::std::fmt::Formatter) -> ::std::fmt::Result {
    thread_local! {
      static RECURSIVE: Cell<bool> = Cell::new(false);
    }
    RECURSIVE.with(|recursive| {
      if recursive.get() {
        write!(f, "TypedHandle({})", self.id())
      } else {
        recursive.set(true);
        defer! { recursive.set(false) }
        write!(f, "TypedHandle({:?})", *self.0)
      }
    })
  }
}

impl<S: SimulationSpec> Debug for SfDynHandle<S> {
  fn fmt(&self, f: &mut ::std::fmt::Formatter) -> ::std::fmt::Result {
    write!(f, "DynHandle({})", self.id())
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
  child_id_generator: RefCell<EventChildrenIdGenerator>,
  woken_now_stack: Vec<DynHandle>,
  steward: RefCell<&'a mut Steward<S>>,
}
#[derive(Derivative)]
#[derivative(Debug(bound = ""), Clone(bound = ""))]
pub struct Snapshot<S: SimulationSpec> {
  time: S::Time,
  globals: Rc<Globals<S, SfEntityHandleKind<S>>>,
  scheduled_events: Vec<SfDynHandle<S>>,
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
  entity: SfDynHandle<S>,
  time: S::Time,
}

type SnapshotsTree<S> = BTreeMap<usize, SnapshotHandle<S>>;

#[derive(Derivative)]
#[derivative(Debug(bound = ""))]
pub struct Steward<S: SimulationSpec> {
  globals: Rc<Globals<S, SfEntityHandleKind<S>>>,
  last_event: Option<S::Time>,
  schedule: BTreeSet<ScheduledWake<S>>,
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

impl<'b, S: SimulationSpec> Accessor for SfEventAccessor<'b, S> {
  type SimulationSpec = S;
  type EntityHandleKind = SfEntityHandleKind<S>;

  fn get_immutable<E: EntityKind>(
    entity: &TypedHandle<E, Self::EntityHandleKind>,
  ) -> &ImmutableData<E, Self::EntityHandleKind> {
    &entity.0.immutable
  }

  type QueryGuard<'a, E: EntityKind> = Ref<'a, MutableData<E, SfEntityHandleKind<S>>>;
  fn query<'a, E: EntityKind>(
    &'a self,
    entity: &'a TypedHandle<E, Self::EntityHandleKind>,
  ) -> Self::QueryGuard<'a, E> {
    entity.mutable.current_value()
  }

  fn query_schedule<E: Wake<Self::SimulationSpec>>(
    &self,
    entity: &TypedHandle<E, Self::EntityHandleKind>,
  ) -> Option<<Self::SimulationSpec as SimulationSpec>::Time> {
    entity.universal.schedule.current_value()
  }
}

impl<'b, S: SimulationSpec> InitializedAccessor for SfEventAccessor<'b, S> {
  fn globals(&self) -> &Globals<Self::SimulationSpec, Self::EntityHandleKind> {
    &*self.globals
  }
  fn now(&self) -> &<Self::SimulationSpec as SimulationSpec>::Time {
    self.now
  }
}

struct SnapshotQueryResult<T>(T);
impl<T> Deref for SnapshotQueryResult<T> {
  type Target = T;

  fn deref(&self) -> &Self::Target {
    &self.0
  }
}

impl<S: SimulationSpec> Accessor for Snapshot<S> {
  type SimulationSpec = S;
  type EntityHandleKind = SfEntityHandleKind<S>;

  fn get_immutable<E: EntityKind>(
    entity: &TypedHandle<E, Self::EntityHandleKind>,
  ) -> &ImmutableData<E, Self::EntityHandleKind> {
    &entity.0.immutable
  }

  type QueryGuard<'a, E: EntityKind> = SnapshotQueryResult<MutableData<E, SfEntityHandleKind<S>>>;
  fn query<'a, E: EntityKind>(
    &'a self,
    entity: &'a TypedHandle<E, Self::EntityHandleKind>,
  ) -> Self::QueryGuard<'a, E> {
    SnapshotQueryResult(entity.mutable.value_before(self.time))
  }

  fn query_schedule<E: Wake<Self::SimulationSpec>>(
    &self,
    entity: &TypedHandle<E, Self::EntityHandleKind>,
  ) -> Option<<Self::SimulationSpec as SimulationSpec>::Time> {
    entity.universal.schedule.value_before(self.time)
  }
}

impl<S: SimulationSpec> InitializedAccessor for Snapshot<S> {
  fn globals(&self) -> &Globals<Self::SimulationSpec, Self::EntityHandleKind> {
    &*self.globals
  }
  fn now(&self) -> &<Self::SimulationSpec as SimulationSpec>::Time {
    self.time
  }
}

impl<'b, S: SimulationSpec> CreateEntityAccessor for SfEventAccessor<'b, S> {
  fn create_entity<E: EntityKind>(
    &self,
    immutable: ImmutableData<E, Self::EntityHandleKind>,
    mutable: MutableData<E, Self::EntityHandleKind>,
  ) -> TypedHandle<E, Self::EntityHandleKind> {
    let id = self
      .child_id_generator
      .next(self.first_waker_universal.id, self.now);
    TypedHandle(Rc::new(EntityInner {
      universal: EntityUniversal {
        id,
        schedule: History::new(None),
      },
      immutable,
      mutable: History::new(mutable),
    }))
  }
}

impl<'b, S: SimulationSpec> EventAccessor for SfEventAccessor<'b, S> {
  fn modify<'a, E: EntityKind, M: Modify<MutableData<E, Self::EntityHandleKind>>>(
    &'a self,
    entity: &'a TypedHandle<E, Self::EntityHandleKind>,
    modification: M,
  ) {
    entity.mutable.modify(modification);
  }

  fn set_schedule<E: Wake<Self::SimulationSpec>>(
    &self,
    entity: &TypedHandle<E, Self::EntityHandleKind>,
    time: Option<<Self::SimulationSpec as SimulationSpec>::Time>,
  ) {
    let old = *entity.universal.schedule.current_value();
    if old != time {
      entity.schedule.modify(modification);
      let global_schedule = self.steward.borrow_mut();
      if let Some(old_time) = old {
        assert!(
          global_schedule.remove(ScheduledWake {
            entity: entity.clone(),
            time: old_time,
          }),
          "global schedule didn't match entity schedules"
        );
      }
      if let Some(new_time) = time {
        assert!(
          global_schedule.insert(ScheduledWake {
            entity: entity.clone(),
            time: new_time,
          }),
          "global schedule didn't match entity schedules"
        );
      }
    }
    if time == Some(self.now.clone()) {
      self.woken_now_stack.push(entity);
    }
  }
}

impl<S: SimulationSpec> SnapshotAccessor for Snapshot<S> {
  type ScheduledEvents = ();
  fn scheduled_events(&self) -> Self::ScheduledEvents {}
}

// ###################################################
// ############       Steward impls       ############
// ###################################################

impl<S: SimulationSpec> Steward<S> {
  fn next_event(&self) -> Option<&EventHandle<S>> {
    let first_fiat_event_iter = self.upcoming_fiat_events.iter().take(1);
    let first_predicted_event_iter = self.existent_predictions.iter().take(1);
    let events_iter = first_fiat_event_iter.chain(first_predicted_event_iter);
    events_iter.min().map(|a| &a.0)
  }

  fn wake_entity(&mut self, entity: &DynHandle<S>) {
    let now = entity
        .universal()
        .schedule
        .current_value()
        .clone()
        .expect("tried to wake an entity that wasn't scheduled to wake");
    self.last_event = Some(now.clone());
    self.universal.schedule.modify(ReplaceWith(None));
    assert!(
      self.schedule.remove(ScheduledWake {
        entity: entity.clone(),
        time: now,
      }),
      "global schedule didn't match entity schedules"
    );
    let mut accessor = SfEventAccessor {
      now,
      first_waker_universal: entity.universal(),
      globals: steward.globals.clone(),
      child_id_generator: RefCell::new(EventChildrenIdGenerator::new()),
      woken_now_stack: vec![entity],
      steward: RefCell::new(steward),
    };
    while let Some(top) = accessor.woken_now_stack.pop() {
      top.wake(top, &mut accessor);
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
    if self.valid_since() > time {
      return Err(FiatEventOperationError::InvalidTime);
    }
    let entity = TypedHandle(Rc::new(EntityInner {
      universal: EntityUniversal {
        id,
        schedule: History::new(Some(time)),
      },
      immutable,
      mutable: History::new(mutable),
    }));

    assert!(
      global_schedule.insert(ScheduledWake {
        entity: entity.clone(),
        time,
      }),
      "global schedule didn't match entity schedules"
    );
  }
  fn remove_fiat_event(
    &mut self,
    time: &<Self::SimulationSpec as SimulationSpec>::Time,
    id: EntityId,
  ) -> Result<(), FiatEventOperationError> {
    if self.valid_since() > time {
      return Err(FiatEventOperationError::InvalidTime);
    }

    assert!(
      global_schedule.remove(ScheduledWake {
        entity: entity.clone(),
        time,
      }),
      "global schedule didn't match entity schedules"
    );
  }
  fn snapshot_before(
    &mut self,
    time: &<Self::SimulationSpec as SimulationSpec>::Time,
  ) -> Option<Self::SnapshotAccessor> {
    // since this TimeSteward never discards history, it can always return Some for snapshots
    while let Some(updated) = self.updated_until_before() {
      if updated >= *time {
        break;
      }
      self.step();
    }
    Some(Snapshot {time, globals: self.globals.clone(), scheduled_events: self.schedule.iter().map(|s| s.entity).collect()})
  }

  fn valid_since(&self) -> ValidSince<<Self::SimulationSpec as SimulationSpec>::Time>{
    match self.last_event {
        None => ValidSince::TheBeginning,
        Some(time) => ValidSince::After(time)),
      }
  }
  fn forget_before(&mut self, time: &<Self::SimulationSpec as SimulationSpec>::Time){}



  fn insert_fiat_event<E: Event<Self>>(
    &mut self,
    time: S::Time,
    id: EntityId,
    event: E,
  ) -> Result<(), FiatEventOperationError> {
    if self.valid_since() > time {
      return Err(FiatEventOperationError::InvalidTime);
    }
    match self
      .upcoming_fiat_events
      .insert(TimeOrderedEventHandle(DataHandle::new_nonreplicable(
        (),
        EventInner {
          time: extended_time_of_fiat_event(time, id),
          data: Box::new(event),
        },
      ))) {
      true => Ok(()),
      false => Err(FiatEventOperationError::InvalidInput),
    }
  }

  fn remove_fiat_event(
    &mut self,
    time: &S::Time,
    id: EntityId,
  ) -> Result<(), FiatEventOperationError> {
    if self.valid_since() > *time {
      return Err(FiatEventOperationError::InvalidTime);
    }
    match self
      .upcoming_fiat_events
      .remove(&extended_time_of_fiat_event(time.clone(), id))
    {
      false => Err(FiatEventOperationError::InvalidInput),
      true => Ok(()),
    }
  }
}

impl<S: SimulationSpec> ConstructibleTimeSteward for Steward<S> {
  fn from_globals(
    metadata: (),
    globals: Globals<Self::SimulationSpec, Self::EntityHandleKind>,
  ) -> Self{
    Steward {
      globals: Rc::new(globals),
      last_event: None,
      schedule: BTreeSet::new()
    }
  }

  fn from_construct_globals<G: ConstructGlobals<Self::SimulationSpec>>(
    metadata: (),
    construct_globals: G,
  ) -> Self {}

  fn from_serialized<R: Read>(metadata: Metadata, data: &mut R) -> ::bincode::Result<Self> {
    unimplemented!()
  }
}

impl<S: SimulationSpec> IncrementalTimeSteward for Steward<S> {
  fn step(&mut self) {
    if let Some(event) = self.next_event().cloned() {
      self.execute_event(&event);
    }
  }
  fn updated_until_before(&self) -> Option<S::Time> {
    self.schedule.first()
      .map(|s| s.time)
  }
}
impl<S: SimulationSpec> CanonicalTimeSteward for Steward<S> {}
