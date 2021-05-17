/*!

The simplest possible implementation of the TimeSteward interface.

This is intended as a reference implementation of the canonical TimeSteward simulation behavior, and its goal is to be as simple as possible, sacrificing such things as "being a FullTimeSteward", "being optimized", and even "preventing memory leaks".

This has 2 purposes:
1. To help the maintainer(s) of the TimeSteward library understand the behavior that the more-optimized TimeSteward implementors are trying to replicate.
2. To be used in automated tests of more-optimized TimeSteward implementors, to guarantee that they *do* in fact replicate the intended behavior.

*/

use derivative::Derivative;
use scopeguard::defer;
use serde::{de, Deserializer};
use std::cell::{Cell, Ref, RefCell};
use std::collections::{BTreeMap, BTreeSet};
use std::fmt::Debug;
use std::io::Read;
use std::marker::PhantomData;
use std::ops::Deref;
use std::rc::Rc;

use crate::api::*;
use crate::implementation_support::common::*;
use crate::EntityId;

// ###################################################
// ############     Handle definitions    ############
// ###################################################
#[derive(Derivative)]
#[derivative(Clone(bound = ""))]
struct SfTypedHandle<S: SimulationSpec, E: EntityKind>(Rc<EntityInner<S, E>>);
#[derive(Derivative)]
#[derivative(Clone(bound = ""))]
struct SfDynHandle<S: SimulationSpec>(Rc<dyn AnyEntityInner<S>>);
struct SfEntityHandleKind<S>(PhantomData<*const S>, !);
impl<S: SimulationSpec> EntityHandleKind for SfEntityHandleKind<S> {
  type TypedHandle<E: EntityKind> = SfTypedHandle<S, E>;
  type DynHandle = SfDynHandle<S>;
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
trait AnyEntityInner<S: SimulationSpec> {
  fn universal(&self) -> &EntityUniversal<S>;
  fn wake(&self, self_handle: &SfDynHandle<S>, accessor: &mut SfEventAccessor<S>);
}

#[derive(Derivative)]
#[derivative(Debug(bound = "T: Debug"))]
struct HistoryInner<S: SimulationSpec, T> {
  current_value: T,
  changes: Vec<(S::Time, Box<dyn AnyUndo<T>>)>,
}

#[derive(Derivative)]
#[derivative(Debug(bound = "T: Debug"))]
struct History<S: SimulationSpec, T>(RefCell<HistoryInner<S, T>>);

#[derive(Debug)]
struct Undo<T, M: Modify<T>> {
  data: M::UndoData,
  _marker: PhantomData<*const T>,
}

trait AnyUndo<T>: Debug {
  fn undo(&self, entity: &mut T);
}

impl<T: SimulationStateData, M: Modify<T>> AnyUndo<T> for Undo<T, M> {
  fn undo(&self, entity: &mut T) {
    M::undo(entity, &self.data);
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
    Ref::map(self.0.borrow(), |history| &history.current_value)
  }
  fn modify<M: Modify<T>>(&self, modification: M, time: S::Time) {
    let history = self.0.borrow_mut();
    history.changes.push((
      time,
      Box::new(modification.modify(&mut history.current_value)),
    ));
  }
  fn value_before(&self, time: S::Time) -> T
  where
    T: Clone,
  {
    let history = self.0.borrow();
    let mut value = history.current_value.clone();
    for (change_time, undo) in history.changes.iter().rev() {
      if *change_time < time {
        break;
      }
      undo.undo(&mut value);
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
    &self.universal
  }

  fn wake(&self, self_handle: &SfDynHandle<S>, accessor: &mut SfEventAccessor<S>) {
    debug_assert_eq!(
      self as *const EntityInner<S, E> as *const (),
      &*self_handle.0 as *const dyn AnyEntityInner<S> as *const ()
    );
    let self_handle: SfTypedHandle<S, E> = self_handle.downcast().unwrap();
    <E as Wake<S>>::wake(accessor, self_handle);
  }
}

impl<S: SimulationSpec> EntityHandle for SfDynHandle<S> {
  fn id(&self) -> EntityId {
    self.0.universal().id
  }
}

delegate! (
  [S: SimulationSpec, E: EntityKind]
  [PartialEq, Eq, PartialOrd, Ord, Hash, Serialize]
  for [SfTypedHandle<S, E>]
  to [this => &this.id()]
);
delegate! (
  [S: SimulationSpec]
  [PartialEq, Eq, PartialOrd, Ord, Hash, Serialize]
  for [SfDynHandle<S>]
  to [this => &this.id()]
);

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
  woken_now_stack: Vec<SfDynHandle<S>>,
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
    entity.0.mutable.current_value()
  }

  fn query_schedule<E: Wake<Self::SimulationSpec>>(
    &self,
    entity: &TypedHandle<E, Self::EntityHandleKind>,
  ) -> Option<<Self::SimulationSpec as SimulationSpec>::Time> {
    entity.0.universal.schedule.current_value().clone()
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
    SnapshotQueryResult(entity.0.mutable.value_before(self.time))
  }

  fn query_schedule<E: Wake<Self::SimulationSpec>>(
    &self,
    entity: &TypedHandle<E, Self::EntityHandleKind>,
  ) -> Option<<Self::SimulationSpec as SimulationSpec>::Time> {
    entity.0.universal.schedule.value_before(self.time)
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
    &self,
    immutable: ImmutableData<E, Self::EntityHandleKind>,
    mutable: MutableData<E, Self::EntityHandleKind>,
  ) -> TypedHandle<E, Self::EntityHandleKind> {
    let id = self
      .child_id_generator
      .borrow_mut()
      .next(&self.first_waker_universal.id, self.now);
    SfTypedHandle(Rc::new(EntityInner {
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
    entity.0.mutable.modify(modification, self.now.clone());
  }

  fn set_schedule<E: Wake<Self::SimulationSpec>>(
    &self,
    entity: &TypedHandle<E, Self::EntityHandleKind>,
    time: Option<<Self::SimulationSpec as SimulationSpec>::Time>,
  ) {
    let old = *entity.0.universal.schedule.current_value();
    if old != time {
      entity
        .0
        .universal
        .schedule
        .modify(ReplaceWith(time), self.now.clone());
      let steward = self.steward.borrow_mut();
      if let Some(old_time) = old {
        assert!(
          steward.schedule.remove(&ScheduledWake {
            entity: entity.clone(),
            time: old_time,
          }),
          "global schedule didn't match entity schedules"
        );
      }
      if let Some(new_time) = time {
        assert!(
          steward.schedule.insert(ScheduledWake {
            entity: entity.clone(),
            time: new_time,
          }),
          "global schedule didn't match entity schedules"
        );
      }
    }
    if time == Some(self.now.clone()) && entity.id() <= self.first_waker_universal.id {
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
  fn wake_entity(&mut self, entity: SfDynHandle<S>) {
    let now = entity
      .0
      .universal()
      .schedule
      .current_value()
      .clone()
      .expect("tried to wake an entity that wasn't scheduled to wake");
    self.last_event = Some(now.clone());
    let mut accessor = SfEventAccessor {
      now,
      first_waker_universal: entity.0.universal(),
      globals: self.globals.clone(),
      child_id_generator: RefCell::new(EventChildrenIdGenerator::new()),
      woken_now_stack: vec![entity],
      steward: RefCell::new(self),
    };
    while let Some(top) = accessor.woken_now_stack.pop() {
      top
        .0
        .universal()
        .schedule
        .modify(ReplaceWith(None), now.clone());
      assert!(
        self.schedule.remove(&ScheduledWake {
          entity: top.clone(),
          time: now,
        }),
        "global schedule didn't match entity schedules"
      );
      top.0.wake(top, &mut accessor);
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
    let entity = SfTypedHandle(Rc::new(EntityInner {
      universal: EntityUniversal {
        id,
        schedule: History::new(Some(time)),
      },
      immutable,
      mutable: History::new(mutable),
    }));

    assert!(
      self.schedule.insert(ScheduledWake {
        entity: entity.clone(),
        time,
      }),
      "global schedule didn't match entity schedules"
    );
    Ok(())
  }
  fn remove_fiat_event(
    &mut self,
    time: &<Self::SimulationSpec as SimulationSpec>::Time,
    id: EntityId,
  ) -> Result<(), FiatEventOperationError> {
    if self.valid_since() > *time {
      return Err(FiatEventOperationError::InvalidTime);
    }

    assert!(
      self.schedule.remove(&ScheduledWake {
        entity: entity.clone(),
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
    while let Some(updated) = self.updated_until_before() {
      if updated >= time {
        break;
      }
      self.step();
    }
    Some(Snapshot {
      time,
      globals: self.globals.clone(),
      scheduled_events: self.schedule.iter().map(|s| s.entity).collect(),
    })
  }

  fn valid_since(&self) -> ValidSince<<Self::SimulationSpec as SimulationSpec>::Time> {
    match self.last_event {
      None => ValidSince::TheBeginning,
      Some(time) => ValidSince::After(time),
    }
  }
  fn forget_before(&mut self, time: &<Self::SimulationSpec as SimulationSpec>::Time) {}
}

impl<S: SimulationSpec> ConstructibleTimeSteward for Steward<S> {
  fn from_globals(
    metadata: (),
    globals: Globals<Self::SimulationSpec, Self::EntityHandleKind>,
  ) -> Self {
    Steward {
      globals: Rc::new(globals),
      last_event: None,
      schedule: BTreeSet::new(),
    }
  }

  fn from_construct_globals<G: ConstructGlobals<Self::SimulationSpec>>(
    metadata: (),
    construct_globals: G,
  ) -> Self {
    unimplemented!()
  }

  fn from_serialized<R: Read>(metadata: (), data: &mut R) -> ::bincode::Result<Self> {
    unimplemented!()
  }
}

impl<S: SimulationSpec> IncrementalTimeSteward for Steward<S> {
  fn step(&mut self) {
    if let Some(event) = self.schedule.first().cloned() {
      self.wake_entity(event.entity);
    }
  }
  fn updated_until_before(&self) -> Option<S::Time> {
    self.schedule.first().map(|s| s.time)
  }
}
impl<S: SimulationSpec> CanonicalTimeSteward for Steward<S> {}
