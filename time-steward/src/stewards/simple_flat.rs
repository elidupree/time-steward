use std::any::{Any, TypeId};
use std::borrow::Borrow;
use std::cell::{Cell, Ref, RefCell};
use std::cmp::{max, Ordering};
use std::collections::{BTreeMap, BTreeSet, Bound, HashMap};
use std::fmt::Debug;
use std::hash::{Hash, Hasher};
use std::io::{Read, Write};
use std::mem;
use std::ops::Deref;
use std::rc::Rc;

use super::super::api::*;
use super::super::implementation_support::common::*;
use DeterministicRandomId;
use type_utils::{PersistentlyIdentifiedType, DynamicPersistentlyIdentifiedType, try_identity};

use implementation_support::insert_only;

//time_steward_steward_specific_api!();

thread_local! {
  static NEXT_SERIAL_NUMBER: Cell <usize> = Cell::new (0);
}
fn new_serial_number() -> usize {
  NEXT_SERIAL_NUMBER.with(|cell| {
    let result = cell.get();
    cell.set(result + 1);
    result
  })
}


#[derive(Derivative)]
#[derivative(Clone(bound = ""))]
pub struct DataHandle<T: SimulationStateData + PersistentlyIdentifiedType> {
  data: Rc<T>,
}



#[derive(Debug)]
pub struct EntityCell<T> {
  serial_number: usize,
  data: RefCell<EntityCellInner<T>>,
}

#[derive(Debug)]
pub struct EntityCellInner<T> {
  current_value: T,
  history_head: HistoryIndex,
}



#[derive(Derivative)]
#[derivative(Clone(bound = ""))]
pub struct EventHandle<S: SimulationSpec> {
  data: Rc<EventInner<S>>,
}

#[derive(Debug)]
struct EventInner<S: SimulationSpec> {
  time: ExtendedTime<S>,
  data: Box<EventInnerTrait<S>>,
}

trait EventInnerTrait<S: SimulationSpec>:
  Any + Debug + SerializeInto + DynamicPersistentlyIdentifiedType
{
  fn execute(&self, self_handle: &EventHandle<S>, steward: &mut Steward<S>);
  fn get_type_id(&self)->TypeId;
}

impl<S: SimulationSpec, T: Event<Steward = Steward<S>>> EventInnerTrait<S> for T {
  fn execute(&self, self_handle: &EventHandle<S>, steward: &mut Steward<S>) {
    let mut accessor = EventAccessorStruct {
      handle: self_handle.clone(),
      globals: steward.globals.clone(),
      child_id_generator: EventChildrenIdGenerator::new(),
      steward: steward,
    };
    <T as Event>::execute(self, &mut accessor);
  }
  fn get_type_id(&self)->TypeId {TypeId::of::<T>()}
}




#[derive(Debug)]
pub struct EventAccessorStruct<'a, S: SimulationSpec> {
  handle: EventHandle<S>,
  globals: Rc<S::Globals>,
  child_id_generator: EventChildrenIdGenerator,
  steward: &'a mut Steward<S>,
}
#[derive(Debug)]
pub struct SnapshotInner<S: SimulationSpec> {
  index: usize,
  time: ExtendedTime<S>,
  globals: Rc<S::Globals>,
  clones: insert_only::HashMap<usize, Box<Any>>,
  shared: Rc<RefCell<StewardShared<S>>>,
}
#[derive(Debug, Clone)]
pub struct SnapshotHandle<S: SimulationSpec> {
  data: Rc<SnapshotInner<S>>,
}


#[derive(Debug)]
pub struct Steward<S: SimulationSpec> {
  globals: Rc<S::Globals>,
  invalid_before: ValidSince<S::Time>,
  last_event: Option<ExtendedTime<S>>,
  upcoming_fiat_events: BTreeSet<EventHandle<S>>,
  existent_predictions: BTreeSet<EventHandle<S>>,
  shared: Rc<RefCell<StewardShared<S>>>,
  next_snapshot_index: usize,
}

#[derive(Debug)]
struct StewardShared<S: SimulationSpec> {
  snapshots: SnapshotsTree<S>,
  history: History<S>,
}


pub type QueryGuard<'a, T> = Ref<'a, T>;


mod history {
use super::EventHandle;
use std::collections::VecDeque;
use std::any::Any;
use std::marker::PhantomData;
use crate::{SimulationSpec, Modify, ExtendedTime, EventHandleTrait, Entity};

#[derive (Copy, Clone, Debug, Default)]
pub struct HistoryIndex (usize);


#[derive(Debug)]
struct HistoryItem<S: SimulationSpec>  {
  event: EventHandle<S>,
  undo: Box <dyn AnyUndo>,
  previous: HistoryIndex,
}

#[derive(Debug)]
struct Undo <T:Entity,M: Modify <T>> {
  data: M::UndoData,
  _marker: PhantomData <*const T>,
}

trait AnyUndo: ::std::fmt::Debug {
  fn undo (&self, entity: &mut dyn Any);
}

impl<T: Entity,M: Modify <T>> AnyUndo for Undo <T, M> {
  fn undo (&self, entity: &mut dyn Any) {
    M::undo (entity.downcast_mut().unwrap(), &self.data);
  }
}

#[derive(Debug)]
pub struct History<S: SimulationSpec>  {
  items: VecDeque <HistoryItem<S>>,
  start: usize,
}

impl HistoryIndex {
  pub fn null()->HistoryIndex {HistoryIndex (0)}
}

impl<S: SimulationSpec>  History<S>{
  pub fn new ()->History <S> {
    History {
      items: VecDeque::new(),
      start: 1,
    }
  }

  pub fn insert_modification <T:Entity, M: Modify <T>> (&mut self, head: &mut HistoryIndex, event: EventHandle<S>, undo: M::UndoData) {
    let undo: Undo <T, M> = Undo {data: undo,_marker: PhantomData};
    let item = HistoryItem {
      event, undo: Box::new (undo), previous: *head
    };
    *head = HistoryIndex (self.start + self.items.len());
    self.items.push_back (item);
  }
  
  fn get (&self, index: HistoryIndex)->Option <& HistoryItem <S>> {
    self.items.get (index.0.wrapping_sub(self.start))
  }
  
  pub fn forget_before(&mut self, time: ExtendedTime <S>) {
    while let Some (first) = self.items.pop_front() {
      if *first.event.extended_time() >= time {
        self.items.push_front (first);
        break;
      }
    }
  }
  
  pub fn rollback_to_before <T: Entity> (&self, head: HistoryIndex, current_value: &T, time: &ExtendedTime <S>) -> T {
    let mut value = current_value.clone();
    let mut index = head;
    while let Some (item) = self.get (index) {
      if item.event.extended_time() < time {break}
      item.undo.undo (&mut value);
      index = item.previous;
    }
    value
  }
}

}

use self::history::{History, HistoryIndex};



impl<S: SimulationSpec> EventHandleTrait<S> for EventHandle<S> {
  fn extended_time(&self) -> &ExtendedTime<S> {
    &self.data.time
  }
  fn downcast_ref<T: Any>(&self) -> Option<&T> {
    downcast_ref!(&*self.data.data, T, EventInnerTrait<S>)
  }
}

impl<T: SimulationStateData + PersistentlyIdentifiedType> DataHandleTrait<T> for DataHandle<T> {
  fn new_for_globals(data: T) -> Self {
    DataHandle {
      data: Rc::new(data),
    }
  }
}
impl<T: Entity> EntityCellTrait<T> for EntityCell<T> {
  fn new(data: T) -> Self {
    EntityCell {
      serial_number: new_serial_number(),
      data: RefCell::new(EntityCellInner {current_value: data, history_head: HistoryIndex::null()}),
    }
  }
}

impl<T: SimulationStateData + PersistentlyIdentifiedType> Deref for DataHandle<T> {
  type Target = T;
  fn deref(&self) -> &T {
    &*self.data
  }
}

time_steward_common_impls_for_handles!();
time_steward_common_impls_for_uniquely_identified_handle! ([T: SimulationStateData + PersistentlyIdentifiedType] [DataHandle <T>] self => (&*self.data as *const T): *const T);
time_steward_common_impls_for_uniquely_identified_handle! ([T: Entity] [EntityCell <T>] self => (self.serial_number): usize);

time_steward_serialization_impls!();
fn deserialization_create_event_inner<S: SimulationSpec, T: Event<Steward = Steward<S>>>(
  time: ExtendedTime<S>,
  data: T,
  _in_future: bool,
) -> EventInner<S> {
  EventInner {
    time: time,
    data: Box::new(data),
  }
}
fn deserialization_create_prediction<S: SimulationSpec>(
  steward: &mut Steward<S>,
  prediction: EventHandle<S>,
) {
  steward.existent_predictions.insert(prediction);
}


impl<S: SimulationSpec> SnapshotHandle<S> {
  fn get_clone<T: Entity>(
    &self,
    entity: &EntityCell<T>,
  ) -> &T {
    self.data.clones.get_default (entity.serial_number, | | {
      let data = entity.data.borrow();
      Some(Box::new (
        (*self.data.shared).borrow().history.rollback_to_before (data.history_head, & data.current_value, & self.data.time)
      ))
    }).unwrap ().downcast_ref::<T>().expect("A clone in a snapshot was a different type than what it was supposed to be a clone of; maybe two different timelines got the same serial number somehow")
  }
}

type SnapshotsTree<S> = BTreeMap<usize, SnapshotHandle<S>>;

impl<S: SimulationSpec> Drop for SnapshotHandle<S> {
  fn drop(&mut self) {
    assert!(Rc::strong_count(&self.data) >= 2);
    // if we are the last one dropped, our data still exists, and so does the entry in the tree
    if Rc::strong_count(&self.data) == 2 {
      // when we drop the one from the map recursively, that one will also observe a strong count of 2, so short-circuit it
      if let Ok(mut shared) = self.data.shared.try_borrow_mut() {
        shared.snapshots.remove(&self.data.index);
      }
    }
  }
}

impl<'b, S: SimulationSpec> Accessor for EventAccessorStruct<'b, S> {
  type Steward = Steward<S>;
  fn globals(&self) -> &S::Globals {
    &*self.globals
  }
  fn extended_now(&self) -> &ExtendedTime<<Self::Steward as TimeSteward>::SimulationSpec> {
    self.this_event().extended_time()
  }
  fn query <'a, E: Entity, C: EntityCellTrait<E>> (&'a self, entity: &'a C)-><Self::Steward as TimeStewardEntityCell<'a, E, C>>::QueryGuard
    where Self::Steward: TimeStewardEntityCell<'a, E, C> {
    let guard = try_identity::<&C, & EntityCell <E>>(entity).unwrap().data.borrow();
    try_identity::<QueryGuard<'a, _>, <Self::Steward as TimeStewardEntityCell<'a, E, C>>::QueryGuard>(guard).unwrap()
  }
}
impl<S: SimulationSpec> Accessor for SnapshotHandle<S> {
  type Steward = Steward<S>;
  fn globals(&self) -> &S::Globals {
    &self.data.globals
  }
  fn extended_now(&self) -> &ExtendedTime<<Self::Steward as TimeSteward>::SimulationSpec> {
    &self.data.time
  }
  fn query <'a, E: Entity, C: EntityCellTrait<E>> (&'a self, entity: &'a C)-><Self::Steward as TimeStewardEntityCell<'a, E, C>>::QueryGuard
    where Self::Steward: TimeStewardEntityCell<'a, E, C> {
    // hack: we could return just &T, but unfortunately my workarounds force us to return Ref
    let entity = try_identity::<&C, & EntityCell <E>>(entity).unwrap();
    let clone: &'a E = self.get_clone(entity);
    let guard: QueryGuard<'a, _> = entity.data.borrow();
    let guard = Ref::map(guard, move |_| clone);
    try_identity::<QueryGuard<'a, E>, <Self::Steward as TimeStewardEntityCell<'a, E, C>>::QueryGuard>(guard).unwrap()
  }
}
impl<'b, S: SimulationSpec> EventAccessor for EventAccessorStruct<'b, S> {
  fn this_event(&self) -> &EventHandle<S> {
    &self.handle
  }

  fn new_handle<T: SimulationStateData + PersistentlyIdentifiedType>(
    &self,
    data: T,
  ) -> <Self::Steward as TimeStewardDataHandle <T>>::DataHandle where Self::Steward: TimeStewardDataHandle <T> {
    try_identity(DataHandle {
      data: Rc::new(data),
    }).unwrap()
  }
  
  fn modify <'a, M: Modify<E>, E: Entity, C: EntityCellTrait<E>> (&'a self, entity: &'a C, modification: M)
    where Self::Steward: TimeStewardEntityCell<'a, E, C> {
    let mut modify_guard = try_identity::<&C, & EntityCell <E>>(entity).unwrap().data.borrow_mut();
    self.steward.shared.borrow_mut().history.insert_modification(&mut modify_guard.history_head, self.handle.clone(), modification.modify (&mut modify_guard.current_value));
  }


  fn create_prediction<E: Event<Steward = Self::Steward>>(
    &self,
    time: <<Self::Steward as TimeSteward>::SimulationSpec as SimulationSpec>::Time,
    event: E,
  ) -> EventHandle<S> {
    let time = extended_time_of_predicted_event::<<Self::Steward as TimeSteward>::SimulationSpec>(
      time,
      self.child_id_generator.next(&self.handle.id()),
      self.extended_now(),
    )
    .unwrap();
    let prediction = EventHandle {
      data: Rc::new(EventInner {
        time: time,
        data: Box::new(event),
      }),
    };
    assert!(
        self
          .steward
          .existent_predictions
          .insert(prediction.clone()),
        "created a prediction that already existed?!"
      );
    //printlnerr!("at {:?}, creating prediction at {:?}", self.extended_now(), handle.extended_time());
    prediction
  }
  fn destroy_prediction(&self, prediction: &<Self::Steward as TimeSteward>::EventHandle) {
    assert!(
        self
          .steward
          .existent_predictions
          .remove(prediction),
        "destroyed a prediction that didn't exist"
      );
  }
}

impl<S: SimulationSpec> SnapshotAccessor for SnapshotHandle<S> {
  fn serialize_into<W: Write>(&self, writer: &mut W) -> ::bincode::Result<()> {
    serialize_snapshot(writer, self.clone())
  }
}


impl<S: SimulationSpec> Steward<S> {
  fn next_event(&self) -> Option<&EventHandle<S>> {
    let first_fiat_event_iter = self.upcoming_fiat_events.iter().take(1);
    let first_predicted_event_iter = self.existent_predictions.iter().take(1);
    let events_iter = first_fiat_event_iter.chain(first_predicted_event_iter);
    events_iter.min()
  }

  fn execute_event(&mut self, event: &EventHandle<S>) {
    event.data.data.execute(event, &mut *self);
    // clean it up:
    self.upcoming_fiat_events.remove(event);
    self.existent_predictions.remove(event);
    self.last_event = Some(event.extended_time().clone());
  }
}

impl<S: SimulationSpec> TimeSteward for Steward<S> {
  type SimulationSpec = S;
  type SnapshotAccessor = SnapshotHandle<S>;
  type EventHandle = EventHandle<S>;

  fn valid_since(&self) -> ValidSince<S::Time> {
    max(
      self.invalid_before.clone(),
      match self.last_event {
        None => ValidSince::TheBeginning,
        Some(ref time) => ValidSince::After(time.base.clone()),
      },
    )
  }

  fn insert_fiat_event<E: Event<Steward = Self>>(
    &mut self,
    time: S::Time,
    id: DeterministicRandomId,
    event: E,
  ) -> Result<(), FiatEventOperationError> {
    if self.valid_since() > time {
      return Err(FiatEventOperationError::InvalidTime);
    }
    match self.upcoming_fiat_events.insert(EventHandle {
      data: Rc::new(EventInner {
        time: extended_time_of_fiat_event(time, id),
        data: Box::new(event),
      }),
    }) {
      true => Ok(()),
      false => Err(FiatEventOperationError::InvalidInput),
    }
  }

  fn remove_fiat_event(
    &mut self,
    time: &S::Time,
    id: DeterministicRandomId,
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

  fn snapshot_before(&mut self, time: &S::Time) -> Option<Self::SnapshotAccessor> {
    // NOT self.valid_since(); this Steward can continue recording snapshots from earlier than the earliest time it can accept fiat event input
    if self.invalid_before > *time {
      return None;
    }
    while let Some(updated) = self.updated_until_before() {
      if updated >= *time {
        break;
      }
      self.step();
    }
    let handle = SnapshotHandle {
      data: Rc::new(SnapshotInner {
        index: self.next_snapshot_index,
        globals: self.globals.clone(),
        time: ExtendedTime::beginning_of(time.clone()),
        clones: insert_only::HashMap::new(),
        shared: self.shared.clone(),
      }),
    };
    self
      .shared
      .borrow_mut()
      .snapshots
      .insert(self.next_snapshot_index, handle.clone());
    self.next_snapshot_index += 1;
    Some(handle)
  }

  fn forget_before(&mut self, time: &S::Time) {
    self.invalid_before = max(
      self.invalid_before.clone(),
      ValidSince::Before(time.clone()),
    );
  }
}

impl<S: SimulationSpec> ConstructibleTimeSteward for Steward<S> {
  fn from_globals(globals: <Self::SimulationSpec as SimulationSpec>::Globals) -> Self {
    Steward {
      globals: Rc::new(globals),
      invalid_before: ValidSince::TheBeginning,
      last_event: None,
      upcoming_fiat_events: BTreeSet::new(),
      existent_predictions: BTreeSet::new(),
      shared:Rc::new(RefCell::new(StewardShared {
        snapshots: BTreeMap::new(),
        history: History::new(),
      })),
      next_snapshot_index: 0,
    }
  }

  fn deserialize_from<R: Read>(data: &mut R) -> ::bincode::Result<Self> {
    deserialize_something(data)
  }
}

impl<S: SimulationSpec> IncrementalTimeSteward for Steward<S> {
  fn step(&mut self) {
    if let Some(event) = self.next_event().cloned() {
      self.execute_event(&event);
    }
  }
  fn updated_until_before(&self) -> Option<S::Time> {
    self
      .next_event()
      .map(|event| event.extended_time().base.clone())
  }
}
//impl<S: SimulationSpec> CanonicalTimeSteward for Steward<S> {}

//time_steward_define_simple_timeline!();
//time_steward_define_bbox_collision_detection!();
