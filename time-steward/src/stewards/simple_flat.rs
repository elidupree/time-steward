use std::any::{Any, TypeId};
use std::borrow::Borrow;
use std::cell::{Cell, Ref, RefCell};
use std::cmp::{min, max};
use std::collections::{BTreeMap, BTreeSet};
use std::fmt::Debug;
use std::io::{Read, Write};
use std::rc::Rc;

use super::super::api::*;
use super::super::implementation_support::common::*;
use crate::{DeterministicRandomId, EntityHandle};
use crate::type_utils::{PersistentlyIdentifiedType, DynamicPersistentlyIdentifiedType, PersistentTypeId};

use crate::implementation_support::insert_only;

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


#[derive(Debug)]
pub struct EventInner<S: SimulationSpec> {
  time: ExtendedTime<S>,
  data: Box<EventInnerTrait<S>>,
}

trait EventInnerTrait<S: SimulationSpec>:
  Any + Debug + DynamicPersistentlyIdentifiedType
{
  fn execute(&self, self_handle: &EventHandle<S>, steward: &mut Steward<S>);
  fn get_type_id(&self)->TypeId;
}

impl<S: SimulationSpec, T: Event<Steward = Steward<S>>> EventInnerTrait<S> for T {
  fn execute(&self, self_handle: &EventHandle<S>, steward: &mut Steward<S>) {
    let mut accessor = EventAccessorStruct {
      handle: self_handle.clone(),
      globals: steward.globals.clone(),
      child_id_generator: RefCell::new (EventChildrenIdGenerator::new()),
      steward: RefCell::new(steward),
    };
    <T as Event>::execute(self, &mut accessor);
  }
  fn get_type_id(&self)->TypeId {TypeId::of::<T>()}
}

type EventHandle<S> = DataHandle<(), EventInner<S>>;

#[derive(Debug)]
pub struct EventAccessorStruct<'a, S: SimulationSpec> {
  handle: EventHandle<S>,
  globals: Rc<S::Globals>,
  // note: these are only RefCell because of the problem where modify/create_prediction can't take &mut self
  child_id_generator: RefCell<EventChildrenIdGenerator>,
  steward: RefCell<&'a mut Steward<S>>,
}
#[derive(Debug)]
pub struct SnapshotInner<S: SimulationSpec> {
  index: usize,
  time: ExtendedTime<S>,
  globals: Rc<S::Globals>,
  // hack: RefCell only so that we can return a Ref
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
  upcoming_fiat_events: BTreeSet<TimeOrderedEventHandle<Steward<S>>>,
  existent_predictions: BTreeSet<TimeOrderedEventHandle<Steward<S>>>,
  shared: Rc<RefCell<StewardShared<S>>>,
  next_snapshot_index: usize,
}

#[derive(Debug)]
struct StewardShared<S: SimulationSpec> {
  snapshots: SnapshotsTree<S>,
  history: History<S>,
}



mod history {
use super::EventHandle;
use std::collections::VecDeque;
use std::any::Any;
use std::marker::PhantomData;
use crate::{SimulationSpec, Modify, ExtendedTime, EventHandleTrait, SimulationStateData};

#[derive (Copy, Clone, Debug, Default)]
pub struct HistoryIndex (usize);


#[derive(Debug)]
struct HistoryItem<S: SimulationSpec>  {
  event: EventHandle<S>,
  undo: Box <dyn AnyUndo>,
  previous: HistoryIndex,
}

#[derive(Debug)]
struct Undo <T:SimulationStateData,M: Modify <T>> {
  data: M::UndoData,
  _marker: PhantomData <*const T>,
}

trait AnyUndo: ::std::fmt::Debug {
  fn undo (&self, entity: &mut dyn Any);
}

impl<T: SimulationStateData,M: Modify <T>> AnyUndo for Undo <T, M> {
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

  pub fn insert_modification <T:SimulationStateData, M: Modify <T>> (&mut self, head: &mut HistoryIndex, event: EventHandle<S>, undo: M::UndoData) {
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
  
  pub fn rollback_to_before <T: SimulationStateData> (&self, head: HistoryIndex, current_value: &T, time: &ExtendedTime <S>) -> T {
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



impl<S: SimulationSpec, ImmutableData: SimulationStateData + PersistentlyIdentifiedType, MutableData: SimulationStateData + PersistentlyIdentifiedType> TimeStewardEntityHandleHack <ImmutableData, MutableData> for Steward<S> {
  type EntityHandle = DataHandle<ImmutableData, EntityCell<MutableData>>;
  fn new_entity_handle_nonreplicable_hack (immutable: ImmutableData, mutable: MutableData)->Self::EntityHandle {
    DataHandle::new_nonreplicable(immutable, EntityCell {
      serial_number: new_serial_number(),
      data: RefCell::new(EntityCellInner {current_value: mutable, history_head: HistoryIndex::null()}),
    })
  }
}


impl<ImmutableData: SimulationStateData + PersistentlyIdentifiedType, MutableData: SimulationStateData + PersistentlyIdentifiedType> EntityHandleTrait for DataHandle<ImmutableData, EntityCell<MutableData>> {
  type ImmutableData = ImmutableData;
  type MutableData = MutableData;
}

impl<ImmutableData: SimulationStateData + PersistentlyIdentifiedType, MutableData: SimulationStateData + PersistentlyIdentifiedType> PersistentlyIdentifiedType for DataHandle<ImmutableData, EntityCell<MutableData>> {
  const ID: PersistentTypeId = PersistentTypeId(0x9ec13003c540d3a9 ^ ImmutableData::ID.0 ^ MutableData::ID.0);
}


impl<S: SimulationSpec> EventHandleTrait<S> for EventHandle<S> {
  fn extended_time(&self) -> &ExtendedTime<S> {
    &self.private().time
  }
}

impl<S: SimulationSpec> Borrow<ExtendedTime<S>> for EventHandle<S> {
  fn borrow(&self) -> &ExtendedTime<S> {
    &self.private().time
  }
}


/*
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
}*/



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
}
impl <'a, 'b, ImmutableData: SimulationStateData + PersistentlyIdentifiedType, MutableData: SimulationStateData + PersistentlyIdentifiedType, S: SimulationSpec> AccessorQueryHack<'a, ImmutableData, MutableData> for EventAccessorStruct<'b, S> {
  type QueryGuard = Ref<'a, MutableData>;
  fn query_hack (&'a self, entity: &'a EntityHandle <Steward<S>, ImmutableData, MutableData>)->Self::QueryGuard {
    let guard = entity.private().data.borrow();
    Ref::map (guard, | inner | & inner.current_value)
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
}
impl <'a, ImmutableData: SimulationStateData + PersistentlyIdentifiedType, MutableData: SimulationStateData + PersistentlyIdentifiedType, S: SimulationSpec> AccessorQueryHack<'a, ImmutableData, MutableData> for SnapshotHandle<S> {
  type QueryGuard = &'a MutableData;
  fn query_hack (&'a self, entity: &'a EntityHandle <Steward<S>, ImmutableData, MutableData>)->Self::QueryGuard {
    let guard = entity.private().data.borrow();
    Ref::map (guard, | inner | & inner.current_value);
    
    let entity_guard = entity.private().data.borrow();
    
    // hack: store a copy of the entity handle to guarantee that it doesn't get dropped so that it's valid to use its address as a unique id
    &self.data.clones.get_default (entity.address() as usize, | | {
      let entity: EntityHandle <Steward<S>, ImmutableData, MutableData> = entity.clone();
      let past_value: MutableData = (*self.data.shared).borrow().history.rollback_to_before (entity_guard.history_head, & entity_guard.current_value, & self.data.time);
      Some(Box::new (
        (entity, past_value)
      ))
    }).unwrap ().downcast_ref::<(EntityHandle <Steward<S>, ImmutableData, MutableData>, MutableData)>().expect("A clone in a snapshot was a different type than what it was supposed to be a clone of; maybe two different timelines got the same serial number somehow").1
  }
}

impl<'b, S: SimulationSpec> EventAccessor for EventAccessorStruct<'b, S> {
  fn this_event(&self) -> &EventHandle<S> {
    &self.handle
  }

  fn new_entity_handle<ImmutableData: SimulationStateData + PersistentlyIdentifiedType, MutableData: SimulationStateData + PersistentlyIdentifiedType>(
    &self,
    immutable: ImmutableData, mutable: MutableData
  ) -> EntityHandle<Self::Steward, ImmutableData, MutableData> {
    Self::Steward::new_entity_handle_nonreplicable(immutable, mutable)
  }
  
  fn modify <'a, ImmutableData: SimulationStateData + PersistentlyIdentifiedType, MutableData: SimulationStateData + PersistentlyIdentifiedType, M: Modify<MutableData>> (&'a self, entity: &'a EntityHandle <Steward<S>, ImmutableData, MutableData>, modification: M) {
    let mut modify_guard = entity.private().data.borrow_mut();
    let undo = modification.modify (&mut modify_guard.current_value);
    let steward_guard = self.steward.borrow();
    steward_guard.shared.borrow_mut().history.insert_modification::<MutableData, M>(&mut modify_guard.history_head, self.handle.clone(), undo);
  }


  fn create_prediction<E: Event<Steward = Self::Steward>>(
    &self,
    time: <<Self::Steward as TimeSteward>::SimulationSpec as SimulationSpec>::Time,
    event: E,
  ) -> EventHandle<S> {
    let time = extended_time_of_predicted_event::<<Self::Steward as TimeSteward>::SimulationSpec>(
      time,
      self.child_id_generator.borrow_mut().next(&self.handle.id()),
      self.extended_now(),
    )
    .unwrap();
    let prediction = DataHandle::new_nonreplicable ((), EventInner {
        time: time,
        data: Box::new(event),
      });
    assert!(
        self
          .steward
          .borrow_mut()
          .existent_predictions
          .insert(TimeOrderedEventHandle(prediction.clone())),
        "created a prediction that already existed?!"
      );
    //printlnerr!("at {:?}, creating prediction at {:?}", self.extended_now(), handle.extended_time());
    prediction
  }
  fn destroy_prediction(&self, prediction: &<Self::Steward as TimeSteward>::EventHandle) {
    assert!(
        self
          .steward
          .borrow_mut()
          .existent_predictions
          .remove(prediction.extended_time()),
        "destroyed a prediction that didn't exist"
      );
  }
}

impl<S: SimulationSpec> SnapshotAccessor for SnapshotHandle<S> {
  fn serialize_into<W: Write>(&self, writer: &mut W) -> ::bincode::Result<()> {
    unimplemented!()//serialize_snapshot(writer, self.clone())
  }
}


impl<S: SimulationSpec> Steward<S> {
  fn next_event(&self) -> Option<&EventHandle<S>> {
    let first_fiat_event_iter = self.upcoming_fiat_events.iter().take(1);
    let first_predicted_event_iter = self.existent_predictions.iter().take(1);
    let events_iter = first_fiat_event_iter.chain(first_predicted_event_iter);
    events_iter.min().map(|a|&a.0)
  }

  fn execute_event(&mut self, event: &EventHandle<S>) {
    event.private().data.execute(event, &mut *self);
    // clean it up:
    self.upcoming_fiat_events.remove(event.extended_time());
    self.existent_predictions.remove(event.extended_time());
    self.last_event = Some(event.extended_time().clone());
  }
  
  fn garbage_collect (&mut self) {
    let mut earliest_remembered_time = self.invalid_before.clone();
    for snapshot in (*self.shared).borrow().snapshots.values() {
      earliest_remembered_time = min (earliest_remembered_time, ValidSince::Before (snapshot.data.time.base.clone()));
    }
    
    if let ValidSince::Before (time) | ValidSince::After (time) = earliest_remembered_time {
      self.shared.borrow_mut().history.forget_before (ExtendedTime::beginning_of (time));
    }
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
    match self.upcoming_fiat_events.insert(TimeOrderedEventHandle(DataHandle::new_nonreplicable(
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
    self.garbage_collect();
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
    unimplemented!()//deserialize_something(data)
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
impl<S: SimulationSpec> CanonicalTimeSteward for Steward<S> {}

//time_steward_define_simple_timeline!();
//time_steward_define_bbox_collision_detection!();
