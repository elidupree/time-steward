/*! 

A full TimeSteward implementation that has decent (amortized) asymptotic performance for all common operations.

This is intended to be the simplest possible implementation that meets those conditions. As such, it's not especially optimized. Here are some of its specific weaknesses:
 
*no support for multithreading
*when a field changes in the past, this TimeSteward immediately erases all more-recent versions of that field. This can take time proportional to the amount of times that field has changed since the past change. (It doesn't affect the amortized time because the recording of each thing amortizes its eventual deletion, but it can cause a hiccup.)
*This erasing happens even if the field was overwritten at some point without being examined. In that case, we could theoretically optimize by leaving the future of the field untouched.
*There can also be hiccups at arbitrary times when the hash table resizes.
*We haven't optimized for the "most changes happen in the present" case, which means we pay a bunch of log n factors when we could be paying O(1).
*If you keep around old snapshots of times when no fields are actually being modified anymore, they will eventually have all their data copied into them unnecessarily. This could be avoided if we had a good two-dimensional tree type so that the snapshots could be queried by (SnapshotIdx X BaseTime) rectangles.
*We also do not optimize the for the case where a field is changed in the past but then the field is changed BACK before it affects anything (either by another change in the fiat events or by a regular prediction). The same applies to the case where an event is invalidated, then rerun, but makes the same changes as it made the first time. This allows dependency chains to propagate much faster than they should.
*There might be more small dependency optimizations we could do, like distinguishing between accessing just a field's data data and accessing just its last change time, or even the difference between those and just checking whether the field exists or not, or other conditions (although we would need an API change to track some of those things). However, I suspect that the additional runtime cost of recording these different dependencies wouldn't be worth it. (This would only have a small effect at best, because it wouldn't slow down dependency chain propagation unless there are fields that haven't implemented guaranteed_equal__unsafe().)

*/

use super::{DeterministicRandomId, SiphashIdGenerator, RowId, ColumnId, FieldId, PredictorId,
            Column, ExtendedTime, EventRng, Basics, TimeSteward, FiatEventOperationResult,
            ValidSince, TimeStewardLifetimedMethods, TimeStewardStaticMethods};

use std::collections::{HashMap, BTreeMap, HashSet};
use std::collections::hash_map::Entry;
use std::hash::Hash;
// use std::collections::Bound::{Included, Excluded, Unbounded};
use std::any::Any;
use std::borrow::Borrow;
use std::rc::Rc;
use std::cell::{Cell, RefCell};
use std::ops::Drop;
use rand::Rng;
use insert_only;

type SnapshotIdx = u64;


/*

An ExtendedTime may be in one of several states
– empty and unused
– a fiat event scheduled but nothing executed
– a fiat event scheduled and executed consistently to that, and still valid
– a fiat event scheduled and executed consistently to that, but its accessed fields have changed
– a fiat event executed, but not scheduled (we could disallow this by doing it immediately)
– a fiat event executed but then rescheduled differently (ditto)
– a predicted event scheduled but nothing executed
– a predicted event scheduled and executed consistently to that, and still valid
– a predicted event scheduled and executed consistently to that, but its accessed fields have changed
– a predicted event executed, but not scheduled (we could disallow this in STABLE states but it is guaranteed to occur at least temporarily during a function)
– a predicted event executed but then rescheduled differently (ditto)

There are enough parallels between fiat and predicted that we should probably combine them:

0. Unused
1. Scheduled only
2. Executed consistently, still valid
3. Executed consistently, fields changed
4. Executed but not scheduled
5. Executed but scheduled differently

possible movements:
(1, 4)->0
0->1
(1, 3, 5)->2
2->3
(2, 3, 5)->4
4->5

The ExtendedTime needs attention in (1, 3, 4, 5) but not (2, 0).
Thus, the changes that affect "needs attention" are:
(1, 4)->0
0->1
(1, 3, 5)->2
2->3
2->4

Which can be split up into the ones CAUSED by giving the time attention:
4->0
(1, 3, 5)->2
And the ones caused from a distance:
0->1 (scheduling)
1->0 and 2->4 (un-scheduling)
2->3 (invalidating)

that's assuming that you're not allowed to reschedule without un-scheduling first (which, in my current model, is true for both fiat events and predicted events)

The only things distinguishing 3 and 5 are how they are created. Let's combine them:

0. Unused
1. Scheduled only
2. Executed consistently, still valid
3. Executed consistently, fields OR schedule different than when it was executed
4. Executed but not scheduled

possible movements:
(1, 4)->0
0->1
(1, 3)->2
(2, 4)->3
(2, 3)->4

The ExtendedTime needs attention in (1, 3, 4) but not (2, 0).
Thus, the changes that affect "needs attention" are:
(1, 4)->0
0->1
(1, 3)->2
2->3
2->4

Which can be split up into the ones CAUSED by giving the time attention:
4->0
(1, 3)->2
And the ones caused from a distance:
0->1 (scheduling)
1->0 and 2->4 (un-scheduling)
2->3 (invalidating)

notice that the (3, 4) trap can only be escaped by giving the time attention.
The only way to REMOVE "needs attention" from a time from a distance is 1->0.



What about predictions?
Each (RowId, PredictorId) defines one "prediction history". The prediction history may be incomplete (predictions missing after a specific ExtendedTime) but may not be invalid (containing incorrect predictions). To do that, we clear invalidated predictions whenever a FieldHistory changes (but we don't clear invalidated events/fields when a prediction history changes).

The incompleteness must be simple: things can be missing after a certain time, but there can't be missing patches in the middle of the history.

For each history, keep exactly a record of 0 or 1 times when we need to make a new prediction. That time is always one of the following:
0. Nothing
1. The end of the validity-interval of the last prediction in the history
2. The earliest nonexistent->existent transition of the corresponding field after (1) or (0).

This can change in several ways:
3. A tail of the prediction history gets invalidated, but there are still valid predictions immediately before that. (1->1)
4. A tail of the prediction history gets invalidated, and there are no valid predictions immediately before that (because the field didn't exist). (1->2, 1->0, or 2->0)
5. The field becomes existent. (0->2)
6. We make a new prediction at the current prediction-needed-time. ((1 or 2)->(1 or 2))

3 and 5 are easy. 6 is the trickiest case – 6 needs to find the NEXT creation time. When 4 does 1->2, it can only be referring to a creation time at exactly the invalidation start time, because if it was an earlier one, the history would have been invalid.



*/

enum EventValidity {
  Invalid,
  ValidWithDependencies (HashSet <FieldId>),
}
struct EventExecutionState {
  fields_changed: HashSet <FieldId>,
  validity: EventValidity,
}

struct EventState <B: Basics> {
  schedule: Option <Event <B>>,
  execution_state: Option <EventExecutionState>,
}

impl <B: Basics> StewardOwned <B> {
  fn schedule_event (&mut self, time: ExtendedTime <B>, event: Event <B>) {
    match self.event_states.entry (time.clone()) {
      Entry::Vacant (entry) => {
        entry.insert (EventState {schedule: Some (event), execution_state: None});
        self.events_needing_attention.insert (time);
      }
      Entry::Occupied (mut entry) => {
        let state = entry.get_mut();
        assert!(state.scheduled.is_none(), "scheduling an event where there is already one scheduled");
        state.schedule = Some (event);
        if state.execution_state.is_none() {
          self.events_needing_attention.insert (time);
        }
      }
    }
  }
  
  fn unschedule_event (&mut self, time: ExtendedTime <B>) {
    match self.event_states.entry (time.clone()) {
      Entry::Vacant (_) => {
        panic!("You can't unschedule an event that wasn't scheduled")
      }
      Entry::Occupied (mut entry) => {
        let state = entry.get_mut();
        assert!(state.schedule.is_some(), "You can't unschedule an event that wasn't scheduled");
        state.schedule = None;
        if let Some (ref mut execution_state) = state.execution_state {
          Self::invalidate_execution (time, execution_state, self.events_needing_attention, self.dependencies);
        } else {
          self.events_needing_attention.remove (time);
          entry.remove();
        }
      }
    }
  }
  
  fn invalidate_execution (time: & ExtendedTime <B>, execution: &mut EventExecutionState, events_needing_attention: &mut BTreeSet<ExtendedTime<B>>, steward_dependencies: &mut DependenciesMap) {
    if let ValidWithDependencies (dependencies) = execution.validity {
      execution_state.validity = Invalid;
      events_needing_attention.insert (time);
      for dependency in dependencies {
        match steward_dependencies.entry (dependency) {
          Entry::Vacant (_) => panic!("dependency records are inconsistent"),
          Entry::Occupied (mut entry) => {
            entry.get_mut().remove (time);
            if entry.get().is_empty() {entry.remove();}
          }
        }
      }
    }
  }
  
  fn invalidate_dependencies (&mut self, id: FieldId, time: ExtendedTime <B>) {
    if let Some (my_dependencies) = self.dependencies.get (id) {
      for (access_time, access_info) in my_dependencies.range (Excluded (time), Unbounded) {
        match access_info {
          EventAccess => Self::invalidate_execution (access_time, self.event_states.get(discarded.last_change).expect ("event that accessed this field was missing").execution_state.expect ("event that accessed this field not marked executed"), self.events_needing_attention, self.dependencies),
          PredictionAccess (row_id, predictor_id) => {
            let history = self.predictions_by_id.get_mut ((row_id, predictor_id)).expect ("access records are inconsistent");
            while let prediction = history.predictions.last_mut() {
              if prediction.valid_until <= time {break;}
              if let Some ((event_time,_)) = prediction.what_will_happen {
                self.unschedule_event (event_time);
              }
              if prediction.made_at < time {
                prediction.valid_until = time;
              } else {
                history.predictions.pop();
              }
              if prediction.made_at <= time {
                change_needed_prediction_time (time.clone(), &mut history, &mut self.predictions_missing_by_time);
                break;
              }
            }
          },
        }
      }
    }
  }
}

fn change_needed_prediction_time (time: Option <ExtendedTime <B>>, history: &mut PredictionHistory, predictions_missing_by_time: &mut BTreeMap<ExtendedTime <B>, HashSet <(RowId, PredictorId)>>) {
                if let Some (previous) = history.next_needed {
                  let entry = match self.owned.predictions_missing_by_time.entry (previous) {Entry::Vacant (_) => panic!("prediction needed records are inconsistent"), Entry::Occupied (entry) => entry};
                  entry.get_mut().remove((row_id, predictor_id));
                  if entry.get().is_empty() {entry.remove();}
                }
                if let Some (new) = time {
                predictions_missing_by_time.entry (new.clone()).or_insert (Default::default()).insert ((row_id, predictor_id));}
                history.next_needed = time;

}

impl <B: Basics> Steward <B> {
  
  fn discard_changes (&mut self, id: FieldId, history: &mut FieldHistory <B>, index: usize, during_processing_of_event_responsible_for_first_discarded: bool, snapshots: &SnapshotsData <B>) {
    history.update_snapshots (snapshots);
    self.invalidate_dependencies (id, history.changes [index].last_change);
    let mut discard_iter = history.changes.split_off (index).into_iter();
    if during_processing_of_event_responsible_for_first_discarded {discard_iter.next();}
    for discarded in discard_iter {
      Self::invalidate_execution (discarded.last_change, self.event_states.get(discarded.last_change).expect ("event that created this change was missing").execution_state.expect ("event that created this change not marked executed"), self.events_needing_attention, self.dependencies);
    }
  }
  
  fn add_change (&mut self, id: FieldId, history: &mut FieldHistory <B>, change: Field <B>, snapshots: &mut SnapshotsData <B>)  {
    history.changes.last().map (| last_change | assert!(last_change.last_change <change.last_change));
    history.update_snapshots (snapshots);
    self.invalidate_dependencies (id, change.last_change);
    if let Some (previous) = history.changes.last() {
      if previous.data.is_none() {
        assert!(change.data.is_some(), "a change from nonexistent to nonexistent shouldn't be recorded");
        if let Some (predictors) = self.shared.predictors_by_column.get (id.column) { 
          for predictor in predictors {
            let prediction_history = self.predictions_by_id.entry ((id.row, predictor.predictor_id)).or_insert (Default::default());
            if let Some (time) = prediction_history.next_needed {
              assert!(time <= change.last_change, "failed to invalidate old predictions before inserting new one");
            } else {
              self.owned.predictions_missing_by_time.entry (change.last_change.clone()).or_insert (Default::default()).insert ((id.row, predictor.predictor_id)),
              prediction_history.next_needed = Some (change.last_change.clone());
            }
          }
        }
      }
    }
    history.changes.push (change);
  }



  fn create_execution (&mut self, time: & ExtendedTime <B>, new_results: MutatorResults <B>)->EventExecutionState {
    let fields = self.shared.fields.borrow_mut();
    let new_fields_changed = HashSet::new();
    let new_dependencies = HashSet::with_capacity(new_results.fields);
    for (id, field) in new_results.fields {
      new_dependencies.insert (id);
      if field.last_change == time {
        new_fields_changed.insert (id);
        let mut history = fields.field_states.entry(id).or_insert (FieldHistory {changes: Vec:: new(), first_snapshot_not_updated = self.owned.next_snapshot});
        match history.changes.binary_search_by_key (time, | change | change.last_change) {
          Ok (index) => panic!("there shouldn't be a change at this time is no event has been executed then"),
          Err (index) => {
            self.discard_changes (id, entry.get_mut(), index, false, &fields.changed_since_snapshots);
            self.add_change (id, entry.get_mut(), field, &fields.changed_since_snapshots);
          }
        }
      }
    }
  }
  fn remove_execution (&mut self, time: & ExtendedTime <B>, execution: EventExecutionState) {
    let fields = self.shared.fields.borrow_mut();
    for id in execution.fields_changed {
      if let Entry::Occupied (mut entry) = field_states.entry(id) {
        //some of these could have ALREADY been deleted –
        //in fact, perhaps that's how the event was invalidated in the first place
        if let Ok (index) = entry.get().changes.binary_search_by_key (time, | change | change.last_change) {
          self.discard_changes (id, entity.get_mut(), index, true, &fields.changed_since_snapshots);
          if entry.get().changes.is_empty() {entry.remove();}
        }
      }
    }
  }
  fn replace_execution (&mut self, time: & ExtendedTime <B>, execution: &mut EventExecutionState, new_results: MutatorResults <B>) {
    let fields = self.shared.fields.borrow_mut();
    let new_fields_changed = HashSet::new();
    let new_dependencies = HashSet::with_capacity(new_results.fields);
    for (id, field) in new_results.fields {
      new_dependencies.insert (id);
      if field.last_change == time {
        new_fields_changed.insert (id);
        let mut history = fields.field_states.entry(id).or_insert (FieldHistory {changes: Vec:: new(), first_snapshot_not_updated = self.owned.next_snapshot});
        match history.changes.binary_search_by_key (time, | change | change.last_change) {
          Ok (index) => {
            self.owned.discard_changes (id, entry.get_mut(), index, true, &fields.changed_since_snapshots);
            self.owned.add_change (id, entry.get_mut(), field, &fields.changed_since_snapshots);
          }
          Err (index) => {
            self.discard_changes (id, entry.get_mut(), index, false, &fields.changed_since_snapshots);
            self.add_change (id, entry.get_mut(), field, &fields.changed_since_snapshots);
          }
        }
      }
    }
    for id in execution.fields_changed {
      if new_fields_changed.get(id).is_none() {
        if let Entry::Occupied (mut entry) = field_states.entry(id) {
          //some of these could have ALREADY been deleted –
          //in fact, perhaps that's how the event was invalidated in the first place
          if let Ok (index) = entry.get().changes.binary_search_by_key (time, | change | change.last_change) {
            self.owned.discard_changes (id, entity.get_mut(), index, true, &fields.changed_since_snapshots);
            if entry.get().changes.is_empty() {entry.remove();}
          }
        }
      }
    }
    execution.fields_changed = new_fields_changed;
    execution.validity = ValidWithDependencies (new_dependencies);
  }
  
  fn do_event (&mut self, time: ExtendedTime <B>) {
    self.owned.events_needing_attention.remove(& time);
    let state = self.owned.event_states.remove (& time).expect ("You can't do an event that wasn't scheduled");
    if let Some (event) = entry.get().schedule {
      let results;
      {
        let field_ref = &*self.shared.fields.borrow();
        let mut mutator = Mutator {
          now: time,
          steward: &mut self.owned,
          shared: &self.shared,
          fields: field_ref,
          generator: super::generator_for_event(event_time.id),
          results: RefCell::new (MutatorResults {fields: HashMap::new()},
        };
        event(&mut mutator);
        results = mutator. results;
      }
      if let Some (ref mut execution) = state.execution_state {
        self.replace_execution (& time, &mut execution, results);
      }
      else {
        state.execution_state = Some (self.create_execution (& time, results));
      }
      self.owned.event_states.insert (time, state);
    } else {
      self.remove_execution (& time, state.execution_state.expect("a null event state was left lying around");
    }
  }
  fn make_prediction (&mut self, time: ExtendedTime <B>, row_id: RowId, predictor_id: PredictorId) {
    let history = self.predictions_by_id.get ((row_id, predictor_id)).expect (" prediction needed records are inconsistent");
    let predictor = self.get_predictor(predictor_id).clone();
    let results;
    let generic;
    {
      let field_ref = &*self.shared.fields.borrow();
      let mut pa = PredictorAccessor {
        predictor_id: predictor_id,
        about_row_id: row_id,
        internal_now: time,
        shared: &self.shared,
        fields: field_ref,
        generic: common::GenericPredictorAccessor::new(),
        results: RefCell::new(PredictorAccessorResults {
          valid_until: None,
        }),
      };
      (predictor.function)(&mut pa, row_id);
      results = pa.results.into_inner();
      generic = pa.generic;
    }
    let (dependencies, hasher) = generic.dependencies.into_inner();
    let dependencies_hash = hasher.generate();
    
    let field = self.shared.fields.borrow().field_states.get (FieldId::new (row_id, predictor.column_id)).expect ("why are we making a prediction if the field never exists");
    let next_change_index = match field.changes.binary_search_by_key (time, | change | change.last_change) {
      Ok (index) => index + 1, Err (index) => index + 1};
    let next_needed =
    if let Some (next_change) = field.changes.get (next_change_index) {
      if next_change.data.is_none() { 
        if next_change.last_change < & results.valid_until {
          results.valid_until = next_creation.last_change.clone();
        }
        if let Some (next_creation) = field.changes.get (next_change_index + 1) {
          assert!(next_creation.data.is_some(), "there is no need to store multiple deletions in a row");

          Some (next_creation. last_change.clone()) 
        }
        else {None}
      }
      else {Some (results.valid_until.clone())}
    }
    else {Some (results.valid_until.clone())};
    
    change_needed_prediction_time (next_needed, &mut history, &mut self.owned.predictions_missing_by_time);
    
    
    let prediction = Rc::new(Prediction {
      predictor_accessed: dependencies,
      what_will_happen: results.soonest_prediction.and_then(|(event_base_time, event)| {
          super::next_extended_time_of_predicted_event(predictor_id,
                                                       row_id,
                                                       dependencies_hash,
                                                       event_base_time,
                                                       &self.owned
                                                            .last_event
                                                            .as_ref()
                                                            .expect("how can we be calling a \
                                                                     predictor when there are no \
                                                                     fields yet?"))
            .map(|event_time| (event_time, event))
      }),
      made_at: time, valid_until: results.valid_until,
    });
    self.owned.predictions_by_id.insert((row_id, predictor_id), prediction.clone());
    if let Some((ref time, event)) = prediction.what_will_happen {
      self.owned.schedule_event (time.clone(), prediction.clone());
    }
  }
  fn do_next (&mut self) {
    match (self.owned.events_needing_attention.iter().next(), self.owned.predictions_missing_by_time.iter().next()) {
      (None, None) =>(),
      (Some (ref event_time), None) => self.do_event (event_time.clone()),
      (None, Some ((ref prediction_time, (row_id, predictor_id)))) => self.make_prediction (prediction_time.clone(), row_id, predictor_id),
      (Some (ref event_time), Some ((ref prediction_time, (row_id, predictor_id)))) => if event_time <= prediction_time {self.do_event (event_time.clone())} else {self.make_prediction (prediction_time.clone(), row_id, predictor_id)},
    }
  }
}

impl <B: Basics> FieldHistory <B> {
  fn update_snapshots (&mut self, snapshots: &SnapshotsData <B>) {
    for (index, snapshot_map) in snapshots.range(Included (self.first_snapshot_not_updated), Unbounded) {
      snapshot_map.1.get_default(field_id, || SnapshotField {
        data: self.changes.get (match self.changes.binary_search_by_key (time, | change | change.last_change) { Ok (index) => index - 1, Err (index) => index - 1,})
      })
      self.first_snapshot_not_updated = index + 1;
    }
  }
}


#[derive (Clone)]
struct Field<B: Basics> {
  last_change: ExtendedTime<B>,
  data: Option <Rc<Any>>,
}
type SnapshotField<B: Basics> = (FieldRc, ExtendedTime <B>);

enum AccessInfo {
  EventAccess,
  PredictionAccess (RowId, PredictorId),
}
struct FieldHistory <B: Basics> {
  changes: Vec<Field <B>>,
  first_snapshot_not_updated: SnapshotIdx,
}

type SnapshotsData <B> = BTreeMap<SnapshotIdx,
                                    Rc<insert_only::HashMap<FieldId, SnapshotField<B>>>>;

struct Fields<B: Basics> {
  field_states: HashMap<FieldId, FieldHistory <B>>,
  changed_since_snapshots: SnapshotsData <B>,
}

type DependenciesMap <B> =HashMap<FieldId, BTreeMap<ExtendedTime <B>, AccessInfo>>;

#[derive (Clone)]
struct Prediction<B: Basics> {
  predictor_accessed: Vec<FieldId>,
  what_will_happen: Option<(ExtendedTime<B>, Event<B>)>,
  made_at: ExtendedTime <B>,
  valid_until: Option <ExtendedTime <B>>,
}
struct PredictionHistory <B: Basics> {
  next_needed: Option <ExtendedTime <B>>,
  predictions:Vec<Prediction<B>>,
}

struct StewardShared<B: Basics> {
  settings: Settings <B>,
  constants: B::Constants,
  fields: RefCell<Fields<B>>,
}
#[derive (Clone)]
struct StewardOwned<B: Basics> {
  event_states: HashMap <ExtendedTime<B>, EventState <B>>,
  events_needing_attention: BTreeSet<ExtendedTime<B>>,

  next_snapshot: SnapshotIdx,
  predictions_by_id: HashMap<(RowId, PredictorId), PredictionHistory <B>>,
  predictions_missing_by_time: BTreeMap<ExtendedTime <B>, HashSet <(RowId, PredictorId)>>,
  dependencies: DependenciesMap <B>,
}
#[derive (Clone)]
pub struct Steward<B: Basics> {
  owned: StewardOwned<B>,
  shared: Rc<StewardShared<B>>,
}
pub struct Snapshot<B: Basics> {
  now: B::Time,
  index: SnapshotIdx,
  field_states: Rc<insert_only::HashMap<FieldId, SnapshotField<B>>>,
  shared: Rc<StewardShared<B>>,
}
struct MutatorResults <B: Basics> {
  fields: HashMap<FieldId, Field <B>>,
}

pub struct Mutator<'a, B: Basics> {
  now: ExtendedTime<B>,
  steward: &'a mut StewardOwned<B>,
  shared: &'a StewardShared<B>,
  fields: &'a Fields<B>,
  generator: EventRng,
  results: RefCell<MutatorResults <B>>,  
}
struct PredictorAccessorResults<B: Basics> {
  soonest_prediction: Option<(B::Time, Event<B>)>,
  dependencies: Vec<FieldId>,
  dependencies_hasher: SiphashIdGenerator,
  valid_until: Option <ExtendedTime <B>>,
}
pub struct PredictorAccessor<'a, B: Basics> {
  predictor_id: PredictorId,
  about_row_id: RowId,
  internal_now: ExtendedTime<B>,
  shared: &'a StewardShared<B>,
  fields: &'a Fields<B>,
  results: RefCell<PredictorAccessorResults<B>>,
}
pub type EventFn<B> = for<'d, 'e> Fn(&'d mut Mutator<'e, B>);
pub type Event<B> = Rc<EventFn<B>>;
pub type Predictor<B> = super::Predictor<PredictorFn<B>>;
pub type PredictorFn<B> = for<'b, 'c> Fn(&'b mut PredictorAccessor<'c, B>, RowId);



impl<B: Basics> Drop for Snapshot<B> {
  fn drop(&mut self) {
    self.shared.fields.borrow_mut().changed_since_snapshots.remove(&self.index);
  }
}

impl<B: Basics> super::Accessor<B> for Snapshot<B> {
  fn data_and_last_change<C: Column>(&self, id: RowId) -> Option<(&C::FieldType, &B::Time)> {
    let field_id = FieldId::new (id, C::column_id());
    let field = self.field_states.get_default(field_id, || {
                                     SnapshotField {
                                       data: self.shared
                                                 .fields
                                                 .borrow()
                                                 .get_for_snapshot (&field_id, self.now)
                                                 .cloned(),
                                       touched_by_steward: Cell::new(false),
                                     }
                                   })

    extract_field_info::<B, C>(                                   .data
                                   .as_ref())
      .map(|p| (p.0, &p.1.base))
  }
  fn constants(&self) -> &B::Constants {
    &self.shared.constants
  }
}
impl<'a, B: Basics> super::Accessor<B> for Mutator<'a, B> {
  fn data_and_last_change<C: Column>(&self, id: RowId) -> Option<(&C::FieldType, &B::Time)> {
    let field = extract_field_info::<B, C> (self.results.borrow_mut().fields.entry (id).or_insert_with (| | fields.get::<C>(id, self.now)));
    (field.0, & field.1.base)
  }
  fn constants(&self) -> &B::Constants {
    &self.shared.constants
  }
}
impl<'a, B: Basics> super::Accessor<B> for PredictorAccessor<'a, B> {
  fn data_and_last_change<C: Column>(&self, id: RowId) -> Option<(&C::FieldType, &B::Time)> {
    let field_id = FieldId::new (id, C::column_id());
    let mut results = self.results.borrow_mut();
    self.steward
        .borrow_mut()
        .prediction_dependencies
        .entry(field_id)
        .or_insert(HashSet::new())
        .insert((self.about_row_id, self.predictor_id));
    results.dependencies.push(field_id);
    self.fields.get::<C>(id, self.now).map(|p| {
      p.1.id.hash(&mut results.dependencies_hasher);
      (p.0, &p.1.base)
    })
  }
  fn constants(&self) -> &B::Constants {
    &self.shared.constants
  }
}

impl<B: Basics> super::MomentaryAccessor<B> for Snapshot<B> {
  fn now(&self) -> &B::Time {
    &self.now
  }
}
impl<'a, B: Basics> super::MomentaryAccessor<B> for Mutator<'a, B> {
  fn now(&self) -> &B::Time {
    &self.now.base
  }
}
impl<'a, B: Basics> super::PredictorAccessor<B, EventFn<B>> for PredictorAccessor<'a, B> {
  fn predict_immediately(&mut self, event: Event<B>) {
    let t = self.internal_now.base.clone();
    self.predict_at_time(&t, event);
  }
  fn predict_at_time(&mut self, time: &B::Time, event: Event<B>) {
    if time < &self.internal_now.base {
      return;
    }
    let mut results = self.results.borrow_mut();
    if let Some((ref old_time, _)) = results.soonest_prediction {
      if old_time <= time {
        return;
      }
    }
    results.soonest_prediction = Some((time.clone(), event));
  }
}
impl<B: Basics> super::Snapshot<B> for Snapshot<B> {}

impl<'a, B: Basics> super::Mutator<B> for Mutator<'a, B> {
  fn set<C: Column>(&mut self, id: RowId, data: Option<C::FieldType>) {
    let field_id = FieldId::new (id, C::column_id());
    self.results.borrow_mut().fields.insert (Field {last_change: self.now, data: data.map (| whatever | Rc::new (whatever))});
  }
  fn rng(&mut self) -> &mut EventRng {
    &mut self.generator
  }
  fn random_id(&mut self) -> RowId {
    RowId { data: [self.generator.gen::<u64>(), self.generator.gen::<u64>()] }
  }
}


// https://github.com/rust-lang/rfcs/issues/1485
trait Filter<T> {
  fn filter<P: FnOnce(&T) -> bool>(self, predicate: P) -> Self;
}
impl<T> Filter<T> for Option<T> {
  fn filter<P: FnOnce(&T) -> bool>(self, predicate: P) -> Self {
    self.and_then(|x| {
      if predicate(&x) {
        Some(x)
      } else {
        None
      }
    })
  }
}

fn extract_field_info<B: Basics, C: Column>(field: &Field<B>)
                                            -> (&C::FieldType, &ExtendedTime<B>) {
    (field.data
              .downcast_ref::<C::FieldType>()
              .expect("a field had the wrong type for its column")
              .borrow(),
     & field.last_change)

}

fn get<C: Column, Value>(map: &HashMap<FieldId, Value>, id: RowId) -> Option<&Value> {
  map.get(&FieldId {
    row_id: id,
    column_id: C::column_id(),
  })

}

impl<B: Basics> Fields<B> {
  fn get<C: Column>(&self, id: RowId) -> Option<(&C::FieldType, &ExtendedTime<B>)> {
    let field_states = &self.field_states;
    extract_field_info::<B, C>(get::<C, Field<B>>(field_states, id))
  }
  // returns true if the field changed from existing to nonexistent or vice versa
  fn set<C: Column>(&mut self, id: RowId, value: C::FieldType, time: &ExtendedTime<B>) -> bool {
    let field = Field {
      data: Rc::new(value),
      last_change: time.clone(),
    };
    match self.field_states
              .entry(FieldId {
                row_id: id,
                column_id: C::column_id(),
              }) {
      Entry::Occupied(mut entry) => {
        entry.insert(field);
        false
      }
      Entry::Vacant(entry) => {
        entry.insert(field);
        true
      }
    }
  }
  // returns true if the field changed from existing to nonexistent or vice versa
  fn remove<C: Column>(&mut self, id: RowId) -> bool {
    self.field_states
        .remove(&FieldId {
          row_id: id,
          column_id: C::column_id(),
        })
        .is_some()
  }
  // returns true if the field changed from existing to nonexistent or vice versa
  fn set_opt<C: Column>(&mut self,
                        id: RowId,
                        value_opt: Option<C::FieldType>,
                        time: &ExtendedTime<B>)
                        -> bool {
    if let Some(value) = value_opt {
      self.set::<C>(id, value, time)
    } else {
      self.remove::<C>(id)
    }
  }
}
impl<B: Basics> Steward<B> {
  fn next_event(&self) -> Option<(ExtendedTime<B>, Event<B>)> {
    let first_fiat_event_iter = self.owned
                                    .fiat_events
                                    .iter()
                                    .map(|ev| (ev.0.clone(), ev.1.clone()))
                                    .take(1);
    let first_predicted_event_iter = self.owned
                                         .predictions_by_time
                                         .iter()
                                         .map(|pair| {
                                           (pair.0.clone(),
                                            pair.1
                                                .what_will_happen
                                                .as_ref()
                                                .expect("a prediction that predicted nothing was \
                                                         stored in predictions")
                                                .1
                                                .clone())
                                         })
                                         .take(1);
    /* let predicted_events_iter = self.settings
     * .predictors
     * .iter()
     * .flat_map(|predictor|
     * TODO change field_states
     * to separate by field type, for efficiency,
     * like the haskell does?
     * self.state.field_states.keys().filter_map(move |field_id|
     * if field_id.column_id != predictor.column_id {
     * None
     * } else {
     * let mut pa = PredictorAccessor{
     * steward: self,
     * soonest_prediction: None,
     * dependencies_hasher: SiphashIdGenerator::new(),
     * };
     * (predictor.function)(&mut pa, field_id.row_id);
     * let dependencies_hash = pa.dependencies_hasher.generate();
     * pa.soonest_prediction.and_then(|(event_base_time, event)|
     * super::next_extended_time_of_predicted_event(
     * predictor.predictor_id,
     * field_id.row_id,
     * dependencies_hash,
     * event_base_time,
     * &self.state.last_event.as_ref().expect ("how can we be calling a predictor when there are no fields yet?")
     * ).map(|event_time| (event_time, event)))})); */
    let events_iter = first_fiat_event_iter.chain(first_predicted_event_iter);
    events_iter.min_by_key(|ev| ev.0.clone())
  }

  fn get_predictor(&self, predictor_id: PredictorId) -> &Predictor<B> {
    self.shared
        .predictors_by_id
        .get(&predictor_id)
        .expect("somehow a PredictorId appeared with no associated predictor")
  }

  fn clear_prediction(&mut self, row_id: RowId, predictor_id: PredictorId) {
    if let Some(prediction) = self.owned.predictions_by_id.get(&(row_id, predictor_id)) {
      for field_id in prediction.predictor_accessed.iter() {
        if let Entry::Occupied(mut entry) = self.owned
                                                .prediction_dependencies
                                                .entry(field_id.clone()) {
          entry.get_mut().remove(&(row_id, predictor_id));
          if entry.get().is_empty() {
            entry.remove();
          }
        }
      }
      if let Some((ref when, _)) = prediction.what_will_happen {
        self.owned.predictions_by_time.remove(when).expect("prediction records were inconsistent");
      }
    }
  }

  fn execute_event(&mut self, event_time: ExtendedTime<B>, event: Event<B>) {
    let predictions_needed;

    {
      let field_ref = &mut *self.shared.fields.borrow_mut();
      let mut mutator = Mutator {
        now: event_time.clone(),
        steward: &mut self.owned,
        shared: &self.shared,
        fields: field_ref,
        generator: super::generator_for_event(event_time.id),
        predictions_needed: HashSet::new(),
      };
      event(&mut mutator);
      predictions_needed = mutator.predictions_needed;
    }
    // if it was a fiat event, clean it up:
    self.owned.fiat_events.remove(&event_time);
    self.owned.last_event = Some(event_time);


    let old_value = self.fields.field_states.get(&field_id).cloned();
    let existence_changed = self.fields.set_opt::<C>(id, data, &self.now);
    for snapshot_map in self.fields.changed_since_snapshots.iter().rev() {
      let info = snapshot_map.1.get_default(field_id, || {
        SnapshotField {
          data: old_value.clone(),
          touched_by_steward: Cell::new(false),
        }
      });
      if info.touched_by_steward.get() {
        break;
      }
      info.touched_by_steward.set(true);

    if existence_changed {
      self.shared.predictors_by_column.get(&C::column_id()).map(|predictors| {
        for predictor in predictors {
          self.predictions_needed.insert((id, predictor.predictor_id));
        }
      });
    }
    if let Entry::Occupied(entry) = self.steward.prediction_dependencies.entry(field_id) {
      for prediction in entry.get() {
        self.predictions_needed.insert(prediction.clone());
      }
      entry.remove();
    }

    for (row_id, predictor_id) in predictions_needed {
      self.clear_prediction(row_id, predictor_id);
      let now = self.owned
                    .last_event
                    .clone()
                    .expect("how can we be calling a predictor when there are no fields yet?");
      let function = self.get_predictor(predictor_id).function.clone();
      let results;
      {
        let field_ref = &*self.shared.fields.borrow();
        let mut pa = PredictorAccessor {
          predictor_id: predictor_id,
          about_row_id: row_id,
          internal_now: now,
          steward: RefCell::new(&mut self.owned),
          shared: &self.shared,
          fields: field_ref,
          results: RefCell::new(PredictorAccessorResults {
            soonest_prediction: None,
            dependencies: Vec::new(),
            dependencies_hasher: SiphashIdGenerator::new(),
          }),
        };
        (function)(&mut pa, row_id);
        results = pa.results.into_inner();
      }
      let dependencies_hash = results.dependencies_hasher.generate();
      let prediction = Rc::new(Prediction {
        predictor_id: predictor_id,
        prediction_is_about_row_id: row_id,
        predictor_accessed: results.dependencies,
        what_will_happen: results.soonest_prediction.and_then(|(event_base_time, event)| {
          super::next_extended_time_of_predicted_event(predictor_id,
                                                       row_id,
                                                       dependencies_hash,
                                                       event_base_time,
                                                       &self.owned
                                                            .last_event
                                                            .as_ref()
                                                            .expect("how can we be calling a \
                                                                     predictor when there are no \
                                                                     fields yet?"))
            .map(|event_time| (event_time, event))
        }),
      });
      self.owned.predictions_by_id.insert((row_id, predictor_id), prediction.clone());
      if let Some((ref time, _)) = prediction.what_will_happen {
        self.owned.predictions_by_time.insert(time.clone(), prediction.clone());
      }
    }
  }

  fn update_until_beginning_of(&mut self, target_time: &B::Time) {
    while let Some(ev) = self.next_event().filter(|ev| ev.0.base < *target_time) {
      let (event_time, event) = ev;
      self.execute_event(event_time, event);
    }
  }
}


impl<'a, B: Basics> TimeStewardLifetimedMethods<'a, B> for Steward<B> {
  type Mutator = Mutator <'a, B>;
  type PredictorAccessor = PredictorAccessor <'a, B>;
}
impl<B: Basics> TimeStewardStaticMethods<B> for Steward<B> {
  type EventFn = EventFn <B>;
  type PredictorFn = PredictorFn <B>;
  type Snapshot = Snapshot<B>;

  fn valid_since(&self) -> ValidSince<B::Time> {
    match self.owned.last_event {
      None => ValidSince::TheBeginning,
      Some(ref time) => ValidSince::After(time.base.clone()),
    }
  }

  fn new_empty(constants: B::Constants,
               predictors: Vec<super::Predictor<Self::PredictorFn>>)
               -> Self {
    let mut predictors_by_id = HashMap::new();
    let mut predictors_by_column = HashMap::new();
    for predictor in predictors {
      predictors_by_id.insert(predictor.predictor_id, predictor.clone());
      predictors_by_column.entry(predictor.column_id).or_insert(Vec::new()).push(predictor);
    }

    Steward {
      owned: StewardOwned {
        last_event: None,
        fiat_events: BTreeMap::new(),
        next_snapshot: 0,
        predictions_by_time: BTreeMap::new(),
        predictions_by_id: HashMap::new(),
        prediction_dependencies: HashMap::new(),
      },
      shared: Rc::new(StewardShared {
        predictors_by_id: predictors_by_id,
        predictors_by_column: predictors_by_column,
        constants: constants,
        fields: RefCell::new(Fields {
          field_states: HashMap::new(),
          changed_since_snapshots: BTreeMap::new(),
        }),
      }),
    }
  }

  fn insert_fiat_event(&mut self,
                       time: B::Time,
                       id: DeterministicRandomId,
                       event: Event<B>)
                       -> FiatEventOperationResult {
    if let Some(ref change) = self.owned.last_event {
      if change.base >= time {
        return FiatEventOperationResult::InvalidTime;
      }
    }
    match self.owned.fiat_events.insert(super::extended_time_of_fiat_event(time, id), event) {
      None => FiatEventOperationResult::Success,
      Some(_) => FiatEventOperationResult::InvalidInput,
    }
  }

  fn erase_fiat_event(&mut self,
                      time: &B::Time,
                      id: DeterministicRandomId)
                      -> FiatEventOperationResult {
    if let Some(ref change) = self.owned.last_event {
      if change.base >= *time {
        return FiatEventOperationResult::InvalidTime;
      }
    }
    match self.owned.fiat_events.remove(&super::extended_time_of_fiat_event(time.clone(), id)) {
      None => FiatEventOperationResult::InvalidInput,
      Some(_) => FiatEventOperationResult::Success,
    }
  }

  fn snapshot_before<'b>(&'b mut self, time: &'b B::Time) -> Option<Self::Snapshot> {
    if let Some(ref change) = self.owned.last_event {
      if change.base >= *time {
        return None;
      }
    }
    self.update_until_beginning_of(time);

    let result = Some(Snapshot {
      now: time.clone(),
      index: self.owned.next_snapshot,
      field_states: self.shared
                        .fields
                        .borrow_mut()
                        .changed_since_snapshots
                        .entry(self.owned.next_snapshot)
                        .or_insert(Rc::new(insert_only::HashMap::new()))
                        .clone(),
      shared: self.shared.clone(),
    });

    self.owned.next_snapshot += 1;
    result
  }
}
impl<B: Basics> TimeSteward<B> for Steward<B> {}
