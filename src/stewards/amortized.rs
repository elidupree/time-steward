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
            Column, StewardRc, FieldRc, ExtendedTime,  Basics, TimeSteward, FiatEventOperationError,
            ValidSince, TimeStewardSettings, TimeSteward};
use stewards::common::{self, Filter};
use std::collections::{HashMap, BTreeMap, HashSet, BTreeSet};
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
use data_structures::partially_persistent_nonindexed_set;

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
              self.owned.predictions_missing_by_time.entry (change.last_change.clone()).or_insert (Default::default()).insert ((id.row, predictor.predictor_id));
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
        let mut history = fields.field_states.entry(id).or_insert (FieldHistory {changes: Vec:: new(), first_snapshot_not_updated: self.owned.next_snapshot});
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
        let mut history = fields.field_states.entry(id).or_insert (FieldHistory {changes: Vec:: new(), first_snapshot_not_updated: self.owned.next_snapshot});
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
  
  fn do_event (&mut self, time: & ExtendedTime <B>) {
    self.owned.events_needing_attention.remove(time);
    let state = self.owned.event_states.remove (time).expect ("You can't do an event that wasn't scheduled");
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
          results: RefCell::new (MutatorResults {fields: HashMap::new()}),
        };
        event(&mut mutator);
        results = mutator. results;
      }
      if let Some (ref mut execution) = state.execution_state {
        self.replace_execution (& time, &mut execution, results);
      }
      else {
        state.execution_state = Some (self.create_execution (time, results));
      }
      self.owned.event_states.insert (time.clone(), state);
    } else {
      self.remove_execution (time, state.execution_state.expect("a null event state was left lying around"));
    }
  }
  fn make_prediction (&mut self, time: & ExtendedTime <B>, row_id: RowId, predictor_id: PredictorId) {
    let history = self.predictions_by_id.get ((row_id, predictor_id)).expect (" prediction needed records are inconsistent");
    let predictor = self.get_predictor(predictor_id).clone();
    let results;
    let generic;
    {
      let field_ref = &*self.shared.fields.borrow();
      let mut pa = PredictorAccessor {
        predictor_id: predictor_id,
        about_row_id: row_id,
        internal_now: time.clone(),
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
      made_at: time.clone(), valid_until: results.valid_until,
    });
    self.owned.predictions_by_id.insert((row_id, predictor_id), prediction.clone());
    if let Some((ref time, event)) = prediction.what_will_happen {
      self.owned.schedule_event (time.clone(), prediction.clone());
    }
  }
  fn do_next (&mut self)->Option <ExtendedTime <B>> {
    match (self.owned.events_needing_attention.iter().next(), self.owned.predictions_missing_by_time.iter().next()) {
      (None, None) => None,
      (Some (ref event_time), None) => {
        let time = event_time.clone();
        self.do_event (&time);
        Some (time)
      },
      (None, Some ((ref prediction_time, (row_id, predictor_id)))) => {
        let time = prediction_time.clone();
        self.make_prediction (&time, row_id, predictor_id);
        Some (time)
      },
      (Some (ref event_time), Some ((ref prediction_time, (row_id, predictor_id)))) => {
        if event_time <= prediction_time {
          let time = event_time.clone();
          self.do_event (&time);
          Some (time)
        } else {
          let time = prediction_time.clone();
          self.make_prediction (&time, row_id, predictor_id);
          Some (time)
        }
      },
    }
  }
}

impl <B: Basics> FieldHistory <B> {
  fn update_snapshots (&mut self, my_id: FieldId, snapshots: &SnapshotsData <B>) {
    for (index, snapshot_map) in snapshots.iter().rev() {
      if index <self.first_snapshot_not_updated {break;}
      snapshot_map.1.get_default(my_id, || {
        let index = match self.changes.binary_search_by_key (time, | change | change.last_change) { Ok (index) => index - 1, Err (index) => index - 1,};
        self.changes.get (index).and_then  (| change | change.data.as_ref().map (| data | (data, change.last_change)))
        
      });
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
  steward: &'a mut StewardOwned<B>,
  shared: &'a StewardShared<B>,
  fields: &'a Fields<B>,
  generic: common::GenericMutator<B>,
  results: RefCell<MutatorResults <B>>,  
}
struct PredictorAccessorResults<B: Basics> {
  valid_until: Option <ExtendedTime <B>>,
}
pub struct PredictorAccessor<'a, B: Basics> {
  predictor_id: PredictorId,
  about_row_id: RowId,
  internal_now: ExtendedTime<B>,
  shared: &'a StewardShared<B>,
  fields: &'a Fields<B>,
  generic: common::GenericPredictorAccessor<B, Event<B>>,
  results: RefCell<PredictorAccessorResults<B>>,
}
pub type EventFn<B> = for<'d, 'e> Fn(&'d mut Mutator<'e, B>);
pub type Event<B> = Rc<EventFn<B>>;

time_steward_common_dynamic_callback_structs! (Mutator, PredictorAccessor, DynamicEventFn, DynamicPredictorFn, DynamicPredictor, Settings);

impl<B: Basics> Drop for Snapshot<B> {
  fn drop(&mut self) {
    self.shared.fields.borrow_mut().changed_since_snapshots.remove(&self.index);
  }
}

impl<B: Basics> ::Accessor<B> for Snapshot<B> {
  fn generic_data_and_extended_last_change(&self,
                                             id: FieldId)
                                             -> Option<(&FieldRc, &ExtendedTime<B>)> {
    self.field_states.get_default(id, ||
                                     self.shared
                                                 .fields
                                                 .borrow()
                                                 .get_for_snapshot (id, self.now, self.index)
                                                  )
  }
  fn constants(&self) -> &B::Constants {
    &self.shared.constants
  }
}
impl<'a, B: Basics> ::Accessor<B> for Mutator<'a, B> {
  fn generic_data_and_extended_last_change(&self,
                                             id: FieldId)
                                             -> Option<(&FieldRc, &ExtendedTime<B>)> {
    let field = self.results.borrow_mut().fields.entry (id).or_insert_with (| | self.fields.get(id, self.now));
    field.data.map (| data | (data, field.last_change))
  }
  fn constants(&self) -> &B::Constants {
    &self.shared.constants
  }
    time_steward_common_accessor_methods_for_mutator!(B);
}
impl<'a, B: Basics> PredictorAccessor<'a, B> {
  fn get_impl(&self, id: FieldId) -> Option<(&FieldRc, &ExtendedTime<B>)> {
    self.fields.get(id)
  }
}
impl<'a, B: Basics> ::Accessor<B> for PredictorAccessor<'a, B> {
  time_steward_common_accessor_methods_for_predictor_accessor!(B, get_impl);
  fn constants(&self) -> &B::Constants {
    &self.shared.constants
  }
  fn unsafe_now(&self) -> &B::Time {
    &self.internal_now.base
  }
}

impl<B: Basics> ::MomentaryAccessor<B> for Snapshot<B> {}
impl<'a, B: Basics> ::MomentaryAccessor<B> for Mutator<'a, B> {}
impl<'a, B: Basics> ::PredictorAccessor<B, EventFn<B>> for PredictorAccessor<'a, B> {
  time_steward_common_predictor_accessor_methods_for_predictor_accessor!(B, DynamicEventFn);}

impl<B: Basics> ::Snapshot<B> for Snapshot<B> {
  fn num_fields(&self) -> usize {
    self.num_fields
  }
}

pub struct SnapshotIter<'a, B: Basics>(partially_persistent_nonindexed_set::SnapshotIter<'a,
                                                                                         FieldId>,
                                       &'a Snapshot<B>);
impl<'a, B: Basics> Iterator for SnapshotIter<'a, B> {
  type Item = (FieldId, (& 'a FieldRc, & 'a ExtendedTime <B>));
  fn next(&mut self) -> Option<Self::Item> {
    (self.0).next().map(|id| {
      (id,
       (self.1)
         .generic_data_and_extended_last_change(id)
         .expect("the snapshot thinks a FieldId exists when it doesn't"))
    })
  }
  fn size_hint(&self) -> (usize, Option<usize>) {
    (self.1.num_fields, Some(self.1.num_fields))
  }
}
impl<'a, B: Basics> IntoIterator for &'a Snapshot<B> {
  type Item = (FieldId, (& 'a FieldRc, & 'a ExtendedTime <B>));
  type IntoIter = SnapshotIter <'a, B>;
  fn into_iter(self) -> Self::IntoIter {
    SnapshotIter(self.field_ids.iter(), self)
  }
}

impl<'a, B: Basics> ::Mutator<B> for Mutator<'a, B> {
  fn set<C: Column>(&mut self, id: RowId, data: Option<C::FieldType>) {
    let field_id = FieldId::new (id, C::column_id());
    self.results.borrow_mut().fields.insert (field_id, Field {last_change: self.now, data: data.map (| whatever | Rc::new (whatever))});
  }
  time_steward_common_mutator_methods_for_mutator!(B);
}
impl<'a, B: Basics> Rng for Mutator<'a, B> {
  time_steward_common_rng_methods_for_mutator!(B);
}

impl<B: Basics> Fields<B> {
  fn get (& self, id: FieldId, time: & ExtendedTime <B>) -> Option<(& FieldRc, &ExtendedTime<B>)> {
    self.field_states.get (id).and_then (| history | {
      let index = match history.changes.binary_search_by_key (time, | change | change.last_change) { Ok (index) => index - 1, Err (index) => index - 1};
      history.changes.get (index).and_then (| change | change.data.as_ref().map (| data | (data, change.last_change)))
    })
  }
}
impl<B: Basics> Steward<B> {
  fn update_until_beginning_of(&mut self, target_time: &B::Time) {
    //TODO: this goes one step too far. Is that a problem?
    while let Some(time) = self.do_next().filter(| time | time.base < *target_time) {}
  }
}


impl<B: Basics> TimeSteward <B> for Steward<B> {
  type Snapshot = Snapshot<B>;
  type Settings = Settings <B>;

  fn valid_since(&self) -> ValidSince<B::Time> {
    self.owned.invalid_before.clone()
  }

  fn new_empty(constants: B::Constants, settings: Self::Settings) -> Self {

    Steward {
      owned: StewardOwned {
        last_event: None,
        invalid_before: ValidSince::TheBeginning,
        fiat_events: BTreeMap::new(),
        next_snapshot: 0,
        existent_fields: partially_persistent_nonindexed_set::Set::new(),
        predictions_by_time: BTreeMap::new(),
        predictions_by_id: HashMap::new(),
        prediction_dependencies: HashMap::new(),
      },
      shared: Rc::new(StewardShared {
        settings: settings,
        constants: constants,
        fields: RefCell::new(Fields {
          field_states: HashMap::new(),
          changed_since_snapshots: BTreeMap::new(),
        }),
      }),
    }
  }


  fn from_snapshot<'a, S: ::Snapshot<B>>(snapshot: &'a S, settings: Self::Settings) -> Self
    where &'a S: IntoIterator<Item = ::SnapshotEntry<'a, B>>
  {
    let mut result = Self::new_empty(snapshot.constants().clone(), settings);
    result.owned.invalid_before = ValidSince::Before(snapshot.now().clone());
    let mut predictions_needed = HashSet::new();
    result.shared.fields.borrow_mut().field_states =
      snapshot.into_iter()
              .map(|(id, stuff)| {
                if match result.owned.last_event {
                  None => true,
                  Some(ref time) => stuff.1 > time,
                } {
                  result.owned.last_event = Some(stuff.1.clone());
                }
                result.shared.settings.predictors_by_column.get(&id.column_id).map(|predictors| {
                  for predictor in predictors {
                    predictions_needed.insert((id.row_id, predictor.predictor_id));
                  }
                });
                (id,
                 Field {
                  data: stuff.0.clone(),
                  last_change: stuff.1.clone(),
                  first_snapshot_not_updated: 0,
                })
              })
              .collect();
    for (row_id, predictor_id) in predictions_needed {
      result.make_prediction(row_id, predictor_id);
    }

    result
  }
  fn insert_fiat_event<E: ::EventFn<B>>(&mut self,
                                             time: B::Time,
                                             id: DeterministicRandomId,
                                             event: E)
                                             -> Result<(), FiatEventOperationError> {
    if self.valid_since() > time {
      return Err(FiatEventOperationError::InvalidTime);
    }
    match self.owned.fiat_events.insert(common::extended_time_of_fiat_event(time, id),
                                        StewardRc::new(DynamicEventFn::new(event))) {
      None => Ok(()),
      Some(_) => Err(FiatEventOperationError::InvalidInput),
    }
  }

  fn erase_fiat_event(&mut self,
                      time: &B::Time,
                      id: DeterministicRandomId)
                      -> Result<(), FiatEventOperationError> {
    if self.valid_since() > *time {
      return Err(FiatEventOperationError::InvalidTime);
    }
    match self.owned.fiat_events.remove(& common::extended_time_of_fiat_event(time.clone(), id)) {
      None => Err(FiatEventOperationError::InvalidInput),
      Some(_) => Ok(()),
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
