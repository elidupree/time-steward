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

use {DeterministicRandomId, SiphashIdGenerator, RowId, ColumnId, FieldId, PredictorId,
            Column, StewardRc, FieldRc, ExtendedTime, Basics, Accessor, FiatEventOperationError,
            ValidSince, TimeStewardSettings, TimeSteward};
use stewards::common::{self, Filter};
use std::collections::{HashMap, BTreeMap, HashSet, BTreeSet, btree_map};
use std::collections::hash_map::Entry;
// use std::collections::Bound::{Included, Excluded, Unbounded};
use std::any::Any;
use std::borrow::Borrow;
use std::rc::Rc;
use std::cell::{Cell, RefCell};
use std::ops::Drop;
use std::mem;
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

impl <B: Basics> StewardEventsInfo <B> {
  fn schedule_event (&mut self, time: ExtendedTime <B>, event: Event <B>)->Result <(),()> {
    match self.event_states.entry (time.clone()) {
      Entry::Vacant (entry) => {
        entry.insert (EventState {schedule: Some (event), execution_state: None});
        self.events_needing_attention.insert (time);
      }
      Entry::Occupied (mut entry) => {
        let state = entry.get_mut();
        if state.schedule.is_some() { return Err (());}
        state.schedule = Some (event);
        if state.execution_state.is_none() {
          self.events_needing_attention.insert (time);
        }
      }
    };
    Ok (())
  }
  
  fn unschedule_event (&mut self, time: & ExtendedTime <B>)->Result <(),()> {
    match self.event_states.entry (time.clone()) {
      Entry::Vacant (_) => return Err (()),
      Entry::Occupied (mut entry) => {
        if entry.get_mut().schedule.is_none() { return Err (());}
        entry.get_mut().schedule = None;
        if let Some (ref mut execution_state) = entry.get_mut().execution_state {
          invalidate_execution::<B> (time, execution_state, &mut self.events_needing_attention, &mut self.dependencies);
        }
        if entry.get().execution_state.is_none() {
          self.events_needing_attention.remove (time);
          entry.remove();
        }
      }
    };
    Ok (())
  }
}
  fn invalidate_execution <B: Basics> (time: & ExtendedTime <B>, execution: &mut EventExecutionState, events_needing_attention: &mut BTreeSet<ExtendedTime<B>>, steward_dependencies: &mut DependenciesMap <B>) {
    if let EventValidity::ValidWithDependencies (dependencies) = mem::replace (&mut execution.validity, EventValidity::Invalid) {
      events_needing_attention.insert (time.clone());
      for dependency in dependencies {
        match steward_dependencies.entry (dependency) {
          Entry::Vacant (_) => panic!("dependency records are inconsistent"),
          Entry::Occupied (mut entry) => {
            entry.get_mut().bounded.remove (time);
            if entry.get().is_empty() {entry.remove();}
          }
        }
      }
    }
  }
  
impl <B: Basics> StewardOwned <B> {
  fn invalidate_prediction_dependency (&mut self, row_id: RowId, predictor_id: PredictorId, time: & ExtendedTime <B>) {

            let mut remove = false;
            {
            let mut history = self.predictions_by_id.get_mut (&(row_id, predictor_id)).expect ("access records are inconsistent");
            while let Some (mut prediction) = history.predictions.pop() {
              if let Some (ref limit) = prediction.valid_until {if limit <= time {break;}}
              if let Some ((ref event_time,_)) = prediction.what_will_happen {
                if event_time >time {self.events.unschedule_event (event_time).unwrap();}
              }
              
              if prediction.made_at <= *time {
                change_needed_prediction_time (row_id, predictor_id, &mut history, Some (time.clone()), &mut self.predictions_missing_by_time);
              }
              
      for dependency in prediction.predictor_accessed.iter() {
        match self.events.dependencies.entry (dependency.clone()) {
          Entry::Vacant (_) => panic!("dependency records are inconsistent"),
          Entry::Occupied (mut entry) => {
            match prediction.valid_until {Some (ref limit) => {entry.get_mut().bounded.remove (limit);}, None => {entry.get_mut().unbounded.remove (&(row_id, predictor_id));}};
            if entry.get().is_empty() {entry.remove();}
          }
        }
      }

              if prediction.made_at < *time {
                prediction.valid_until = Some (time.clone());
      for dependency in prediction.predictor_accessed.iter() {
        self.events.dependencies.entry (dependency.clone()).or_insert (Default::default()).bounded.insert (time.clone(), AccessInfo::PredictionAccess (row_id, predictor_id));
              }

                history.predictions.push (prediction);
              }
            }
            if history.predictions.is_empty() && history.next_needed.is_none() { remove = true; }
            }
            if remove {self.predictions_by_id.remove (& (row_id, predictor_id));}
    }  
  fn invalidate_dependencies (&mut self, id: FieldId, time: & ExtendedTime <B>) {
    let invalid_dependencies_option = if let Entry::Occupied (mut my_dependencies) = self.events.dependencies.entry (id) {
      // BTreeMap::split_off() DOES remove this splitting key, while we want to NOT include that key.
      // TODO: will Rust eventually make this easier?
      let mut result = (my_dependencies.get_mut().bounded.split_off (time), mem::replace (&mut my_dependencies.get_mut().unbounded, Default::default()));
      if let Some (whoops) = result.0.remove (time) {my_dependencies.get_mut().bounded.insert (time.clone(), whoops);}
      if my_dependencies.get().is_empty() {my_dependencies.remove();}
      Some (result)
    } else {None};
    if let Some ((bounded, unbounded)) = invalid_dependencies_option {
      for (access_time, access_info) in bounded {
        match access_info {
          AccessInfo::EventAccess => invalidate_execution::<B> (&access_time, &mut self.events.event_states.get_mut (&access_time).expect ("event that accessed this field was missing").execution_state.as_mut().expect ("event that accessed this field not marked executed"), &mut self.events.events_needing_attention, &mut self.events.dependencies),
          AccessInfo::PredictionAccess (row_id, predictor_id) => self.invalidate_prediction_dependency (row_id, predictor_id, time),
        }
      }
      for (row_id, predictor_id) in unbounded {
        self.invalidate_prediction_dependency (row_id, predictor_id, time);
      }
    }
  }
  
  fn discard_changes (&mut self, id: FieldId, history: &mut FieldHistory <B>, index: usize, during_processing_of_event_responsible_for_first_discarded: bool, snapshots: &SnapshotsData <B>) {
    if index == history.changes.len() {return;}
    history.update_snapshots (id, snapshots);
    self.invalidate_dependencies (id, &history.changes [index].last_change);
    let mut discard_iter = history.changes.split_off (index).into_iter();
    if during_processing_of_event_responsible_for_first_discarded {discard_iter.next();}
    for discarded in discard_iter {
      invalidate_execution::<B> (& discarded.last_change, &mut self.events.event_states.get_mut (& discarded.last_change).expect ("event that created this change was missing").execution_state.as_mut().expect ("event that created this change not marked executed"), &mut self.events.events_needing_attention, &mut self.events.dependencies);
    }
  }
 
  fn add_change (&mut self, id: FieldId, history: &mut FieldHistory <B>, change: Field <B>, snapshots: &mut SnapshotsData <B>, shared: & StewardShared <B>)  {
    history.changes.last().map (| last_change | assert!(last_change.last_change <change.last_change));
    history.update_snapshots (id, snapshots);
    self.invalidate_dependencies (id, &change.last_change);
    if history.changes.last().map_or (true, | previous | previous.data.is_none()) {
        assert!(change.data.is_some(), "a change from nonexistent to nonexistent shouldn't be recorded");
        if let Some (predictors) = shared.settings.predictors_by_column.get (&id.column_id) { 
          for predictor in predictors {
            let prediction_history = self.predictions_by_id.entry ((id.row_id, predictor.predictor_id)).or_insert (Default::default());
            if let Some (time) = prediction_history.next_needed.clone() {
              assert!(time <= change.last_change, "failed to invalidate old predictions before inserting new one");
            } else {
              self.predictions_missing_by_time.entry (change.last_change.clone()).or_insert (Default::default()).insert ((id.row_id, predictor.predictor_id));
              prediction_history.next_needed = Some (change.last_change.clone());
            }
          }
        }
    }
    history.changes.push (change);
  }
}

fn change_needed_prediction_time <B: Basics> (row_id: RowId, predictor_id: PredictorId, history: &mut PredictionHistory <B>, time: Option <ExtendedTime <B>>, predictions_missing_by_time: &mut BTreeMap<ExtendedTime <B>, HashSet <(RowId, PredictorId)>>) {
                if let Some (previous) = mem::replace (&mut history.next_needed, time) {
                  let mut entry = match predictions_missing_by_time.entry (previous) {btree_map::Entry::Vacant (_) => panic!("prediction needed records are inconsistent"), btree_map::Entry::Occupied (entry) => entry};
                  entry.get_mut().remove(& (row_id, predictor_id));
                  if entry.get().is_empty() {entry.remove();}
                }
                if let Some (new) = history.next_needed.clone() {
                predictions_missing_by_time.entry (new).or_insert (Default::default()).insert ((row_id, predictor_id));}

}

impl <B: Basics> Steward <B> { 


  fn create_execution (&mut self, time: & ExtendedTime <B>, new_results: MutatorResults <B>)->EventExecutionState {
    let mut fields_guard = self.shared.fields.borrow_mut();
    let fields = &mut*fields_guard;
    let field_states = &mut fields.field_states;
    let changed_since_snapshots = &mut fields.changed_since_snapshots;
    let mut new_fields_changed = HashSet::new();
    let mut new_dependencies = HashSet::with_capacity(new_results.fields.len());
    let mut added_fields = Vec::with_capacity (new_results.fields.len());
    for (id, field) in new_results.fields {
      new_dependencies.insert (id);
      self.owned.events.dependencies.entry (id).or_insert (Default::default()).bounded.insert (time.clone(), AccessInfo::EventAccess);
      if field.last_change == *time {
        new_fields_changed.insert (id);
        let mut history = field_states.entry(id).or_insert (FieldHistory {changes: Vec:: new(), first_snapshot_not_updated: self.owned.next_snapshot});
        if history.changes.is_empty() {added_fields.push (id);}
        match history.changes.binary_search_by_key (&time, | change | &change.last_change) {
          Ok (_) => panic!("there shouldn't be a change at this time is no event has been executed then"),
          Err (index) => {
            self.owned.discard_changes (id, &mut history, index, false, changed_since_snapshots);
            self.owned.add_change (id, &mut history, field, changed_since_snapshots, & self.shared);
          }
        }
      }
    }
    for id in added_fields {self.owned.existent_fields.insert (id, & | key | field_states.contains_key (key));}
    EventExecutionState {fields_changed: new_fields_changed, validity: EventValidity::ValidWithDependencies (new_dependencies)}
  }
  fn remove_execution (&mut self, time: & ExtendedTime <B>, execution: EventExecutionState) {
    let mut fields_guard = self.shared.fields.borrow_mut();
    let fields = &mut*fields_guard;
    let field_states = &mut fields.field_states;
    let changed_since_snapshots = &mut fields.changed_since_snapshots;
    let mut removed_fields = Vec::with_capacity (execution.fields_changed.len());
    for id in execution.fields_changed {
      if let Entry::Occupied (mut entry) = field_states.entry(id) {
        //some of these could have ALREADY been deleted –
        //in fact, perhaps that's how the event was invalidated in the first place
        if let Ok (index) = entry.get().changes.binary_search_by_key (&time, | change | &change.last_change) {
          self.owned.discard_changes (id, entry.get_mut(), index, true, changed_since_snapshots);
          if entry.get().changes.is_empty() {removed_fields.push (id); entry.remove();}
        }
      }
    }
    for id in removed_fields {self.owned.existent_fields.remove (id, & | key | field_states.contains_key (key));}
  }
  fn replace_execution (&mut self, time: & ExtendedTime <B>, execution: &mut EventExecutionState, new_results: MutatorResults <B>) {
    let mut fields_guard = self.shared.fields.borrow_mut();
    let fields = &mut*fields_guard;
    let field_states = &mut fields.field_states;
    let changed_since_snapshots = &mut fields.changed_since_snapshots;
    let mut new_fields_changed = HashSet::new();
    let mut new_dependencies = HashSet::with_capacity(new_results.fields.len());
    let mut removed_fields = Vec::with_capacity (execution.fields_changed.len());
    let mut added_fields = Vec::with_capacity (new_results.fields.len());
    for (id, field) in new_results.fields {
      new_dependencies.insert (id);
      self.owned.events.dependencies.entry (id).or_insert (Default::default()).bounded.insert (time.clone(), AccessInfo::EventAccess);
      if field.last_change == *time {
        new_fields_changed.insert (id);
        //TODO: handle erasing a non-existent field properly
        let mut history = field_states.entry(id).or_insert (FieldHistory {changes: Vec:: new(), first_snapshot_not_updated: self.owned.next_snapshot});
        if history.changes.is_empty() {added_fields.push (id);}
        match history.changes.binary_search_by_key (&time, | change | &change.last_change) {
          Ok (index) => {
            //TODO: be able to check field equality in general
            //if history.changes [index].data != field.data {
              self.owned.discard_changes (id, &mut history, index, true, changed_since_snapshots);
              self.owned.add_change (id, &mut history, field, changed_since_snapshots, & self.shared);
            //}
          }
          Err (index) => {
            self.owned.discard_changes (id, &mut history, index, false, changed_since_snapshots);
            self.owned.add_change (id, &mut history, field, changed_since_snapshots, & self.shared);
          }
        }
      }
    }
    for id in mem::replace (&mut execution.fields_changed, new_fields_changed) {
      if execution.fields_changed.get(&id).is_none() {
        if let Entry::Occupied (mut entry) = field_states.entry(id) {
          //some of these could have ALREADY been deleted –
          //in fact, perhaps that's how the event was invalidated in the first place
          if let Ok (index) = entry.get().changes.binary_search_by_key (&time, | change | &change.last_change) {
            self.owned.discard_changes (id, entry.get_mut(), index, true, changed_since_snapshots);
            if entry.get().changes.is_empty() {removed_fields.push (id); entry.remove();}
          }
        }
      }
    }
    execution.validity = EventValidity::ValidWithDependencies (new_dependencies);
    for id in added_fields {self.owned.existent_fields.insert (id, & | key | field_states.contains_key (key));}
    for id in removed_fields {self.owned.existent_fields.remove (id, & | key | field_states.contains_key (key));}
  }
  
  fn do_event (&mut self, time: & ExtendedTime <B>) {
    self.owned.events.events_needing_attention.remove(time);
    let mut state = self.owned.events.event_states.remove (time).expect ("You can't do an event that wasn't scheduled");
    if let Some (event) = state.schedule {
      let results;
      {
        let field_ref = &*self.shared.fields.borrow();
        let mut mutator = Mutator {
          //steward: &mut self.owned,
          shared: &self.shared,
          fields: field_ref,
        generic: common::GenericMutator::new(time.clone()),
          results: MutatorResults {fields: insert_only::HashMap::new()},
        };
        event(&mut mutator);
        results = mutator. results;
      }
      state.schedule = Some (event);
      if let Some (ref mut execution) = state.execution_state {
        self.replace_execution (& time, execution, results);
      }
      else {
        state.execution_state = Some (self.create_execution (time, results));
      }
      self.owned.events.event_states.insert (time.clone(), state);
    } else {
      self.remove_execution (time, state.execution_state.expect("a null event state was left lying around"));
    }
  }
  fn make_prediction (&mut self, row_id: RowId, predictor_id: PredictorId, time: & ExtendedTime <B>) {
    let prediction;
    {
    let mut history = self.owned.predictions_by_id.get_mut (&(row_id, predictor_id)).expect (" prediction needed records are inconsistent");
    let predictor = self.shared.settings.predictors_by_id.get (&predictor_id).unwrap().clone();
let fields = self.shared.fields.borrow();
    let mut results;
    let generic;
    {
      let field_ref = &*fields;
      let mut pa = PredictorAccessor {
        //predictor_id: predictor_id,
        //about_row_id: row_id,
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
    
    let field = fields.field_states.get (&FieldId::new (row_id, predictor.column_id)).expect ("why are we making a prediction if the field never exists");
    let next_change_index = match field.changes.binary_search_by_key (&time, | change | &change.last_change) {
      Ok (index) => index + 1, Err (index) => index + 1};
    let next_needed =
    if let Some (next_change) = field.changes.get (next_change_index) {
      if next_change.data.is_none() { 
        if let Some (current_end) = results.valid_until {
          if next_change.last_change < current_end {
            results.valid_until = Some (next_change.last_change.clone());
          }
          else {results.valid_until = Some (current_end);}
        }
        else {results.valid_until = Some (next_change.last_change.clone());}
        if let Some (next_creation) = field.changes.get (next_change_index + 1) {
          assert!(next_creation.data.is_some(), "there is no need to store multiple deletions in a row");

          Some (next_creation. last_change.clone()) 
        }
        else {None}
      }
      else {results.valid_until.clone()}
    }
    else {results.valid_until.clone()};
    
    change_needed_prediction_time (row_id, predictor_id, &mut history, next_needed, &mut self.owned.predictions_missing_by_time);
    
    
    prediction = Prediction {
      predictor_accessed: dependencies,
      what_will_happen: generic.soonest_prediction.and_then(|(event_base_time, event)| {
          common::next_extended_time_of_predicted_event(predictor_id,
                                                       row_id,
                                                       dependencies_hash,
                                                       event_base_time,
                                                       time)
            .map(|event_time| (event_time, event))
      }),
      made_at: time.clone(), valid_until: results.valid_until,
    };
    }
    for dependency in prediction.predictor_accessed.iter() {
      let dependencies = self.owned.events.dependencies.entry (dependency.clone()).or_insert (Default::default());
      match prediction.valid_until {Some (ref limit) => {dependencies.bounded.insert (limit.clone(),AccessInfo::PredictionAccess(row_id, predictor_id));}, None => {dependencies.unbounded.insert ((row_id, predictor_id));}};
    }
    if let Some((ref time, ref event)) = prediction.what_will_happen {
      self.owned.events.schedule_event (time.clone(), event.clone()).unwrap();
    }
    self.owned.predictions_by_id.entry((row_id, predictor_id)).or_insert(Default::default()).predictions.push (prediction);
    
  }
  
  fn do_next (&mut self)->Option <ExtendedTime <B>> {
    let next_event = self.owned.events.events_needing_attention.iter().next().cloned();
    let next_prediction = self.owned.predictions_missing_by_time.iter().next(). map (| (time, set) | {
      let &(row_id, predictor_id) = set.iter().next().expect ("empty set of predictions should have been cleaned up");
      (time.clone(), row_id, predictor_id)
    });
    match (next_event, next_prediction) {
      (None, None) => None,
      (Some (event_time), None) => {
        self.do_event (& event_time);
        Some (event_time)
      },
      (None, Some ((prediction_time, row_id, predictor_id))) => {
        self.make_prediction (row_id, predictor_id, & prediction_time);
        Some (prediction_time)
      },
      (Some (event_time), Some ((prediction_time, row_id, predictor_id))) => {
        if event_time <= prediction_time {
          self.do_event (& event_time);
          Some (event_time)
        } else {
          self.make_prediction (row_id, predictor_id, & prediction_time);
          Some (prediction_time)
        }
      },
    }
  }
}

impl <B: Basics> FieldHistory <B> {
  fn update_snapshots (&mut self, my_id: FieldId, snapshots: &SnapshotsData <B>) {
    for (index, &(ref time, ref snapshot_map)) in snapshots.iter().rev() {
      if *index <self.first_snapshot_not_updated {break;}
      snapshot_map.get_default(my_id, || {
        use std::cmp::Ordering;
        let index = match self.changes.binary_search_by (| change | if change.last_change.base < *time {Ordering::Less} else {Ordering::Greater}) { Ok (_) => unreachable!(), Err (index) => index - 1};
        self.changes.get (index).and_then  (| change | change.data.as_ref().map (| data | (data.clone(), change.last_change.clone())))
      });
      self.first_snapshot_not_updated = index + 1;
    }
  }
}


#[derive (Clone)]
struct Field<B: Basics> {
  last_change: ExtendedTime<B>,
  data: Option <FieldRc>,
}
type SnapshotField<B: Basics> = (FieldRc, ExtendedTime <B>);

#[derive (Clone)]
enum AccessInfo {
  EventAccess,
  PredictionAccess (RowId, PredictorId),
}
struct FieldHistory <B: Basics> {
  changes: Vec<Field <B>>,
  first_snapshot_not_updated: SnapshotIdx,
}

type SnapshotsData <B: Basics> = BTreeMap<SnapshotIdx,
                                    (B::Time, Rc<insert_only::HashMap<FieldId, SnapshotField<B>>>)>;

struct Fields<B: Basics> {
  field_states: HashMap<FieldId, FieldHistory <B>>,
  changed_since_snapshots: SnapshotsData <B>,
}

#[derive (Default)]
struct Dependencies <B: Basics> {
bounded:BTreeMap<ExtendedTime <B>, AccessInfo>,
unbounded: HashSet <(RowId, PredictorId)>,
}
impl<B: Basics> Dependencies <B> {
  fn is_empty (&self)->bool {
    self.bounded.is_empty () && self.unbounded.is_empty()
  }
}
type DependenciesMap <B: Basics> =HashMap<FieldId, Dependencies <B>>;

#[derive (Clone)]
struct Prediction<B: Basics> {
  predictor_accessed: Vec<FieldId>,
  what_will_happen: Option<(ExtendedTime<B>, Event<B>)>,
  made_at: ExtendedTime <B>,
  valid_until: Option <ExtendedTime <B>>,
}
#[derive (Default)]
struct PredictionHistory <B: Basics> {
  next_needed: Option <ExtendedTime <B>>,
  predictions:Vec<Prediction<B>>,
}


struct StewardShared<B: Basics> {
  settings: Settings <B>,
  constants: B::Constants,
  fields: RefCell<Fields<B>>,
}

struct StewardEventsInfo <B: Basics> {
  event_states: HashMap <ExtendedTime<B>, EventState <B>>,
  events_needing_attention: BTreeSet<ExtendedTime<B>>,
  dependencies: DependenciesMap <B>,
}

struct StewardOwned<B: Basics> {
  events: StewardEventsInfo <B>,
  
  invalid_before: ValidSince <B::Time>,
  next_snapshot: SnapshotIdx,
  existent_fields: partially_persistent_nonindexed_set::Set<FieldId>,
  
  predictions_by_id: HashMap<(RowId, PredictorId), PredictionHistory <B>>,
  predictions_missing_by_time: BTreeMap<ExtendedTime <B>, HashSet <(RowId, PredictorId)>>,
}

pub struct Steward<B: Basics> {
  owned: StewardOwned<B>,
  shared: Rc<StewardShared<B>>,
}
pub struct Snapshot<B: Basics> {
  now: B::Time,
  index: SnapshotIdx,
  field_states: Rc<insert_only::HashMap<FieldId, SnapshotField<B>>>,
  shared: Rc<StewardShared<B>>,
  num_fields: usize,
  field_ids: partially_persistent_nonindexed_set::Snapshot<FieldId>,
}
pub struct MutatorResults <B: Basics> {
  fields: insert_only::HashMap <FieldId, Field <B>>,

}
pub struct Mutator<'a, B: Basics> {
  //steward: &'a mut StewardOwned<B>,
  shared: &'a StewardShared<B>,
  fields: &'a Fields<B>,
  generic: common::GenericMutator<B>,
  results: MutatorResults <B>,
}
struct PredictorAccessorResults<B: Basics> {
  valid_until: Option <ExtendedTime <B>>,
}
pub struct PredictorAccessor<'a, B: Basics> {
  //predictor_id: PredictorId,
  //about_row_id: RowId,
  internal_now: ExtendedTime<B>,
  shared: &'a StewardShared<B>,
  fields: &'a Fields<B>,
  generic: common::GenericPredictorAccessor<B, Event<B>>,
  results: RefCell<PredictorAccessorResults<B>>,
}
pub type EventFn<B> = for<'d, 'e> Fn(&'d mut Mutator<'e, B>);
pub type Event<B> = StewardRc <EventFn<B>>;

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
                                                 .get_for_snapshot (id, & self.now, self.index)
                                                  ).map (| whatever | (&whatever.0, &whatever.1))
  }
  fn constants(&self) -> &B::Constants {
    &self.shared.constants
  }
  fn unsafe_now(&self) -> &B::Time {
    &self.now
  }
}
impl<'a, B: Basics> ::Accessor<B> for Mutator<'a, B> {
  fn generic_data_and_extended_last_change(&self,
                                             id: FieldId)
                                             -> Option<(&FieldRc, &ExtendedTime<B>)> {
    let field = self.results.fields.get_default (id, | | {
      Some (match self.fields.get(id, & self.generic.now, false) {
        None => Field {data: None, last_change: self.generic.now.clone()},
        Some ((data, time)) => Field {data: Some (data.clone()), last_change: time.clone()},
      })
    }).unwrap();
    field.data.as_ref().map (| data | (data, & field.last_change))
  }
  fn constants(&self) -> &B::Constants {
    &self.shared.constants
  }
    time_steward_common_accessor_methods_for_mutator!(B);
}
impl<'a, B: Basics> PredictorAccessor<'a, B> {
  fn get_impl(&self, id: FieldId) -> Option<(&FieldRc, &ExtendedTime<B>)> {
    self.fields.get(id, & self.internal_now, true)
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
impl<'a, B: Basics> ::PredictorAccessor<B> for PredictorAccessor<'a, B> {
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
    self.results.fields.insert (field_id, Field {last_change: self. generic.now.clone(), data: data.map (| whatever | {let something: FieldRc = StewardRc::new (whatever); something})});
  }
  time_steward_common_mutator_methods_for_mutator!(B);
}
impl<'a, B: Basics> Rng for Mutator<'a, B> {
  time_steward_common_rng_methods_for_mutator!(B);
}

impl<B: Basics> Fields<B> {
  fn get (& self, id: FieldId, time: & ExtendedTime <B>, after: bool) -> Option<(& FieldRc, &ExtendedTime<B>)> {
    self.field_states.get (& id).and_then (| history | {
      let index = match history.changes.binary_search_by_key (&time, | change | &change.last_change) { Ok (index) => if after {index} else {index.wrapping_sub (1)}, Err (index) => index.wrapping_sub (1), };
      history.changes.get (index).and_then (| change | change.data.as_ref().map (| data | (data, & change.last_change)))
    })
  }
  fn get_for_snapshot (& self, id: FieldId, time: &B::Time, index: SnapshotIdx) -> Option<(FieldRc, ExtendedTime<B>)> {
    use std::cmp::Ordering;
    self.field_states.get (& id).and_then (| history | {
      if history.first_snapshot_not_updated > index {return None}
      let index = match history.changes.binary_search_by (| change | if change.last_change.base < *time {Ordering::Less} else {Ordering::Greater}) { Ok (_) => unreachable!(), Err (index) => index.wrapping_sub (1), };
      history.changes.get (index).and_then (| change | change.data.as_ref().map (| data | (data.clone(), change.last_change.clone())))
    })
  }

}
impl<B: Basics> Steward<B> {
  fn update_until_beginning_of(&mut self, target_time: &B::Time) {
    //TODO: this goes one step too far. Is that a problem?
    while let Some(_) = self.do_next().filter(| time | time.base < *target_time) {}
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
        events: StewardEventsInfo {
        
        events_needing_attention: BTreeSet::new(),
        event_states: HashMap::new(),
        dependencies: HashMap::new(),
        },
        invalid_before: ValidSince::TheBeginning,        next_snapshot: 0,
        existent_fields: partially_persistent_nonindexed_set::Set::new(),
        predictions_missing_by_time: BTreeMap::new(),
        predictions_by_id: HashMap::new(),

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
    let mut last_event = None;
    result.shared.fields.borrow_mut().field_states =
      snapshot.into_iter()
              .map(|(id, stuff)| {
                if match last_event {
                  None => true,
                  Some(ref time) => stuff.1 > time,
                } {
                  last_event = Some(stuff.1.clone());
                }
                result.shared.settings.predictors_by_column.get(&id.column_id).map(|predictors| {
                  for predictor in predictors {
                    predictions_needed.insert((id.row_id, predictor.predictor_id));
                  }
                });
                (id,
                 FieldHistory {changes: vec![Field {
                  data: Some (stuff.0.clone()),
                  last_change: stuff.1.clone(),
                }], first_snapshot_not_updated: 0})
              })
              .collect();
    for (row_id, predictor_id) in predictions_needed {
      result.make_prediction(row_id, predictor_id, last_event.as_ref().unwrap());
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
    match self.owned.events.schedule_event (common::extended_time_of_fiat_event(time, id),
                                        StewardRc::new(DynamicEventFn::new(event))) {
      Err (_) => Err(FiatEventOperationError::InvalidInput),
      Ok (_) => Ok(()),
    }
  }

  fn erase_fiat_event(&mut self,
                      time: &B::Time,
                      id: DeterministicRandomId)
                      -> Result<(), FiatEventOperationError> {
    if self.valid_since() > *time {
      return Err(FiatEventOperationError::InvalidTime);
    }
    match self.owned.events.unschedule_event (& common::extended_time_of_fiat_event(time.clone(), id)) {
      Err (_) => Err(FiatEventOperationError::InvalidInput),
      Ok (_) => Ok(()),
    }
  }
  
  fn snapshot_before<'b>(&'b mut self, time: &'b B::Time) -> Option<Self::Snapshot> {
    if self.valid_since() > *time {
      return None;
    }
    self.update_until_beginning_of(time);

    let field_states = self.shared
                        .fields
                        .borrow_mut()
                        .changed_since_snapshots
                        .entry(self.owned.next_snapshot)
                        .or_insert((time.clone(), Rc::new(insert_only::HashMap::new())))
                        .1.clone();
    let result = Some(Snapshot {
      now: time.clone(),
      index: self.owned.next_snapshot,
      field_states: field_states,
      shared: self.shared.clone(),
      num_fields: self.shared.fields.borrow().field_states.len(),
      field_ids: self.owned.existent_fields.snapshot(),
    });

    self.owned.next_snapshot += 1;
    result
  }
}
