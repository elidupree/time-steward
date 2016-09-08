//! A full TimeSteward implementation that has decent (amortized) asymptotic performance for all common operations.
//!
//! This is intended to be the simplest possible implementation that meets those conditions. As such, it's not especially optimized. Here are some of its specific weaknesses:
//!
//! no support for multithreading
//! when a field changes in the past, this TimeSteward immediately erases all more-recent versions of that field. This can take time proportional to the amount of times that field has changed since the past change. (It doesn't affect the amortized time because the recording of each thing amortizes its eventual deletion, but it can cause a hiccup.)
//! This erasing happens even if the field was overwritten at some point without being examined. In that case, we could theoretically optimize by leaving the future of the field untouched.
//! There can also be hiccups at arbitrary times when the hash table resizes.
//! We haven't optimized for the "most changes happen in the present" case, which means we pay a bunch of log n factors when we could be paying O(1).
//! If you keep around old snapshots of times when no fields are actually being modified anymore, they will eventually have all their data copied into them unnecessarily. This could be avoided if we had a good two-dimensional tree type so that the snapshots could be queried by (SnapshotIdx X BaseTime) rectangles.
//! We also do not optimize the for the case where a field is changed in the past but then the field is changed BACK before it affects anything (either by another change in the fiat events or by a regular prediction). The same applies to the case where an event is invalidated, then rerun, but makes the same changes as it made the first time. This allows dependency chains to propagate much faster than they should.
//! There might be more small dependency optimizations we could do, like distinguishing between accessing just a field's data data and accessing just its last change time, or even the difference between those and just checking whether the field exists or not, or other conditions (although we would need an API change to track some of those things). However, I suspect that the additional runtime cost of recording these different dependencies wouldn't be worth it. (This would only have a small effect at best, because it wouldn't slow down dependency chain propagation unless there are fields that haven't implemented guaranteed_equal__unsafe().)
//!
//!

use {DeterministicRandomId, SiphashIdGenerator, RowId, FieldId, PredictorId, Column, StewardRc,
     FieldRc, ExtendedTime, Basics, Accessor, FiatEventOperationError, ValidSince, TimeSteward, IncrementalTimeSteward};
use stewards::common;
use std::collections::{HashMap, BTreeMap, HashSet, BTreeSet, btree_map};
use std::collections::hash_map::Entry;
// use std::collections::Bound::{Included, Excluded, Unbounded};
use std::rc::Rc;
use std::cell::RefCell;
use std::ops::Drop;
use std::mem;
use rand::Rng;
use insert_only;
use data_structures::partially_persistent_nonindexed_set;

type SnapshotIdx = u64;


// An ExtendedTime may be in one of several states
// – empty and unused
// – a fiat event scheduled but nothing executed
// – a fiat event scheduled and executed consistently to that, and still valid
// – a fiat event scheduled and executed consistently to that, but its accessed fields have changed
// – a fiat event executed, but not scheduled (we could disallow this by doing it immediately)
// – a fiat event executed but then rescheduled differently (ditto)
// – a predicted event scheduled but nothing executed
// – a predicted event scheduled and executed consistently to that, and still valid
// – a predicted event scheduled and executed consistently to that, but its accessed fields have changed
// – a predicted event executed, but not scheduled (we could disallow this in STABLE states but it is guaranteed to occur at least temporarily during a function)
// – a predicted event executed but then rescheduled differently (ditto)
//
// There are enough parallels between fiat and predicted that we should probably combine them:
//
// 0. Unused
// 1. Scheduled only
// 2. Executed consistently, still valid
// 3. Executed consistently, fields changed
// 4. Executed but not scheduled
// 5. Executed but scheduled differently
//
// possible movements:
// (1, 4)->0
// 0->1
// (1, 3, 5)->2
// 2->3
// (2, 3, 5)->4
// 4->5
//
// The ExtendedTime needs attention in (1, 3, 4, 5) but not (2, 0).
// Thus, the changes that affect "needs attention" are:
// (1, 4)->0
// 0->1
// (1, 3, 5)->2
// 2->3
// 2->4
//
// Which can be split up into the ones CAUSED by giving the time attention:
// 4->0
// (1, 3, 5)->2
// And the ones caused from a distance:
// 0->1 (scheduling)
// 1->0 and 2->4 (un-scheduling)
// 2->3 (invalidating)
//
// that's assuming that you're not allowed to reschedule without un-scheduling first (which, in my current model, is true for both fiat events and predicted events)
//
// The only things distinguishing 3 and 5 are how they are created. Let's combine them:
//
// 0. Unused
// 1. Scheduled only
// 2. Executed consistently, still valid
// 3. Executed consistently, fields OR schedule different than when it was executed
// 4. Executed but not scheduled
//
// possible movements:
// (1, 4)->0
// 0->1
// (1, 3)->2
// (2, 4)->3
// (2, 3)->4
//
// The ExtendedTime needs attention in (1, 3, 4) but not (2, 0).
// Thus, the changes that affect "needs attention" are:
// (1, 4)->0
// 0->1
// (1, 3)->2
// 2->3
// 2->4
//
// Which can be split up into the ones CAUSED by giving the time attention:
// 4->0
// (1, 3)->2
// And the ones caused from a distance:
// 0->1 (scheduling)
// 1->0 and 2->4 (un-scheduling)
// 2->3 (invalidating)
//
// notice that the (3, 4) trap can only be escaped by giving the time attention.
// The only way to REMOVE "needs attention" from a time from a distance is 1->0.
//
//
//
// What about predictions?
// Each (RowId, PredictorId) defines one "prediction history". The prediction history may be incomplete (predictions missing after a specific ExtendedTime) but may not be invalid (containing incorrect predictions). To do that, we clear invalidated predictions whenever a FieldHistory changes (but we don't clear invalidated events/fields when a prediction history changes).
//
// The incompleteness must be simple: things can be missing after a certain time, but there can't be missing patches in the middle of the history.
//
// For each history, keep exactly a record of 0 or 1 times when we need to make a new prediction. That time is always one of the following:
// 0. Nothing
// 1. The end of the validity-interval of the last prediction in the history
// 2. The earliest nonexistent->existent transition of the corresponding field after (1) or (0).
//
// This can change in several ways:
// 3. A tail of the prediction history gets invalidated, but there are still valid predictions immediately before that. (1->1)
// 4. A tail of the prediction history gets invalidated, and there are no valid predictions immediately before that (because the field didn't exist). (1->2, 1->0, or 2->0)
// 5. The field becomes existent. (0->2)
// 6. We make a new prediction at the current prediction-needed-time. ((1 or 2)->(1 or 2))
//
// 3 and 5 are easy. 6 is the trickiest case – 6 needs to find the NEXT creation time. When 4 does 1->2, it can only be referring to a creation time at exactly the invalidation start time, because if it was an earlier one, the history would have been invalid.
//
//
//
//

enum EventValidity {
  Invalid,
  ValidWithDependencies(HashSet<FieldId>),
}
struct EventExecutionState {
  fields_changed: HashSet<FieldId>,
  validity: EventValidity,
}

struct EventState<B: Basics> {
  schedule: Option<Event<B>>,
  execution_state: Option<EventExecutionState>,
}

fn limit_option_by_value_with_none_representing_positive_infinity <B: Ord + Clone> (first: &mut Option <B>, second: &B) {
  if let Some(value) = first.as_mut() {
    if second < value {
      value.clone_from(second);
    }
  }
  if first.is_none() {
    *first = Some(second.clone());
  }
}

fn split_off_greater <K: Ord + Clone, V> (input: &mut BTreeMap<K, V>, split: & K)->BTreeMap<K, V> {
      // BTreeMap::split_off() DOES remove this splitting key, while we want to NOT include that key.
      // TODO: will Rust eventually make this easier?
      let mut result = input.split_off(split);
      if let Some(whoops) = result.remove(split) {
        input.insert(split.clone(), whoops);
      }
      result
}

fn split_off_greater_set <K: Ord + Clone> (input: &mut BTreeSet <K>, split: & K)->BTreeSet <K> {
      // BTreeMap::split_off() DOES remove this splitting key, while we want to NOT include that key.
      // TODO: will Rust eventually make this easier?
      let mut result = input.split_off(split);
      if result.remove(split) {
        input.insert(split.clone());
      }
      result
}

fn invalidate_execution<B: Basics>(time: &ExtendedTime<B>,
                                   execution: &mut EventExecutionState,
                                   events_needing_attention: &mut BTreeSet<ExtendedTime<B>>,
                                   steward_dependencies: &mut DependenciesMap<B>) {
  if let EventValidity::ValidWithDependencies(dependencies) =
         mem::replace(&mut execution.validity, EventValidity::Invalid) {
    events_needing_attention.insert(time.clone());
    for dependency in dependencies {
      match steward_dependencies.entry(dependency) {
        Entry::Vacant(_) => panic!("dependency records are inconsistent"),
        Entry::Occupied(mut entry) => {
          entry.get_mut().events.remove(time);
          if entry.get().is_empty() {
            entry.remove();
          }
        }
      }
    }
  }
}

impl<B: Basics> StewardEventsInfo<B> {
  fn schedule_event(&mut self, time: ExtendedTime<B>, event: Event<B>) -> Result<(), ()> {
    match self.event_states.entry(time.clone()) {
      Entry::Vacant(entry) => {
        entry.insert(EventState {
          schedule: Some(event),
          execution_state: None,
        });
        self.events_needing_attention.insert(time);
      }
      Entry::Occupied(mut entry) => {
        let state = entry.get_mut();
        if state.schedule.is_some() {
          return Err(());
        }
        state.schedule = Some(event);
        if state.execution_state.is_none() {
          self.events_needing_attention.insert(time);
        }
      }
    };
    Ok(())
  }

  fn unschedule_event(&mut self, time: &ExtendedTime<B>) -> Result<(), ()> {
    match self.event_states.entry(time.clone()) {
      Entry::Vacant(_) => return Err(()),
      Entry::Occupied(mut entry) => {
        if entry.get_mut().schedule.is_none() {
          return Err(());
        }
        entry.get_mut().schedule = None;
        if let Some(ref mut execution_state) = entry.get_mut().execution_state {
          invalidate_execution::<B>(time,
                                    execution_state,
                                    &mut self.events_needing_attention,
                                    &mut self.dependencies);
        }
        if entry.get().execution_state.is_none() {
          self.events_needing_attention.remove(time);
          entry.remove();
        }
      }
    };
    Ok(())
  }

  fn record_prediction_dependencies(&mut self,
                                    row_id: RowId,
                                    predictor_id: PredictorId,
                                    prediction: &Prediction<B>) {
    for dependency in prediction.predictor_accessed.iter() {
      let dependencies = self.dependencies.entry(dependency.clone()).or_insert(Default::default());
      match prediction.valid_until {
        Some(ref limit) => {
          assert!(dependencies.bounded_predictions.entry (limit.clone()).or_insert (Default::default()).insert((row_id, predictor_id)), "duplicate bounded dependency");
        }
        None => {
          assert!(dependencies.unbounded_predictions.insert((row_id, predictor_id)), "duplicate unbounded dependency");
        }
      };
    }
  }

  fn unrecord_prediction_dependencies(&mut self,
                                      row_id: RowId,
                                      predictor_id: PredictorId,
                                      already_dealt_with: Option <FieldId>,
                                      prediction: &Prediction<B>) {
    for dependency in prediction.predictor_accessed.iter() {
      if Some (*dependency) == already_dealt_with {continue;}
      match self.dependencies.entry(dependency.clone()) {
        Entry::Vacant(_) => panic!("dependency records are inconsistent"),
        Entry::Occupied(mut entry) => {
          match prediction.valid_until {
            Some(ref limit) => {
              match entry.get_mut().bounded_predictions.entry (limit.clone()){
                btree_map::Entry::Vacant(_) => panic!("missing bounded dependency set"),
                btree_map::Entry::Occupied(mut inner_entry) => {
                  assert!(inner_entry.get_mut().remove (& (row_id, predictor_id)), "missing bounded dependency");
                  if inner_entry.get().is_empty() {
                    inner_entry.remove();
                  }
                }
              }
            }
            None => {
              assert!(entry.get_mut().unbounded_predictions.remove(&(row_id, predictor_id)), "missing unbounded dependency");
            }
          };
          if entry.get().is_empty() {
            entry.remove();
          }
        }
      }
    }
  }
}

fn change_needed_prediction_time <B: Basics> (row_id: RowId, predictor_id: PredictorId, history: &mut PredictionHistory <B>, time: Option <ExtendedTime <B>>, predictions_missing_by_time: &mut BTreeMap<ExtendedTime <B>, HashSet <(RowId, PredictorId)>>) {
  if let Some(previous) = mem::replace(&mut history.next_needed, time) {
    let mut entry = match predictions_missing_by_time.entry(previous) {
      btree_map::Entry::Vacant(_) => panic!("prediction needed records are inconsistent"),
      btree_map::Entry::Occupied(entry) => entry,
    };
    assert!(entry.get_mut().remove(&(row_id, predictor_id)), "missing record in predictions_missing by time");
    if entry.get().is_empty() {
      entry.remove();
    }
  }
  if let Some(new) = history.next_needed.clone() {
    assert!(predictions_missing_by_time.entry(new)
      .or_insert(Default::default())
      .insert((row_id, predictor_id)), "attempting to make duplicate record in predictions missing by time");
  }
}

impl<B: Basics> StewardOwned<B> {
  fn invalidate_prediction_dependency(&mut self,
                                      row_id: RowId,
                                      predictor_id: PredictorId,
                                      already_dealt_with: Option <FieldId>,
                                      time: &ExtendedTime<B>,
                                      field_is_none_now: bool) {

    let mut remove = false;
    {
      let mut history = self.predictions_by_id
        .get_mut(&(row_id, predictor_id))
        .expect("access records are inconsistent");
      while let Some(mut prediction) = history.predictions.pop() {
        if prediction.valid_until.as_ref().map_or (false, | limit | limit <= time) {
          history.predictions.push(prediction);
          break;
        }
        if let Some((ref event_time, _)) = prediction.what_will_happen {
          if event_time > time {
            self.events.unschedule_event(event_time).unwrap();
          }
        }

        if prediction.made_at <= *time && !field_is_none_now {
          change_needed_prediction_time(row_id,
                                        predictor_id,
                                        &mut history,
                                        Some(time.clone()),
                                        &mut self.predictions_missing_by_time);
        }

        self.events.unrecord_prediction_dependencies(row_id, predictor_id, already_dealt_with, &prediction);

        if prediction.made_at < *time {
          prediction.valid_until = Some(time.clone());
          self.events.record_prediction_dependencies(row_id, predictor_id, &prediction);
          history.predictions.push(prediction);
        }
      }
      if let Some(next) = history.next_needed.clone() {
        if next > *time || (field_is_none_now && next >= *time) {
          change_needed_prediction_time(row_id,
                                        predictor_id,
                                        &mut history,
                                        None,
                                        &mut self.predictions_missing_by_time);
        }
      }
      if history.predictions.is_empty() && history.next_needed.is_none() {
        remove = true;
      }
    }
    if remove {
      self.predictions_by_id.remove(&(row_id, predictor_id));
    }
  }
  fn invalidate_dependencies(&mut self, id: FieldId, time: &ExtendedTime<B>) {
    let invalid_dependencies_option = if let Entry::Occupied(mut my_dependencies) = self.events
      .dependencies
      .entry(id) {
      let result = Some ((split_off_greater_set (&mut my_dependencies.get_mut().events, time),
                        split_off_greater (&mut my_dependencies.get_mut().bounded_predictions, time),
                        mem::replace(&mut my_dependencies.get_mut().unbounded_predictions, Default::default())));
      if my_dependencies.get().is_empty() {
        my_dependencies.remove();
      }
      result
    } else {
      None
    };
    if let Some((events, bounded, unbounded)) = invalid_dependencies_option {
      for access_time in events {
        invalidate_execution::<B>(&access_time,
                                      &mut self.events
                                        .event_states
                                        .get_mut(&access_time)
                                        .expect("event that accessed this field was missing")
                                        .execution_state
                                        .as_mut()
                                        .expect("event that accessed this field not marked \
                                                 executed"),
                                      &mut self.events.events_needing_attention,
                                      &mut self.events.dependencies)
      }
      for (_, list) in bounded {
        for (row_id, predictor_id) in list {
          self.invalidate_prediction_dependency(row_id, predictor_id, Some (id), time, false);
        }
      }
      for (row_id, predictor_id) in unbounded {
        self.invalidate_prediction_dependency(row_id, predictor_id, Some (id), time, false);
      }
    }
  }

  fn discard_changes(&mut self,
                     id: FieldId,
                     history: &mut FieldHistory<B>,
                     index: usize,
                     during_processing_of_event_responsible_for_first_discarded: bool,
                     snapshots: &SnapshotsData<B>,
                     shared: &StewardShared<B>) {
    if index == history.changes.len() {
      return;
    }
    history.update_snapshots(id, snapshots);
    self.invalidate_dependencies(id, &history.changes[index].last_change);
    let is_none_previously =
      history.changes.get(index.wrapping_sub(1)).map_or(true, |previous| previous.data.is_none());
      
      

      // This check could be a bit less aggressive, but this is just the simplest
      if let Some(predictors) = shared.settings.predictors_by_column.get(&id.column_id) {
        for predictor in predictors {
          if self.predictions_by_id.contains_key(&(id.row_id, predictor.predictor_id)) {
            self.invalidate_prediction_dependency(id.row_id,
                                                  predictor.predictor_id,
                                                  None,
                                                  &history.changes[index].last_change,
                                                  is_none_previously);
          }
        }
      }

    let mut discard_iter = history.changes.split_off(index).into_iter();
    if during_processing_of_event_responsible_for_first_discarded {
      discard_iter.next();
    }
    for discarded in discard_iter {
      invalidate_execution::<B>(&discarded.last_change,
                                &mut self.events
                                  .event_states
                                  .get_mut(&discarded.last_change)
                                  .expect("event that created this change was missing")
                                  .execution_state
                                  .as_mut()
                                  .expect("event that created this change not marked executed"),
                                &mut self.events.events_needing_attention,
                                &mut self.events.dependencies);
    }
  }

  fn add_change(&mut self,
                id: FieldId,
                history: &mut FieldHistory<B>,
                change: Field<B>,
                snapshots: &mut SnapshotsData<B>,
                shared: &StewardShared<B>) {
    history.changes.last().map(|last_change| assert!(last_change.last_change <change.last_change));
    history.update_snapshots(id, snapshots);
    self.invalidate_dependencies(id, &change.last_change);
    if history.changes.last().map_or(true, |previous| previous.data.is_none()) {
      assert!(change.data.is_some(), "a change from nonexistent to nonexistent shouldn't be recorded");
      if let Some(predictors) = shared.settings.predictors_by_column.get(&id.column_id) {
        for predictor in predictors {
          let prediction_history = self.predictions_by_id
            .entry((id.row_id, predictor.predictor_id))
            .or_insert(Default::default());
          if let Some(time) = prediction_history.next_needed.clone() {
            assert!(time <= change.last_change, "failed to invalidate old predictions before inserting new one");
          } else {
            change_needed_prediction_time(id.row_id,
                                          predictor.predictor_id,
                                          prediction_history,
                                          Some(change.last_change.clone()),
                                          &mut self.predictions_missing_by_time);
          }
        }
      }
    }
    if change.data.is_none() {
      if let Some(predictors) = shared.settings.predictors_by_column.get(&id.column_id) {
        for predictor in predictors {
          if self.predictions_by_id.contains_key(&(id.row_id, predictor.predictor_id)) {
            self.invalidate_prediction_dependency(id.row_id,
                                                  predictor.predictor_id,
                                                  None,
                                                  &change.last_change,
                                                  true);
          }
        }
      }
    }
    history.changes.push(change);
  }

  fn discard_event_change(&mut self,
                          id: FieldId,
                          time: &ExtendedTime<B>,
                          field_states: &mut HashMap<FieldId, FieldHistory<B>>,
                          snapshots: &mut SnapshotsData<B>,
                          shared: &StewardShared<B>) {
    if let Entry::Occupied(mut entry) = field_states.entry(id) {
      // some of these could have ALREADY been deleted –
      // in fact, perhaps that's how the event was invalidated in the first place
      if let Ok(index) = entry.get()
        .changes
        .binary_search_by_key(&time, |change| &change.last_change) {
        self.discard_changes(id, entry.get_mut(), index, true, snapshots, shared);
        if entry.get().changes.is_empty() {
          self.existent_fields.remove(id);
          entry.remove();
        }
      }
    }
  }

  fn add_event_change(&mut self,
                      id: FieldId,
                      field: Field<B>,
                      time: &ExtendedTime<B>,
                      is_replacement: bool,
                      new_dependencies: &mut HashSet<FieldId>,
                      new_fields_changed: &mut HashSet<FieldId>,
                      field_states: &mut HashMap<FieldId, FieldHistory<B>>,
                      snapshots: &mut SnapshotsData<B>,
                      shared: &StewardShared<B>) {
    new_dependencies.insert(id);
    self.events
      .dependencies
      .entry(id)
      .or_insert(Default::default())
      .events
      .insert(time.clone());
    if field.last_change == *time {
      new_fields_changed.insert(id);
      // TODO: handle erasing a non-existent field properly
      let mut history = field_states.entry(id).or_insert(FieldHistory {
        changes: Vec::new(),
        first_snapshot_not_updated: self.next_snapshot,
      });
      if history.changes.is_empty() {
        self.existent_fields.insert(id);
      }
      match history.changes.binary_search_by_key(&time, |change| &change.last_change) {
        Ok(index) => {
          assert!(is_replacement);
          // TODO: be able to check field equality in general
          // if history.changes [index].data != field.data {
          self.discard_changes(id, &mut history, index, true, snapshots, shared);
          self.add_change(id, &mut history, field, snapshots, shared);
          // }
        }
        Err(index) => {
          if !(field.data.is_none() &&
               history.changes
            .get(index.wrapping_sub(1))
            .map_or(true, |previous| previous.data.is_none())) {
            self.discard_changes(id, &mut history, index, false, snapshots, shared);
            self.add_change(id, &mut history, field, snapshots, shared);
          }
        }
      }
    }
  }
}

impl<B: Basics> Steward<B> {
  fn create_execution(&mut self,
                      time: &ExtendedTime<B>,
                      new_results: MutatorResults<B>)
                      -> EventExecutionState {
    let mut fields_guard = self.shared.fields.borrow_mut();
    let fields = &mut *fields_guard;
    let field_states = &mut fields.field_states;
    let changed_since_snapshots = &mut fields.changed_since_snapshots;
    let mut new_fields_changed = HashSet::new();
    let mut new_dependencies = HashSet::with_capacity(new_results.fields.len());
    for (id, field) in new_results.fields {
      self.owned.add_event_change(id,
                                  field,
                                  time,
                                  false,
                                  &mut new_dependencies,
                                  &mut new_fields_changed,
                                  field_states,
                                  changed_since_snapshots,
                                  &self.shared);
    }
    EventExecutionState {
      fields_changed: new_fields_changed,
      validity: EventValidity::ValidWithDependencies(new_dependencies),
    }
  }
  fn remove_execution(&mut self, time: &ExtendedTime<B>, execution: EventExecutionState) {
    let mut fields_guard = self.shared.fields.borrow_mut();
    let fields = &mut *fields_guard;
    let field_states = &mut fields.field_states;
    let changed_since_snapshots = &mut fields.changed_since_snapshots;
    for id in execution.fields_changed {
      self.owned.discard_event_change(id,
                                      time,
                                      field_states,
                                      changed_since_snapshots,
                                      &self.shared);
    }
  }
  fn replace_execution(&mut self,
                       time: &ExtendedTime<B>,
                       execution: &mut EventExecutionState,
                       new_results: MutatorResults<B>) {
    let mut fields_guard = self.shared.fields.borrow_mut();
    let fields = &mut *fields_guard;
    let field_states = &mut fields.field_states;
    let changed_since_snapshots = &mut fields.changed_since_snapshots;
    let mut new_fields_changed = HashSet::new();
    let mut new_dependencies = HashSet::with_capacity(new_results.fields.len());
    for (id, field) in new_results.fields {
      self.owned.add_event_change(id,
                                  field,
                                  time,
                                  true,
                                  &mut new_dependencies,
                                  &mut new_fields_changed,
                                  field_states,
                                  changed_since_snapshots,
                                  &self.shared);
    }
    for id in mem::replace(&mut execution.fields_changed, new_fields_changed) {
      if execution.fields_changed.get(&id).is_none() {
        self.owned.discard_event_change(id,
                                        time,
                                        field_states,
                                        changed_since_snapshots,
                                        &self.shared);
      }
    }
    execution.validity = EventValidity::ValidWithDependencies(new_dependencies);
  }

  fn do_event(&mut self, time: &ExtendedTime<B>) {
    self.owned.events.events_needing_attention.remove(time);
    let mut state = self.owned
      .events
      .event_states
      .remove(time)
      .expect("You can't do an event that wasn't scheduled");
    if let Some(event) = state.schedule {
      let results;
      {
        let field_ref = &*self.shared.fields.borrow();
        let mut mutator = Mutator {
          // steward: &mut self.owned,
          shared: &self.shared,
          fields: field_ref,
          generic: common::GenericMutator::new(time.clone()),
          results: MutatorResults { fields: insert_only::HashMap::new() },
        };
        event(&mut mutator);
        results = mutator.results;
      }
      state.schedule = Some(event);
      if let Some(ref mut execution) = state.execution_state {
        self.replace_execution(&time, execution, results);
      } else {
        state.execution_state = Some(self.create_execution(time, results));
      }
      self.owned.events.event_states.insert(time.clone(), state);
    } else {
      self.remove_execution(time,
                            state.execution_state
                              .expect("a null event state was left lying around"));
    }
  }
  fn make_prediction(&mut self, row_id: RowId, predictor_id: PredictorId, time: &ExtendedTime<B>) {
    let prediction;
    let mut history = self.owned
      .predictions_by_id
      .get_mut(&(row_id, predictor_id))
      .expect(" prediction needed records are inconsistent");
    {
      assert_eq!(history.next_needed, Some (time.clone()));
      let predictor = self.shared.settings.predictors_by_id.get(&predictor_id).unwrap().clone();
      let fields = self.shared.fields.borrow();
      assert! ( fields.get (FieldId::new (row_id, predictor.column_id), time, true).is_some());
      let mut results;
      let generic;
      {
        let field_ref = &*fields;
        let mut pa = PredictorAccessor {
          // predictor_id: predictor_id,
          // about_row_id: row_id,
          internal_now: time.clone(),
          shared: &self.shared,
          fields: field_ref,
          generic: common::GenericPredictorAccessor::new(),
          results: RefCell::new(PredictorAccessorResults { valid_until: None }),
        };
        (predictor.function)(&mut pa, row_id);
        results = pa.results.into_inner();
        generic = pa.generic;
      }
      let (dependencies, hasher) = generic.dependencies.into_inner();
      let dependencies_hash = hasher.generate();

      let what_will_happen = generic.soonest_prediction.and_then(|(event_base_time, event)| {
        common::next_extended_time_of_predicted_event(predictor_id,
                                                      row_id,
                                                      dependencies_hash,
                                                      event_base_time,
                                                      time)
          .map(|event_time| (event_time, event))
      });

      if let Some((ref event_time, _)) = what_will_happen {
        limit_option_by_value_with_none_representing_positive_infinity(&mut results.valid_until,
                                                                       event_time);
      }

      let field = fields.field_states
        .get(&FieldId::new(row_id, predictor.column_id))
        .expect("why are we making a prediction if the field never exists");
      let next_change_index = match field.changes
        .binary_search_by_key(&time, |change| &change.last_change) {
        Ok(index) => index + 1,
        Err(index) => index,
      };
      let next_needed = if let Some(&Field { data: None, last_change: ref next_change_time }) =
                               field.changes.get(next_change_index) {
        limit_option_by_value_with_none_representing_positive_infinity(&mut results.valid_until,
                                                                       &next_change_time);
        field.changes.get(next_change_index + 1).map(|next_creation| {
            assert!(next_creation.data.is_some(), "there is no need to store multiple deletions in a row");
            next_creation.last_change.clone()
          })
      } else {
        results.valid_until.clone()
      };

      change_needed_prediction_time(row_id,
                                    predictor_id,
                                    &mut history,
                                    next_needed,
                                    &mut self.owned.predictions_missing_by_time);

      prediction = Prediction {
        predictor_accessed: dependencies,
        what_will_happen: what_will_happen,
        made_at: time.clone(),
        valid_until: results.valid_until,
      };
    }
    self.owned.events.record_prediction_dependencies(row_id, predictor_id, &prediction);
    if let Some((ref event_time, ref event)) = prediction.what_will_happen {
      if prediction.valid_until.as_ref().map_or(true, |limit| event_time <= limit) {
        self.owned.events.schedule_event(event_time.clone(), event.clone()).unwrap();
      }
    }
    history.predictions.push(prediction);
  }

  fn next_stuff(&self) -> (Option<ExtendedTime<B>>, Option<(ExtendedTime<B>, RowId, PredictorId)>) {
    let next_event = self.owned.events.events_needing_attention.iter().next().cloned();
    let next_prediction = self.owned.predictions_missing_by_time.iter().next().map(|(time, set)| {
      let &(row_id, predictor_id) =
        set.iter().next().expect("empty set of predictions should have been cleaned up");
      (time.clone(), row_id, predictor_id)
    });
    (next_event, next_prediction)
  }

  fn do_next(&mut self) -> Option<ExtendedTime<B>> {
    match self.next_stuff() {
      (None, None) => None,
      (Some(event_time), None) => {
        self.do_event(&event_time);
        Some(event_time)
      }
      (None, Some((prediction_time, row_id, predictor_id))) => {
        self.make_prediction(row_id, predictor_id, &prediction_time);
        Some(prediction_time)
      }
      (Some(event_time), Some((prediction_time, row_id, predictor_id))) => {
        if event_time <= prediction_time {
          self.do_event(&event_time);
          Some(event_time)
        } else {
          self.make_prediction(row_id, predictor_id, &prediction_time);
          Some(prediction_time)
        }
      }
    }
  }
}

impl<B: Basics> FieldHistory<B> {
  fn previous_change_for_snapshot(&self, time: &B::Time) -> Option<(FieldRc, ExtendedTime<B>)> {
    use std::cmp::Ordering;
    let index = match self.changes
      .binary_search_by(|change| if change.last_change.base < *time {
        Ordering::Less
      } else {
        Ordering::Greater
      }) {
      Ok(_) => unreachable!(),
      Err(index) => index.wrapping_sub(1),
    };
    self.changes.get(index).and_then(|change| {
      change.data.as_ref().map(|data| (data.clone(), change.last_change.clone()))
    })
  }
  fn update_snapshots(&mut self, my_id: FieldId, snapshots: &SnapshotsData<B>) {
    for (index, &(ref time, ref snapshot_map)) in snapshots.iter().rev() {
      if *index < self.first_snapshot_not_updated {
        break;
      }
      snapshot_map.get_default(my_id, || self.previous_change_for_snapshot(time));
    }
    if let Some((index, _)) = snapshots.iter().rev().next() {
      self.first_snapshot_not_updated = index + 1;
    }
  }
}


#[derive (Clone)]
struct Field<B: Basics> {
  last_change: ExtendedTime<B>,
  data: Option<FieldRc>,
}
type SnapshotField<B: Basics> = (FieldRc, ExtendedTime<B>);

//#[derive (Clone)]
//enum AccessInfo {
//  EventAccess,
//  PredictionAccess(RowId, PredictorId),
//}
struct FieldHistory<B: Basics> {
  changes: Vec<Field<B>>,
  first_snapshot_not_updated: SnapshotIdx,
}

type SnapshotsData<B: Basics> = BTreeMap<SnapshotIdx,
                                         (B::Time,
                                          Rc<insert_only::HashMap<FieldId, SnapshotField<B>>>)>;

struct Fields<B: Basics> {
  field_states: HashMap<FieldId, FieldHistory<B>>,
  changed_since_snapshots: SnapshotsData<B>,
}

#[derive (Default)]
struct Dependencies<B: Basics> {
  events: BTreeSet <ExtendedTime<B>>,
  bounded_predictions: BTreeMap<ExtendedTime<B>, HashSet<(RowId, PredictorId)>>,
  unbounded_predictions: HashSet<(RowId, PredictorId)>,
}
impl<B: Basics> Dependencies<B> {
  fn is_empty(&self) -> bool {
    self.events.is_empty() && self.bounded_predictions.is_empty() && self.unbounded_predictions.is_empty()
  }
}
type DependenciesMap<B: Basics> = HashMap<FieldId, Dependencies<B>>;

#[derive (Clone)]
struct Prediction<B: Basics> {
  predictor_accessed: Vec<FieldId>,
  what_will_happen: Option<(ExtendedTime<B>, Event<B>)>,
  made_at: ExtendedTime<B>,
  valid_until: Option<ExtendedTime<B>>,
}
#[derive (Default)]
struct PredictionHistory<B: Basics> {
  next_needed: Option<ExtendedTime<B>>,
  predictions: Vec<Prediction<B>>,
}


struct StewardShared<B: Basics> {
  settings: Settings<B>,
  constants: B::Constants,
  fields: RefCell<Fields<B>>,
}

struct StewardEventsInfo<B: Basics> {
  event_states: HashMap<ExtendedTime<B>, EventState<B>>,
  events_needing_attention: BTreeSet<ExtendedTime<B>>,
  dependencies: DependenciesMap<B>,
}

struct StewardOwned<B: Basics> {
  events: StewardEventsInfo<B>,

  invalid_before: ValidSince<B::Time>,
  next_snapshot: SnapshotIdx,
  existent_fields: partially_persistent_nonindexed_set::Set<FieldId>,

  predictions_by_id: HashMap<(RowId, PredictorId), PredictionHistory<B>>,
  predictions_missing_by_time: BTreeMap<ExtendedTime<B>, HashSet<(RowId, PredictorId)>>,
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
pub struct MutatorResults<B: Basics> {
  fields: insert_only::HashMap<FieldId, Field<B>>,
}
pub struct Mutator<'a, B: Basics> {
  shared: &'a StewardShared<B>,
  fields: &'a Fields<B>,
  generic: common::GenericMutator<B>,
  results: MutatorResults<B>,
}
struct PredictorAccessorResults<B: Basics> {
  valid_until: Option<ExtendedTime<B>>,
}
pub struct PredictorAccessor<'a, B: Basics> {
  // predictor_id: PredictorId,
  // about_row_id: RowId,
  internal_now: ExtendedTime<B>,
  shared: &'a StewardShared<B>,
  fields: &'a Fields<B>,
  generic: common::GenericPredictorAccessor<B, Event<B>>,
  results: RefCell<PredictorAccessorResults<B>>,
}
pub type EventFn<B> = for<'d, 'e> Fn(&'d mut Mutator<'e, B>);
pub type Event<B> = StewardRc<EventFn<B>>;

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
    self.field_states
      .get_default(id, || {
        self.shared
          .fields
          .borrow()
          .get_for_snapshot(id, &self.now, self.index)
      })
      .map(|whatever| (&whatever.0, &whatever.1))
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
    let field = self.results
      .fields
      .get_default(id, || {
        Some(match self.fields.get(id, &self.generic.now, false) {
          None => {
            Field {
              data: None,
              last_change: self.generic.now.clone(),
            }
          }
          Some((data, time)) => {
            Field {
              data: Some(data.clone()),
              last_change: time.clone(),
            }
          }
        })
      })
      .unwrap();
    field.data.as_ref().map(|data| (data, &field.last_change))
  }
  fn constants(&self) -> &B::Constants {
    &self.shared.constants
  }
  time_steward_common_accessor_methods_for_mutator!(B);
}
impl<'a, B: Basics> PredictorAccessor<'a, B> {
  fn get_impl(&self, id: FieldId) -> Option<(&FieldRc, &ExtendedTime<B>)> {
    self.fields.get_and_next(id, &self.internal_now, true).map(|(result, next)| {
      assert! (*result.1 <= self.internal_now);
      if let Some(limit) = next {
        assert!(*limit >self.internal_now);
        limit_option_by_value_with_none_representing_positive_infinity(&mut self.results
                                                                         .borrow_mut()
                                                                         .valid_until,
                                                                       limit);
      }
      result
    })
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
  time_steward_common_predictor_accessor_methods_for_predictor_accessor!(B, DynamicEventFn);
}

impl<B: Basics> ::Snapshot<B> for Snapshot<B> {
  fn num_fields(&self) -> usize {
    // TODO: optimize
    self.into_iter().count()
  }
}

pub struct SnapshotIter<'a, B: Basics>(partially_persistent_nonindexed_set::SnapshotIter<'a,
                                                                                         FieldId>,
                                       &'a Snapshot<B>);
impl<'a, B: Basics> Iterator for SnapshotIter<'a, B> {
  type Item = (FieldId, (&'a FieldRc, &'a ExtendedTime<B>));
  fn next(&mut self) -> Option<Self::Item> {
    loop {
      match (self.0).next().map(|id| (id, (self.1).generic_data_and_extended_last_change(id))) {
        None => return None,
        Some((id, Some(value))) => return Some((id, value)),
        _ => (),
      }
    }
  }
  fn size_hint(&self) -> (usize, Option<usize>) {
    (0, Some(self.1.num_fields))
  }
}
impl<'a, B: Basics> IntoIterator for &'a Snapshot<B> {
  type Item = (FieldId, (&'a FieldRc, &'a ExtendedTime<B>));
  type IntoIter = SnapshotIter<'a, B>;
  fn into_iter(self) -> Self::IntoIter {
    SnapshotIter(self.field_ids.iter(), self)
  }
}

impl<'a, B: Basics> ::Mutator<B> for Mutator<'a, B> {
  fn set<C: Column>(&mut self, id: RowId, data: Option<C::FieldType>) {
    let field_id = FieldId::new(id, C::column_id());
    self.results.fields.insert(field_id,
                               Field {
                                 last_change: self.generic.now.clone(),
                                 data: data.map(|whatever| {
                                   let something: FieldRc = StewardRc::new(whatever);
                                   something
                                 }),
                               });
  }
  time_steward_common_mutator_methods_for_mutator!(B);
}
impl<'a, B: Basics> Rng for Mutator<'a, B> {
  time_steward_common_rng_methods_for_mutator!(B);
}

impl<B: Basics> Fields<B> {
  fn get(&self,
         id: FieldId,
         time: &ExtendedTime<B>,
         after: bool)
         -> Option<(&FieldRc, &ExtendedTime<B>)> {
    self.get_and_next(id, time, after).map(|whatever| whatever.0)
  }
  fn get_and_next(&self,
                  id: FieldId,
                  time: &ExtendedTime<B>,
                  after: bool)
                  -> Option<((&FieldRc, &ExtendedTime<B>), Option<&ExtendedTime<B>>)> {
    self.field_states.get(&id).and_then(|history| {
      let index = match history.changes.binary_search_by_key(&time, |change| &change.last_change) {
        Ok(index) => if after { index } else { index.wrapping_sub(1) },
        Err(index) => index.wrapping_sub(1),
      };
      history.changes.get(index).and_then(|change| {
        change.data.as_ref().map(|data| {
          ((data, &change.last_change),
           history.changes.get(index + 1).map(|result| &result.last_change))
        })
      })
    })
  }
  fn get_for_snapshot(&self,
                      id: FieldId,
                      time: &B::Time,
                      index: SnapshotIdx)
                      -> Option<(FieldRc, ExtendedTime<B>)> {
    self.field_states.get(&id).and_then(|history| {
      if history.first_snapshot_not_updated > index {
        return None;
      }
      history.previous_change_for_snapshot(time)
    })
  }
}
impl<B: Basics> Steward<B> {
  fn update_until_beginning_of(&mut self, target_time: &B::Time) {
    while self.updated_until_before().map_or (false, | time | time < *target_time) {
      self.do_next();
    }
  }
}


impl<B: Basics> TimeSteward<B> for Steward<B> {
  type Snapshot = Snapshot<B>;
  type Settings = Settings<B>;

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
        invalid_before: ValidSince::TheBeginning,
        next_snapshot: 0,
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
    result.shared.fields.borrow_mut().field_states = snapshot.into_iter()
      .map(|(id, stuff)| {
        if match last_event {
          None => true,
          Some(ref time) => stuff.1 > time,
        } {
          last_event = Some(stuff.1.clone());
        }
        result.owned.existent_fields.insert(id);
        result.shared.settings.predictors_by_column.get(&id.column_id).map(|predictors| {
          for predictor in predictors {
            predictions_needed.insert((id.row_id, predictor.predictor_id));
          }
        });
        (id,
         FieldHistory {
          changes: vec![Field {
                  data: Some (stuff.0.clone()),
                  last_change: stuff.1.clone(),
                }],
          first_snapshot_not_updated: 0,
        })
      })
      .collect();
    for &(row_id, predictor_id) in predictions_needed.iter() {
      result.owned.predictions_by_id.insert((row_id, predictor_id),
                                            PredictionHistory {
                                              next_needed: last_event.clone(),
                                              predictions: Vec::new(),
                                            });
      result.owned
        .predictions_missing_by_time
        .entry(last_event.clone().unwrap())
        .or_insert(Default::default())
        .insert((row_id, predictor_id));
    }
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
    match self.owned.events.schedule_event(common::extended_time_of_fiat_event(time, id),
                                           StewardRc::new(DynamicEventFn::new(event))) {
      Err(_) => Err(FiatEventOperationError::InvalidInput),
      Ok(_) => Ok(()),
    }
  }

  fn erase_fiat_event(&mut self,
                      time: &B::Time,
                      id: DeterministicRandomId)
                      -> Result<(), FiatEventOperationError> {
    if self.valid_since() > *time {
      return Err(FiatEventOperationError::InvalidTime);
    }
    match self.owned
      .events
      .unschedule_event(&common::extended_time_of_fiat_event(time.clone(), id)) {
      Err(_) => Err(FiatEventOperationError::InvalidInput),
      Ok(_) => Ok(()),
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
      .1
      .clone();
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

impl<B: Basics> IncrementalTimeSteward<B> for Steward<B> {
  fn step(&mut self) {
    self.do_next();
  }
  fn updated_until_before(&self) -> Option<B::Time> {
    match self.next_stuff() {
      (None, None) => None,
      (Some(event_time), None) => Some(event_time.base),
      (None, Some((prediction_time, _, _))) => Some(prediction_time.base),
      (Some(event_time), Some((prediction_time, _, _))) => {
        if event_time <= prediction_time {
          Some(event_time.base)
        } else {
          Some(prediction_time.base)
        }
      }
    }
  }
}

/*
Wait, we don't actually need to do this, because self.shared isn't dropped as long as the snapshot exist!
impl<B: Basics> Drop for Steward<B> {
  fn drop(&mut self) {
    let mut fields_guard = self.shared.fields.borrow_mut();
    let fields = &mut*fields_guard;
    for (id, field) in fields.field_states.iter_mut() {
      field.update_snapshots (*id, & fields.changed_since_snapshots);
    }
  }
}
*/

