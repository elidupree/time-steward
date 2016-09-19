use super::types::*;
// use stewards::amortized::{EventExecutionState, StewardOwned, StewardShared, FieldHistory, StewardEventsInfo, EventValidity, Field, limit_option_by_value_with_none_representing_positive_infinity, split_off_greater, split_off_greater_set, SnapshotsData, Prediction, PredictionHistory, PredictorAccessorResults, DependenciesMap, DynamicEvent, EventState};

use {SiphashIdGenerator, RowId, FieldId, PredictorId, TimeId,
     FieldRc, ExtendedTime, Basics, TimeSteward};
use implementation_support::common::{self, field_options_are_equal};
use std::collections::{HashMap, BTreeMap, HashSet, BTreeSet, btree_map};
use std::collections::hash_map::Entry;
// use std::collections::Bound::{Included, Excluded, Unbounded};
use std::cell::RefCell;
use std::mem;
use implementation_support::insert_only;

pub fn invalidate_execution<B: Basics>(time: &ExtendedTime<B>,
                                       execution: &mut EventExecutionState,
                                       already_dealt_with: Option<FieldId>,
                                       events_needing_attention: &mut BTreeSet<ExtendedTime<B>>,
                                       steward_dependencies: &mut DependenciesMap<B>) {
  if let EventValidity::ValidWithDependencies(dependencies) =
         mem::replace(&mut execution.validity, EventValidity::Invalid) {
    events_needing_attention.insert(time.clone());
    for dependency in dependencies {
      if Some(dependency) == already_dealt_with {
        continue;
      }
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
  pub fn schedule_event(&mut self,
                        time: ExtendedTime<B>,
                        event: DynamicEvent<B>,
                        scheduled_by: Option <(RowId, PredictorId)>)
                        -> Result<(), ()> {
    match self.event_states.entry(time.clone()) {
      Entry::Vacant(entry) => {
        entry.insert(EventState {
          schedule: Some(event),
          scheduled_by: scheduled_by,
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
        state.scheduled_by = scheduled_by;
        if state.execution_state.is_none() {
          self.events_needing_attention.insert(time);
        }
      }
    };
    Ok(())
  }

  pub fn unschedule_event(&mut self, time: &ExtendedTime<B>) -> Result<(), ()> {
    match self.event_states.entry(time.clone()) {
      Entry::Vacant(_) => return Err(()),
      Entry::Occupied(mut entry) => {
        if entry.get_mut().schedule.is_none() {
          return Err(());
        }
        entry.get_mut().schedule = None;
        entry.get_mut().scheduled_by = None;
        if let Some(ref mut execution_state) = entry.get_mut().execution_state {
          invalidate_execution::<B>(time,
                                    execution_state, None,
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

  pub fn record_prediction_dependencies(&mut self,
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

  pub fn unrecord_prediction_dependencies(&mut self,
                                          row_id: RowId,
                                          predictor_id: PredictorId,
                                          already_dealt_with: Option<FieldId>,
                                          prediction: &Prediction<B>) {
    for dependency in prediction.predictor_accessed.iter() {
      if Some(*dependency) == already_dealt_with {
        continue;
      }
      match self.dependencies.entry(dependency.clone()) {
        Entry::Vacant(_) => panic!("dependency records are inconsistent"),
        Entry::Occupied(mut entry) => {
          match prediction.valid_until {
            Some(ref limit) => {
              match entry.get_mut().bounded_predictions.entry(limit.clone()) {
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

pub fn change_needed_prediction_time <B: Basics> (row_id: RowId, predictor_id: PredictorId, history: &mut PredictionHistory <B>, time: Option <ExtendedTime <B>>, predictions_missing_by_time: &mut BTreeMap<ExtendedTime <B>, HashSet <(RowId, PredictorId)>>) {
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
  pub fn invalidate_prediction_dependency(&mut self,
                                          row_id: RowId,
                                          predictor_id: PredictorId,
                                          already_dealt_with: Option<FieldId>,
                                          time: &ExtendedTime<B>,
                                          field_changes_removed: bool,
                                          field_is_none_now: bool) {

    let mut remove = false;
    {
      let mut history = self.predictions_by_id
        .get_mut(&(row_id, predictor_id))
        .expect("access records are inconsistent");
      while let Some(mut prediction) = history.predictions.pop() {
        if prediction.valid_until.as_ref().map_or(false, |limit| limit <= time) {
          history.predictions.push(prediction);
          break;
        }
        if let Some((ref event_time, _)) = prediction.what_will_happen {
          if event_time > time &&
             prediction.valid_until.as_ref().map_or(true, |limit| limit >= event_time) {
            self.events.unschedule_event(event_time).unwrap();
          }
        }

        if !field_changes_removed {
          let new_needed = ::std::cmp::max (prediction.made_at.clone(), time.clone());
          assert!(history.next_needed.as_ref().map_or(true, |next| new_needed < *next));
          change_needed_prediction_time(row_id,
                                        predictor_id,
                                        &mut history,
                                        Some(new_needed),
                                        &mut self.predictions_missing_by_time);
        }

        self.events
          .unrecord_prediction_dependencies(row_id, predictor_id, already_dealt_with, &prediction);

        if prediction.made_at < *time {
          prediction.valid_until = Some(time.clone());
          self.events.record_prediction_dependencies(row_id, predictor_id, &prediction);
          history.predictions.push(prediction);
        }
      }
      if field_changes_removed {
        if let Some(next) = history.next_needed.clone() {
          if field_is_none_now && next >= *time {
            change_needed_prediction_time(row_id,
                                          predictor_id,
                                          &mut history,
                                          None,
                                          &mut self.predictions_missing_by_time);
          }
          if !field_is_none_now && next > *time {
            change_needed_prediction_time(row_id,
                                          predictor_id,
                                          &mut history,
                                          Some(time.clone()),
                                          &mut self.predictions_missing_by_time);
          }
        }
        else if !field_is_none_now {
          change_needed_prediction_time(row_id,
                                        predictor_id,
                                        &mut history,
                                        Some(time.clone()),
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
  pub fn invalidate_dependencies(&mut self, id: FieldId, time: &ExtendedTime<B>) {
    let invalid_dependencies_option = if let Entry::Occupied(mut my_dependencies) = self.events
      .dependencies
      .entry(id) {
      let result = Some((split_off_greater_set(&mut my_dependencies.get_mut().events, time),
                         split_off_greater(&mut my_dependencies.get_mut().bounded_predictions,
                                           time),
                         mem::replace(&mut my_dependencies.get_mut().unbounded_predictions,
                                      Default::default())));
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
                                    .expect("event that accessed this field not marked executed"),
                                  Some(id),
                                  &mut self.events.events_needing_attention,
                                  &mut self.events.dependencies)
      }
      let mut already_handled = HashSet::with_capacity(bounded.len());
      for (_, list) in bounded {
        for (row_id, predictor_id) in list {
          if already_handled.insert ((row_id, predictor_id)) {
            self.invalidate_prediction_dependency(row_id, predictor_id, Some(id), time, false, false);
          }
        }
      }
      for (row_id, predictor_id) in unbounded {
        if !already_handled.contains (&(row_id, predictor_id)) {
          self.invalidate_prediction_dependency(row_id, predictor_id, Some(id), time, false, false);
        }
      }
    }
  }

  pub fn discard_changes(&mut self,
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
        //if self.predictions_by_id.contains_key(&(id.row_id, predictor.predictor_id)) {
          self.invalidate_prediction_dependency(id.row_id,
                                                predictor.predictor_id,
                                                None,
                                                &history.changes[index].last_change,
                                                true,
                                                is_none_previously);
        //}
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
                                None,
                                &mut self.events.events_needing_attention,
                                &mut self.events.dependencies);
    }
  }

  pub fn add_change(&mut self,
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
                                                  true,
                                                  true);
          }
        }
      }
    }
    history.changes.push(change);
  }

  pub fn discard_event_change(&mut self,
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

  pub fn add_event_change(&mut self,
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
          if !field_options_are_equal::<B>(id.column_id,
                                             history.changes[index].data.as_ref(),
                                             field.data.as_ref()) {
            self.discard_changes(id, &mut history, index, true, snapshots, shared);
            self.add_change(id, &mut history, field, snapshots, shared);
          }
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
  pub(super) fn create_execution(&mut self,
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
    let checksum = new_results.checksum_generator.into_inner().generate().data()[0];
    if let Some(checksum_info) = self.owned.checksum_info.as_mut() {
      checksum_info.add_event_checksum(checksum, &time.base);
    }
    EventExecutionState {
      fields_changed: new_fields_changed,
      checksum: checksum,
      validity: EventValidity::ValidWithDependencies(new_dependencies),
    }
  }
  pub(super) fn remove_execution(&mut self,
                                 time: &ExtendedTime<B>,
                                 execution: EventExecutionState) {
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
    if let Some(checksum_info) = self.owned.checksum_info.as_mut() {
      checksum_info.add_event_checksum(execution.checksum.wrapping_neg(), &time.base);
    }
  }
  pub(super) fn replace_execution(&mut self,
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
    if let Some(checksum_info) = self.owned.checksum_info.as_mut() {
      checksum_info.add_event_checksum(execution.checksum.wrapping_neg(), &time.base);
    }
    execution.checksum = new_results.checksum_generator.into_inner().generate().data()[0];
    if let Some(checksum_info) = self.owned.checksum_info.as_mut() {
      checksum_info.add_event_checksum(execution.checksum, &time.base);
    }
    execution.validity = EventValidity::ValidWithDependencies(new_dependencies);
  }

  pub(super) fn do_event(&mut self, time: &ExtendedTime<B>) {
    self.owned.events.events_needing_attention.remove(time);
    let mut state = self.owned
      .events
      .event_states
      .remove(time)
      .expect("You can't do an event that wasn't scheduled");
    if let Some(event) = state.schedule {
      let results;
      {
        let mut checksum_generator = SiphashIdGenerator::new();
        let field_ref = &*self.shared.fields.borrow();
        ::bincode::serde::serialize_into(&mut checksum_generator,
                                         &(time, event.event_id()),
                                         ::bincode::SizeLimit::Infinite).unwrap();
        let mut mutator = Mutator {
          // steward: &mut self.owned,
          shared: &self.shared,
          fields: field_ref,
          generic: common::GenericMutator::new(time.clone()),
          results: MutatorResults {
            fields: insert_only::HashMap::with_capacity(16),
            checksum_generator: RefCell::new(checksum_generator),
          },
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
  pub(super) fn make_prediction(&self,
                                row_id: RowId,
                                predictor_id: PredictorId,
                                time: &ExtendedTime<B>)->(Prediction <B>, Option <ExtendedTime <B>>, bool) {
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
          results: RefCell::new(PredictorAccessorResults { valid_until: None, used_unsafe_now: false, }),
        };
        (predictor.function)(&mut pa, row_id);
        results = pa.results.into_inner();
        generic = pa.generic;
      }
      let (dependencies, hasher) = generic.dependencies.into_inner();
      let dependencies_hash = hasher.generate();

      let what_will_happen = generic.soonest_prediction.into_inner().and_then(|(event_base_time, event)| {
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
        if results.valid_until.as_ref().unwrap() < next_change_time {
          results.valid_until.clone()
        }else{
        field.changes.get(next_change_index + 1).map(|next_creation| {
            assert!(next_creation.data.is_some(), "there is no need to store multiple deletions in a row");
            next_creation.last_change.clone()
          })
        }
      } else {
        results.valid_until.clone()
      };

      (Prediction {
        // unique the dependencies
        predictor_accessed: dependencies.into_iter().collect::<HashSet <FieldId>>().into_iter().collect(),
        what_will_happen: what_will_happen,
        made_at: time.clone(),
        valid_until: results.valid_until,
      }, next_needed, results.used_unsafe_now)
  }
  
  pub(super) fn test_prediction(&self,
                                row_id: RowId,
                                predictor_id: PredictorId,
                                time: &ExtendedTime<B>, compare_with: &Prediction <B>) {
    let (prediction, _, _) = self.make_prediction (row_id, predictor_id, time);
    match (prediction.what_will_happen.as_ref(), compare_with.what_will_happen.as_ref()) {
      (None, None) =>(),
      (Some (&(ref event_time, ref event)), Some (&(ref original_event_time, ref original_event)))
        if event_time.base == original_event_time.base && event.event_id() == original_event.event_id()
        // TODO: also check the event data equality
        =>(),
      _=> if compare_with.made_at.base == time.base {
        panic!("predictor {:?} gave different results when given the same inputs:\n{:?}\nversus\n{:?}", predictor_id, compare_with, & prediction)
      } else {
        panic!("predictor {:?} gave different results with different values of unsafe_now():\n{:?}\nversus\n{:?}", predictor_id, compare_with, & prediction)
      },
    }
  }
  
  pub(super) fn do_prediction(&mut self,
                                row_id: RowId,
                                predictor_id: PredictorId,
                                time: &ExtendedTime<B>) {
    let (prediction, next_needed, used_unsafe_now) = self.make_prediction (row_id, predictor_id, time);
    
    if cfg! (debug_assertions) && false {
      self.test_prediction (row_id, predictor_id, time, &prediction);
      if used_unsafe_now {
        if let Some (limit) = prediction.valid_until.clone() {
          let mut hack_time = limit.clone();
          assert!(limit > *time);
          for index in 0.. {hack_time.id = TimeId::new (& index); if hack_time < limit && hack_time > *time {break;}}
          assert!(hack_time < limit);
          self.test_prediction (row_id, predictor_id, & hack_time, &prediction);
        }
      }
    }
    
    let mut history = self.owned
      .predictions_by_id
      .get_mut(&(row_id, predictor_id))
      .expect(" prediction needed records are inconsistent");
    assert_eq!(history.next_needed, Some (time.clone()));
    
    change_needed_prediction_time(row_id,
                                  predictor_id,
                                  &mut history,
                                  next_needed,
                                  &mut self.owned.predictions_missing_by_time);
    
    self.owned.events.record_prediction_dependencies(row_id, predictor_id, &prediction);
    if let Some((ref event_time, ref event)) = prediction.what_will_happen {
      if prediction.valid_until.as_ref().map_or(true, |limit| event_time <= limit) {
        self.owned.events.schedule_event(event_time.clone(), event.clone(), Some ((row_id, predictor_id))).unwrap();
      }
    }
    history.predictions.push(prediction);
  }

  pub(super) fn next_stuff
    (&self)
     -> (Option<ExtendedTime<B>>, Option<(ExtendedTime<B>, RowId, PredictorId)>) {
    let next_event = self.owned.events.events_needing_attention.iter().next().cloned();
    let next_prediction = self.owned.predictions_missing_by_time.iter().next().map(|(time, set)| {
      let &(row_id, predictor_id) =
        set.iter().next().expect("empty set of predictions should have been cleaned up");
      (time.clone(), row_id, predictor_id)
    });
    (next_event, next_prediction)
  }

  pub(super) fn do_next(&mut self) -> Option<ExtendedTime<B>> {
    match self.next_stuff() {
      (None, None) => None,
      (Some(event_time), None) => {
        self.do_event(&event_time);
        Some(event_time)
      }
      (None, Some((prediction_time, row_id, predictor_id))) => {
        self.do_prediction(row_id, predictor_id, &prediction_time);
        Some(prediction_time)
      }
      (Some(event_time), Some((prediction_time, row_id, predictor_id))) => {
        if event_time <= prediction_time {
          self.do_event(&event_time);
          Some(event_time)
        } else {
          self.do_prediction(row_id, predictor_id, &prediction_time);
          Some(prediction_time)
        }
      }
    }
  }
  
  pub(super) fn test_lots (&self) {
    let mut accounted_events = HashMap::new();
    
    for (& (row_id, predictor_id), history) in self.owned.predictions_by_id.iter() {
      for index in 1..(history.predictions.len()) {
        assert!(history.predictions [index-1].valid_until.as_ref().expect("internal TimeSteward error: non-final prediction had unbounded valid_until") <= &history.predictions [index].made_at, "internal TimeSteward error: predictions out of order");
      }
      for prediction in history.predictions.iter() {
        prediction.valid_until.as_ref().map(| limit | assert!(*limit > prediction.made_at, "internal TimeSteward error: prediction had negative validity duration"));
        self.test_prediction (row_id, predictor_id, &prediction.made_at, & prediction);
        if let Some (& (ref event_time,_)) = prediction.what_will_happen.as_ref() {
          if prediction.valid_until.as_ref().map_or (true, | limit | limit >= event_time) {
            assert!(accounted_events.insert (event_time.clone(), (row_id, predictor_id)).is_none(), "internal TimeSteward error: 2 predictors predicted an event of the same time");
          }
        }
      }
    }
    
    for (time, state) in self.owned.events.event_states.iter() {
      if let Some (ids) = state.scheduled_by.as_ref() {
        assert!(accounted_events.get (time).expect("internal TimeSteward error: an event claims to have been predicted, but nothing predicted it") == ids, "internal TimeSteward error: an event claims to have been predicted by a certain predictor, but it was predicted by a different predictor");
      }
    }
    
    self.test_prediction_existences();
  }
  pub(super) fn test_prediction_existences (&self) {
    for (id, history) in self.shared.fields.borrow().field_states.iter() {
      for change in history.changes.iter() {
        if let Some (predictors) = self.shared.settings.predictors_by_column.get (&id.column_id) {
          for predictor in predictors.iter() {
            let prediction_history = self.owned.predictions_by_id.get (&(id.row_id, predictor.predictor_id)).unwrap();
            if prediction_history.next_needed.as_ref().map_or (true, | threshold | *threshold >change.last_change && self.valid_since() < threshold.base) {
              let mut satisfied = false;
              for prediction in prediction_history.predictions.iter() {
                if (prediction.made_at <= change.last_change ||
                (self.valid_since() > prediction.made_at.base && self.valid_since() > change.last_change.base)) && prediction.valid_until.as_ref().map_or (true, | limit | *limit >change.last_change) {
                  satisfied = true;
                }
              }
              assert_eq!(satisfied, change.data.is_some(), "internal TimeSteward error: prediction existences are inconsistent with field existences{:?}{:?}",id,predictor.predictor_id);
            }
          }
        }
      }
    }
  }
}

impl<B: Basics> FieldHistory<B> {
  pub fn previous_change_for_snapshot(&self, time: &B::Time) -> Option<(FieldRc, ExtendedTime<B>)> {
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
  pub fn update_snapshots(&mut self, my_id: FieldId, snapshots: &SnapshotsData<B>) {
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
