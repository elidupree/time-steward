#[doc (hidden)]
#[macro_export]
macro_rules! time_steward_define_simple_timeline {
  () => {
pub mod simple_timeline {
use std::collections::{BTreeSet, VecDeque};
use std::mem;

use super::super::super::api::*;
use super::*;
use implementation_support::common::{split_off_greater_set};

#[derive (Copy, Clone, PartialEq, Eq, Hash, Serialize, Deserialize, Debug)]
pub struct GetData;
impl StewardData for GetData {}
#[derive (Copy, Clone, PartialEq, Eq, Hash, Serialize, Deserialize, Debug)]
pub struct GetDataAndSettingEvent;
impl StewardData for GetDataAndSettingEvent {}

pub trait IterateUniquelyOwnedPredictions <Steward: TimeSteward> {
  fn iterate_predictions <F: FnMut (& Steward::EventHandle)> (&self, _callback: &mut F) {}
}


#[serde(bound = "")]
#[derive (Serialize, Deserialize, Derivative)]
#[derivative (Clone (bound=""), Debug (bound=""))]
pub struct SimpleTimeline <VaryingData: StewardData, Steward: TimeSteward> {
  // Hacky workaround for https://github.com/rust-lang/rust/issues/41617 (see https://github.com/serde-rs/serde/issues/943)
  initial: VaryingData,
  #[serde(deserialize_with = "::serde::Deserialize::deserialize")]
  changes: VecDeque<(<Steward as TimeSteward>::EventHandle, VaryingData)>,
  #[serde(deserialize_with = "::serde::Deserialize::deserialize")]
  other_dependent_events: BTreeSet<<Steward as TimeSteward>::EventHandle>,
}

impl <VaryingData: StewardData, Steward: TimeSteward> SimpleTimeline <VaryingData, Steward> {
  pub fn new (initial: VaryingData)->Self {
    SimpleTimeline {
      initial: initial,
      changes: VecDeque::new(),
      other_dependent_events: BTreeSet::new(),
    }
  }
    
  fn search_changes (&self, time: &ExtendedTime <Steward::Basics>) -> Result <usize, usize> {
    // search at the end first, because we are usually in the present.
    
    match self.changes.back() {
      None => return Err(0),
      Some (&(ref event, _)) => {
        match event.extended_time().cmp(time) {
          Ordering::Greater => (),
          Ordering::Equal => return Ok(self.changes.len()-1),
          Ordering::Less => return Err(self.changes.len()),
        }
      },
    }
    
    let mut drop = 2;
    let mut max = self.changes.len()-1;
    let mut min;
    loop {
      if let Some(further) = self.changes.len().checked_sub (drop) {
        let change = self.changes.get (further).unwrap();
        match change.0.extended_time().cmp(time) {
          Ordering::Greater => {
            max = further;
            drop *= 2;
          },
          Ordering::Equal => return Ok(further),
          Ordering::Less => {
            min = further;
            break;
          },
        }
      }
      else {
        min = 0;
        break;
      }
    }
    while max >= min+2 {
      let mid = (max + min) >> 1;
      let change = self.changes.get (mid).unwrap();
      match change.0.extended_time().cmp(time) {
        Ordering::Greater => max = mid,
        Ordering::Equal => return Ok(mid),
        Ordering::Less => min = mid,
      }
    }
    
    if self.changes.get (min).unwrap().0.extended_time() > time {
      return Err (min)
    }
    Err(max)
  }
}
impl <VaryingData: StewardData + IterateUniquelyOwnedPredictions <Steward>, Steward: TimeSteward> SimpleTimeline <VaryingData, Steward> {
  fn remove_present_and_future <Accessor: FutureCleanupAccessor<Steward = Steward>> (&mut self, accessor: &Accessor) {
    let removed = split_off_greater_set (&mut self.other_dependent_events, accessor.extended_now());
    for event in removed {
      accessor.invalidate_execution(&event);
    }
    while let Some (change) = self.changes.pop_back() {
      if change.0.extended_time() < accessor.extended_now() {
        IterateUniquelyOwnedPredictions::<Steward>::iterate_predictions (&change.1, &mut | prediction | accessor.change_prediction_destroyer (prediction, None));
        self.changes.push_back (change);
        break
      }
      IterateUniquelyOwnedPredictions::<Steward>::iterate_predictions (&change.1, &mut | prediction | accessor.change_prediction_destroyer (prediction, Some(& change.0)));
      if change.0.extended_time() > accessor.extended_now() {
        accessor.invalidate_execution(&change.0);
      }
    }
  }
  
  fn modify <Accessor: EventAccessor<Steward = Steward>> (&mut self, modification: VaryingData, accessor: &Accessor){
    if let Some(last) = self.changes.back() {
      assert!(& last.0 <= accessor.handle(), "All future changes should have been cleared before calling modify() ");
      IterateUniquelyOwnedPredictions::<Steward>::iterate_predictions (&last.1, &mut | prediction | accessor.destroy_prediction (prediction));
    }
    // we don't need to create incoming predictions, because they can't be incoming unless they were already created
    self.changes.push_back ((accessor.handle().clone(), modification));
  }
}

impl <VaryingData: StewardData, Steward: TimeSteward> DataTimeline for SimpleTimeline <VaryingData, Steward> {
  type Basics = Steward::Basics;
  
  fn clone_for_snapshot (&self, time: &ExtendedTime <Self::Basics>)->Self {
    let mut changes = VecDeque::new();
    match self.search_changes(&time) {
      Ok(index) => {
        if let Some(previous) = index.checked_sub (1) {changes.push_back (self.changes [previous].clone());}
        changes.push_back (self.changes [index].clone());
      },
      Err (index) => if let Some(previous) = index.checked_sub (1) {changes.push_back (self.changes [previous].clone());},
    };
    SimpleTimeline {
      initial: self.initial.clone(),
      changes: changes,
      other_dependent_events: BTreeSet::new(),
    }
  }
  
  fn forget_before (&mut self, time: &ExtendedTime <Self::Basics>) {
    let retained = self.other_dependent_events.split_off (time);
    mem::replace (&mut self.other_dependent_events, retained);
    
    while self.changes.front().map_or (false, | change | change.0.extended_time() < time) {
      self.changes.pop_front ();
    }
  }
}
impl <'a, VaryingData: StewardData, Steward: TimeSteward> DataTimelineQueriableWith<'a, GetDataAndSettingEvent, GetDataAndSettingEvent> for SimpleTimeline <VaryingData, Steward> {
  type QueryResultOwned = (VaryingData, Option <<Steward as TimeSteward>::EventHandle>);
  type QueryResult = UnnecessaryWrapper<(&'a VaryingData, Option <&'a <Steward as TimeSteward>::EventHandle>)>;

  fn query (&'a self, _: GetDataAndSettingEvent, time: &ExtendedTime <Self::Basics>, offset: QueryOffset)->Self::QueryResult {
    let previous_change_index = match self.search_changes(&time) {
      Ok(index) => match offset {QueryOffset::After => index, QueryOffset::Before => index.wrapping_sub (1)},
      Err (index) => index.wrapping_sub (1),
    };
    UnnecessaryWrapper (match self.changes.get (previous_change_index) {
      Some (change) => (&change.1, Some(&change.0)),
      None => (&self.initial, None),
    })
  }
}
impl <'a, VaryingData: StewardData, Steward: TimeSteward> DataTimelineQueriableWith<'a, GetData, GetData> for SimpleTimeline <VaryingData, Steward> {
  type QueryResultOwned = VaryingData;
  type QueryResult = &'a VaryingData;

  fn query (&'a self, _: GetData, time: &ExtendedTime <Self::Basics>, offset: QueryOffset)->Self::QueryResult {
    let previous_change_index = match self.search_changes(&time) {
      Ok(index) => match offset {QueryOffset::After => index, QueryOffset::Before => index.wrapping_sub (1)},
      Err (index) => index.wrapping_sub (1),
    };
    match self.changes.get (previous_change_index) {
      Some (change) => &change.1,
      None => &self.initial,
    }
  }
}

pub fn tracking_query <'a, Owned: StewardData, Query: PossiblyBorrowedStewardData <'a, Owned>, VaryingData: StewardData, Steward: TimeSteward, Accessor: EventAccessor <Steward = Steward>> (accessor: & Accessor, handle: & DataTimelineCell <SimpleTimeline <VaryingData, Steward>>, query: Query, offset: QueryOffset)-><SimpleTimeline <VaryingData, Steward> as DataTimelineQueriableWith<'a, Owned, Query>>::QueryResult where SimpleTimeline <VaryingData, Steward>: DataTimelineQueriableWith<'a, Owned, Query> {
  accessor.modify (handle, |timeline| {
    timeline.other_dependent_events.insert (accessor.handle().clone());
  });
  accessor.query (handle, query, offset)
}
pub fn modify_simple_timeline <VaryingData: StewardData + IterateUniquelyOwnedPredictions <Steward>, Steward: TimeSteward, Accessor: EventAccessor <Steward = Steward>> (accessor: & Accessor, handle: & DataTimelineCell <SimpleTimeline <VaryingData, Steward>>, modification: VaryingData) {
  #[cfg (debug_assertions)]
  let confirm1 = accessor.query (handle, GetDataAndSettingEvent, QueryOffset::Before).to_owned();
  #[cfg (debug_assertions)]
  let confirm2 = (modification.clone(), Some(accessor.handle().clone()));
  
  let mut do_modify = true;
  if let Some(accessor) = accessor.future_cleanup() {
    let (current_data, current_event) = accessor.query (handle, GetDataAndSettingEvent, QueryOffset::After).0;
    if let Some (current_event) = current_event {
      if current_event.extended_time() == accessor.extended_now() && *current_data == modification {
        do_modify = false;
      }
    }
    if do_modify {
      accessor.peek_mut(handle).remove_present_and_future (accessor);
    }
  }
  if do_modify {
    accessor.modify (handle, move |timeline| {
      timeline.modify(modification, accessor);
    });
  }
  
  #[cfg (debug_assertions)]
  debug_assert! (accessor.query (handle, GetDataAndSettingEvent, QueryOffset::Before).to_owned() == confirm1);
  #[cfg (debug_assertions)]
  debug_assert! (accessor.query (handle, GetDataAndSettingEvent, QueryOffset::After).to_owned() == confirm2);
}
pub fn unmodify_simple_timeline <VaryingData: StewardData + IterateUniquelyOwnedPredictions <Steward>, Steward: TimeSteward, Accessor: FutureCleanupAccessor <Steward = Steward>> (accessor: & Accessor, handle: & DataTimelineCell <SimpleTimeline <VaryingData, Steward>>) {
  #[cfg (debug_assertions)]
  let confirm = accessor.query (handle, GetDataAndSettingEvent, QueryOffset::Before).to_owned();
  
  if let Some(event) = (accessor.query (handle, GetDataAndSettingEvent, QueryOffset::After).0).1 {
    if event.extended_time() == accessor.extended_now() {
      accessor.peek_mut(handle).remove_present_and_future (accessor);
    }
  }
  
  #[cfg (debug_assertions)]
  debug_assert! (accessor.query (handle, GetDataAndSettingEvent, QueryOffset::Before).to_owned() == confirm);
  #[cfg (debug_assertions)]
  debug_assert! (accessor.query (handle, GetDataAndSettingEvent, QueryOffset::After).to_owned() == confirm);
}

} //mod

  };
}


