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
pub struct GetVarying;
impl PersistentlyIdentifiedType for GetVarying {
  const ID: PersistentTypeId = PersistentTypeId(0x8912fbb263c46434);
}

#[derive (Copy, Clone, PartialEq, Eq, Hash, Serialize, Deserialize, Debug)]
pub struct JustDestroyed;
impl PersistentlyIdentifiedType for JustDestroyed {
  const ID: PersistentTypeId = PersistentTypeId(0x4ee4ba51f56f884d);
}

pub trait IterateUniquelyOwnedPredictions <Steward: TimeSteward> {
  fn iterate_predictions <F: FnMut (& Steward::EventHandle)> (&self, _callback: &mut F) {}
}


#[serde(bound = "")]
#[derive (Serialize, Deserialize, Derivative)]
#[derivative (Clone (bound=""), Debug (bound=""))]
pub struct SimpleTimeline <VaryingData: QueryResult, Steward: TimeSteward> {
  // Hacky workaround for https://github.com/rust-lang/rust/issues/41617 (see https://github.com/serde-rs/serde/issues/943)
  #[serde(deserialize_with = "::serde::Deserialize::deserialize")]
  changes: VecDeque<(<Steward as TimeSteward>::EventHandle, VaryingData)>,
  #[serde(deserialize_with = "::serde::Deserialize::deserialize")]
  destroyer: Option<<Steward as TimeSteward>::EventHandle>,
  #[serde(deserialize_with = "::serde::Deserialize::deserialize")]
  other_dependent_events: BTreeSet<<Steward as TimeSteward>::EventHandle>,
}

impl <VaryingData: QueryResult, Steward: TimeSteward> SimpleTimeline <VaryingData, Steward> {
  pub fn new ()->Self {
    SimpleTimeline {
      changes: VecDeque::new(),
      destroyer: None,
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
impl <VaryingData: QueryResult + IterateUniquelyOwnedPredictions <Steward>, Steward: TimeSteward> SimpleTimeline <VaryingData, Steward> {
  fn remove_future <Accessor: FutureCleanupAccessor<Steward = Steward>> (&mut self, accessor: &Accessor, also_present: bool) {
    if self.destroyer.as_ref().map_or(false, |event| {
        let ordering = event.extended_time().cmp(accessor.extended_now());
        ordering == Ordering::Greater || (!also_present && ordering == Ordering::Equal)
      }) {
      self.destroyer = None;
    }
    let removed = split_off_greater_set (&mut self.other_dependent_events, accessor.extended_now());
    for event in removed {
      accessor.invalidate_execution(&event);
    }
    while let Some (change) = self.changes.pop_back() {
      let ordering = change.0.extended_time().cmp(accessor.extended_now());
      if ordering == Ordering::Less || (!also_present && ordering == Ordering::Equal) {
        IterateUniquelyOwnedPredictions::<Steward>::iterate_predictions (&change.1, &mut | prediction | {
          if also_present || accessor.get_prediction_destroyer (prediction).map_or (true, | destroyer | &destroyer > accessor.handle()) {
            accessor.change_prediction_destroyer (prediction, None);
          }
        });
        self.changes.push_back (change);
        break
      }
      // if we are actually discarding the event, we need to clean up some stuff about it
      IterateUniquelyOwnedPredictions::<Steward>::iterate_predictions (&change.1, &mut | prediction | accessor.change_prediction_destroyer (prediction, Some(& change.0)));
      
      // except don't re-invalidate the event we are currently in
      if ordering == Ordering::Greater {
        accessor.invalidate_execution(&change.0);
      }
    }
  }
  
  fn modify <Accessor: EventAccessor<Steward = Steward>> (&mut self, modification: VaryingData, accessor: &Accessor){
    if self.destroyer.as_ref().map_or(false, |event| event.extended_time() <= accessor.extended_now()) {
      panic!("Tried to modify a SimpleTimeline after it was destroyed")
    }
    let mut pop = false;
    if let Some(last) = self.changes.back() {
      assert!(& last.0 <= accessor.handle(), "All future changes should have been cleared before calling modify() ");
      if &last.0 == accessor.handle() {
        pop = true;
      }
    }
    if pop {self.changes.pop_back();}
    // we don't need to create incoming predictions, because they can't be incoming unless they were already created
    self.changes.push_back ((accessor.handle().clone(), modification));
  }
}

impl <VaryingData: QueryResult, Steward: TimeSteward> DataTimeline for SimpleTimeline <VaryingData, Steward> {
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
      changes: changes,
      destroyer: self.destroyer.clone(),
      other_dependent_events: BTreeSet::new(),
    }
  }
  
  fn forget_before (&mut self, time: &ExtendedTime <Self::Basics>) {
    let retained = self.other_dependent_events.split_off (time);
    mem::replace (&mut self.other_dependent_events, retained);
    
    while self.changes.get(1).map_or (false, | change | change.0.extended_time() < time) {
      self.changes.pop_front ();
    }
  }
}
impl <VaryingData: QueryResult, Steward: TimeSteward> DataTimelineQueriableWith<GetVarying> for SimpleTimeline <VaryingData, Steward> {
  type QueryResult = VaryingData;

  fn query (&self, _: &GetVarying, time: &ExtendedTime <Self::Basics>)->Self::QueryResult {
    if self.destroyer.as_ref().map_or(false, |event| event.extended_time() <= time) {
      panic!("Tried to query a SimpleTimeline after it was destroyed")
    }
    let previous_change_index = match self.search_changes(&time) {
      Ok(index) => index,
      Err (index) => index.wrapping_sub (1),
    };
    self.changes.get (previous_change_index).expect("Tried to query a SimpleTimeline before it was first set").1.clone()
  }
}
impl <VaryingData: QueryResult, Steward: TimeSteward> DataTimelineQueryRefableWith<GetVarying> for SimpleTimeline <VaryingData, Steward> {
  fn query_ref (&self, _: &GetVarying, time: &ExtendedTime <Self::Basics>)->&Self::QueryResult {
    if self.destroyer.as_ref().map_or(false, |event| event.extended_time() <= time) {
      panic!("Tried to query a SimpleTimeline after it was destroyed")
    }
    let previous_change_index = match self.search_changes(&time) {
      Ok(index) => index,
      Err (index) => index.wrapping_sub (1),
    };
    &self.changes.get (previous_change_index).expect("Tried to query a SimpleTimeline before it was first set").1
  }
}
impl <VaryingData: QueryResult, Steward: TimeSteward> DataTimelineQueriableWith<JustDestroyed> for SimpleTimeline <VaryingData, Steward> {
  type QueryResult = bool;

  fn query (&self, _: &JustDestroyed, time: &ExtendedTime <Self::Basics>)->Self::QueryResult {
    match self.destroyer.as_ref().map_or(Ordering::Greater, |event| event.extended_time().cmp(time)) {
      Ordering::Greater => false,
      Ordering::Equal => true,
      Ordering::Less => panic!("Tried to query a SimpleTimeline after it was destroyed"),
    }
  }
}

pub fn query <VaryingData: QueryResult, Steward: TimeSteward, A: Accessor <Steward = Steward>> (accessor: & A, handle: & DataTimelineCell <SimpleTimeline <VaryingData, Steward>>)->VaryingData {
  accessor.query (handle, &GetVarying)
}
pub fn tracking_query <VaryingData: QueryResult, Steward: TimeSteward, Accessor: EventAccessor <Steward = Steward>> (accessor: & Accessor, handle: & DataTimelineCell <SimpleTimeline <VaryingData, Steward>>)->VaryingData {
  accessor.modify (handle, |timeline| {
    timeline.other_dependent_events.insert (accessor.handle().clone());
  });
  query (accessor, handle)
}
pub fn query_ref <'timeline, VaryingData: QueryResult, Steward: TimeSteward, A: Accessor <Steward = Steward>> (accessor: &'timeline A, handle: &'timeline DataTimelineCell <SimpleTimeline <VaryingData, Steward>>)->DataTimelineCellReadGuard<'timeline, VaryingData> {
  accessor.query_ref (handle, &GetVarying)
}
pub fn tracking_query_ref <'timeline, VaryingData: QueryResult, Steward: TimeSteward, Accessor: EventAccessor <Steward = Steward>> (accessor: &'timeline Accessor, handle: &'timeline DataTimelineCell <SimpleTimeline <VaryingData, Steward>>)->DataTimelineCellReadGuard<'timeline, VaryingData> {
  accessor.modify (handle, |timeline| {
    timeline.other_dependent_events.insert (accessor.handle().clone());
  });
  query_ref (accessor, handle)
}
pub fn set <VaryingData: QueryResult + IterateUniquelyOwnedPredictions <Steward>, Steward: TimeSteward, Accessor: EventAccessor <Steward = Steward>> (accessor: & Accessor, handle: & DataTimelineCell <SimpleTimeline <VaryingData, Steward>>, modification: VaryingData) {
  //#[cfg (debug_assertions)]
  //let confirm1 = accessor.query (handle, &GetVarying, QueryOffset::Before);
  #[cfg (debug_assertions)]
  let confirm2 = modification.clone();
  
  let mut do_modify = true;
  if let Some(accessor) = accessor.future_cleanup() {
    // currently, we can't do this culling because
    // distinct predictions at the same time compare equal to each other, so
    // updates that replace an older version of the prediction with a newer version
    // would be incorrectly omitted (in fact, this occurred in bouncy_circles)
    /*match accessor.query (handle, &GetVarying) {
      Some((time, data)) =>
        if let Some (new_data) = modification.as_ref() {
          if &time == accessor.extended_now() && &data == new_data {
            do_modify = false;
          }
        },
      None =>
        if modification.is_none() {do_modify = false;}
    };*/
    if do_modify {
      accessor.peek_mut(handle).remove_future (accessor, false);
    }
  }
  if do_modify {
    accessor.modify (handle, move |timeline| {
      timeline.modify(modification, accessor);
    });
  }
  
  //#[cfg (debug_assertions)]
  //debug_assert! (accessor.query (handle, &GetVarying, QueryOffset::Before) == confirm1);
  #[cfg (debug_assertions)]
  debug_assert! (accessor.query (handle, &GetVarying) == confirm2);
}
pub fn unset <VaryingData: QueryResult + IterateUniquelyOwnedPredictions <Steward>, Steward: TimeSteward, Accessor: FutureCleanupAccessor <Steward = Steward>> (accessor: & Accessor, handle: & DataTimelineCell <SimpleTimeline <VaryingData, Steward>>) {
  //#[cfg (debug_assertions)]
  //let confirm = accessor.query (handle, &GetVarying, QueryOffset::Before);
  
  let mut guard = accessor.peek_mut(handle);
  if guard.search_changes(accessor.extended_now()).is_ok() {
    guard.remove_future (accessor, true);
  }
  
  //#[cfg (debug_assertions)]
  //debug_assert! (accessor.query (handle, &GetVarying, QueryOffset::Before) == confirm);
  //#[cfg (debug_assertions)]
  //debug_assert! (accessor.query (handle, &GetVarying) == confirm);
}

pub fn destroy <VaryingData: QueryResult + IterateUniquelyOwnedPredictions <Steward>, Steward: TimeSteward, Accessor: EventAccessor <Steward = Steward>> (accessor: & Accessor, handle: & DataTimelineCell <SimpleTimeline <VaryingData, Steward>>) {
  if let Some(accessor) = accessor.future_cleanup() {
    accessor.peek_mut(handle).remove_future (accessor, false);
  }
  accessor.modify (handle, move |timeline| {
    timeline.destroyer = Some(accessor.handle().clone());
  });
}

/// You're supposed to destroy a SimpleTimeline when there are no more handles to it in the simulation state. In that situation, there might be some handles left on the stack. As a convenience, we provide this way of querying whether a handle is in that state, so that you don't have to keep track of it yourself.
///
/// Note that this will still panic if you check on a timeline in a later event after it's been destroyed.
pub fn just_destroyed <VaryingData: QueryResult, Steward: TimeSteward, A: Accessor <Steward = Steward>> (accessor: & A, handle: & DataTimelineCell <SimpleTimeline <VaryingData, Steward>>)->bool {
  accessor.query (handle, & JustDestroyed)
}


} //mod

  };
}


