#[doc (hidden)]
#[macro_export]
macro_rules! time_steward_define_simple_timeline {
  () => {
pub mod simple_timeline {
use std::collections::BTreeSet;
use std::mem;
use std::cell::RefCell;
use std::marker::PhantomData;

use super::super::super::api::*;
use super::*;
use implementation_support::common::{split_off_greater_set};

#[derive (Copy, Clone, PartialEq, Eq, Hash, Serialize, Deserialize, Debug)]
pub struct GetVarying;
impl StewardData for GetVarying {}



#[serde(bound = "")]
#[derive (Serialize, Deserialize, Derivative)]
#[derivative (Clone (bound=""), Debug (bound=""))]
pub struct SimpleTimeline <VaryingData: StewardData, Steward: TimeSteward> {
  // Hacky workaround for https://github.com/rust-lang/rust/issues/41617 (see https://github.com/serde-rs/serde/issues/943)
  #[serde(deserialize_with = "::serde::Deserialize::deserialize")]
  changes: Vec<(<Steward as TimeSteward>::EventHandle, Option <VaryingData>)>,
  #[serde(deserialize_with = "::serde::Deserialize::deserialize")]
  other_dependent_events: BTreeSet<<Steward as TimeSteward>::EventHandle>,
}

impl <VaryingData: StewardData, Steward: TimeSteward> SimpleTimeline <VaryingData, Steward> {
  pub fn new ()->Self {
    SimpleTimeline {
      changes: Vec::new(),
      other_dependent_events: BTreeSet::new(),
    }
  }
  
  fn remove_from <Accessor: FutureCleanupAccessor<Steward = Steward>> (&mut self, time: &ExtendedTime <Steward::Basics>, accessor: &Accessor) {
    let removed = split_off_greater_set (&mut self.other_dependent_events, time);
    for event in removed {
      accessor.invalidate_execution(&event);
    }
    while let Some (change) = self.changes.pop() {
      if change.0.extended_time() < time {
        self.changes.push (change);
        break
      }
      if change.0.extended_time() > time {
        accessor.invalidate_execution(&change.0);
      }
    }
  }
  
  fn search_changes (&self, time: &ExtendedTime <Steward::Basics>) -> Result <usize, usize> {
    // search at the end first, because we are usually in the present.
    match self.changes.last() {
      None => return Err(0),
      Some (&(ref event, ref data)) => {
        match event.extended_time().cmp(time) {
          Ordering::Greater => (),
          Ordering::Equal => return Ok(self.changes.len()-1),
          Ordering::Less => return Err(self.changes.len()),
        }
      },
    }
    self.changes[..self.changes.len()-1].binary_search_by_key (&time, | change | change.0.extended_time())
  }
}

impl <VaryingData: StewardData, Steward: TimeSteward> DataTimeline for SimpleTimeline <VaryingData, Steward> {
  type Basics = Steward::Basics;
  
  fn clone_for_snapshot (&self, time: &ExtendedTime <Self::Basics>)->Self {
    let slice = match self.search_changes(&time) {
      Ok(index) => &self.changes [index.saturating_sub (1).. index+1],
      Err (index) => &self.changes [index.saturating_sub (1)..index],
    };
    SimpleTimeline {
      changes: slice.to_vec(),
      other_dependent_events: BTreeSet::new(),
    }
  }
  
  fn forget_before (&mut self, time: &ExtendedTime <Self::Basics>) {
    let retained = self.other_dependent_events.split_off (time);
    mem::replace (&mut self.other_dependent_events, retained);
    
    if self.changes.len() > 0 && self.changes [self.changes.len()/2].0.extended_time() < time {
      let keep_from_index = match self.search_changes(&time) {
        Ok (index) => index,
        Err(index) => index - 1,
      };
      self.changes = self.changes.split_off (keep_from_index);
    }
  }
}
impl <VaryingData: StewardData, Steward: TimeSteward> DataTimelineQueriableWith<GetVarying> for SimpleTimeline <VaryingData, Steward> {
  type QueryResult = Option <(ExtendedTime <Self::Basics>, VaryingData)>;

  fn query (&self, _: &GetVarying, time: &ExtendedTime <Self::Basics>, offset: QueryOffset)->Self::QueryResult {
    let previous_change_index = match self.search_changes(&time) {
      Ok(index) => match offset {QueryOffset::After => index, QueryOffset::Before => index.wrapping_sub (1)},
      Err (index) => index.wrapping_sub (1),
    };
    self.changes.get (previous_change_index).and_then (| change | change.1.as_ref().map (| data | (change.0.extended_time().clone(), data.clone())))
  }
}

pub fn tracking_query <VaryingData: StewardData, Steward: TimeSteward, Accessor: EventAccessor <Steward = Steward>> (accessor: & Accessor, handle: & DataTimelineCell <SimpleTimeline <VaryingData, Steward>>, offset: QueryOffset)->Option <(ExtendedTime <Steward::Basics>, VaryingData)> {
  accessor.modify (handle, |timeline| {
    timeline.other_dependent_events.insert (accessor.handle().clone());
  });
  accessor.query (handle, &GetVarying, offset)
}
pub fn modify_simple_timeline <VaryingData: StewardData, Steward: TimeSteward, Accessor: EventAccessor <Steward = Steward>> (accessor: & Accessor, handle: & DataTimelineCell <SimpleTimeline <VaryingData, Steward>>, modification: Option <VaryingData>) {
  #[cfg (debug_assertions)]
  let confirm1 = accessor.query (handle, &GetVarying, QueryOffset::Before);
  #[cfg (debug_assertions)]
  let confirm2 = modification.clone().map(|data|(accessor.extended_now().clone(), data));
  
  if let Some(accessor) = accessor.future_cleanup() {
    match accessor.query (handle, &GetVarying, QueryOffset::After) {
      Some((time, data)) =>
        if let Some (new_data) = modification.as_ref() {
          if &time == accessor.extended_now() && &data == new_data {
            return
          }
        },
      None =>
        if modification.is_none() {return}
    };
    accessor.peek_mut(handle).remove_from (accessor.extended_now(), accessor);
  }
  accessor.modify (handle, move |timeline| {
    timeline.changes.push ((accessor.handle().clone(), modification));
  });
  
  #[cfg (debug_assertions)]
  debug_assert! (accessor.query (handle, &GetVarying, QueryOffset::Before) == confirm1);
  #[cfg (debug_assertions)]
  debug_assert! (accessor.query (handle, &GetVarying, QueryOffset::After) == confirm2);
}
pub fn unmodify_simple_timeline <VaryingData: StewardData, Steward: TimeSteward, Accessor: FutureCleanupAccessor <Steward = Steward>> (accessor: & Accessor, handle: & DataTimelineCell <SimpleTimeline <VaryingData, Steward>>) {
  #[cfg (debug_assertions)]
  let confirm = accessor.query (handle, &GetVarying, QueryOffset::Before);
  
  if let Some((time, _)) = accessor.query (handle, &GetVarying, QueryOffset::After) { if &time == accessor.extended_now() {
    accessor.peek_mut(handle).remove_from (accessor.extended_now(), accessor);
  }}
  
  #[cfg (debug_assertions)]
  debug_assert! (accessor.query (handle, &GetVarying, QueryOffset::Before) == confirm);
  #[cfg (debug_assertions)]
  debug_assert! (accessor.query (handle, &GetVarying, QueryOffset::After) == confirm);
}

} //mod

  };
}


