

#[doc (hidden)]
#[macro_export]
macro_rules! time_steward_define_simple_timeline {
  () => {
pub mod automatic_tracking {
use std::collections::BTreeSet;
use std::mem;
use std::cell::RefCell;

use super::super::super::api::*;
use super::*;
use implementation_support::common::{split_off_greater_set};

#[derive (Copy, Clone, PartialEq, Eq, Hash, Serialize, Deserialize, Debug)]
pub struct GetValue;
impl StewardData for GetValue{}

#[derive (Clone, Serialize, Deserialize, Debug)]
pub struct SimpleTimeline <Data: StewardData, B: Basics> {
  // Hacky workaround for https://github.com/rust-lang/rust/issues/41617 (see https://github.com/serde-rs/serde/issues/943)
  #[serde(deserialize_with = "::serde::Deserialize::deserialize")]
  changes: Vec<(DynamicEventHandle <B>, Option <Data>)>,
  #[serde(deserialize_with = "::serde::Deserialize::deserialize")]
  other_dependent_events: RefCell<BTreeSet<DynamicEventHandle<B>>>,
}

impl <Data: StewardData, B: Basics> SimpleTimeline <Data, B> {
  pub fn new ()->Self {
    SimpleTimeline {
      changes: Vec::new(),
      other_dependent_events: RefCell::new (BTreeSet::new()),
    }
  }
  
  fn invalidate_after <Steward: TimeSteward <Basics = B>, Accessor: InvalidationAccessor<Steward = Steward>> (&self, time: &ExtendedTime <B>, accessor: & Accessor) {
    let mut dependencies = self.other_dependent_events.borrow_mut();
    let removed = split_off_greater_set (&mut *dependencies, time);
    for event in removed {
      accessor.invalidate_dynamic(&event);
    }
    for change in self.changes.iter().rev() {
      let event = &change.0;
      if event.time() <= time {
        break
      }
      accessor.invalidate_dynamic(&event);
    }
  }
  
  fn remove_from (&mut self, time: &ExtendedTime <B>) {
    while let Some (change) = self.changes.pop() {
      if change.0.time() <= time {
        self.changes.push (change);
        break
      }
    }
  }
}

impl <Data: StewardData, B: Basics> DataTimeline for SimpleTimeline <Data, B> {
  type Basics = B;
  
  fn clone_for_snapshot (&self, time: &ExtendedTime <Self::Basics>)->Self {
    let slice = match self.changes.binary_search_by_key (&time, | change | change.0.time()) {
      Ok(index) => &self.changes [index.saturating_sub (1).. index+1],
      Err (index) => &self.changes [index.saturating_sub (1)..index],
    };
    SimpleTimeline {
      changes: slice.to_vec(),
      other_dependent_events: RefCell::new (BTreeSet::new()),
    }
  }
  
  fn forget_before (&mut self, time: &ExtendedTime <Self::Basics>) {
    let mut dependencies = self.other_dependent_events.borrow_mut();
    let retained = dependencies.split_off (time);
    mem::replace (&mut*dependencies, retained);
  }
}
impl <Data: StewardData, B: Basics> DataTimelineQueriableWith<GetValue> for SimpleTimeline <Data, B> {
  type QueryResult = Option <(ExtendedTime <B>, Option <Data>)>;

  fn query (&self, _: &GetValue, time: &ExtendedTime <Self::Basics>, offset: QueryOffset)->Self::QueryResult {
    let previous_change_index = match self.changes.binary_search_by_key (&time, | change | change.0.time()) {
      Ok(index) => match offset {QueryOffset::After => index, QueryOffset::Before => index.wrapping_sub (1)},
      Err (index) => index.wrapping_sub (1),
    };
    self.changes.get (previous_change_index).map (| change | (change.0.time().clone(), change.1.clone()))
  }
}


pub fn query_simple_timeline <Data: StewardData, Steward: TimeSteward, Accessor: EventAccessor <Steward = Steward>> (accessor: & Accessor, handle: & DataTimelineHandle <SimpleTimeline <Data, Steward::Basics>>, offset: QueryOffset)->Option <(ExtendedTime <Steward::Basics>, Option <Data>)> {
  accessor.modify (handle, move |timeline| {
    let mut dependencies = timeline.other_dependent_events.borrow_mut();
    dependencies.insert (accessor.handle().clone());
  });
  accessor.query (handle, &GetValue, offset)
}
pub fn modify_simple_timeline <Data: StewardData, Steward: TimeSteward, Accessor: EventAccessor <Steward = Steward>> (accessor: & Accessor, handle: & DataTimelineHandle <SimpleTimeline <Data, Steward::Basics>>, modification: Option <Data>) {
  if let Some((time, data)) = accessor.query (handle, &GetValue, QueryOffset::After) {
    if &time == accessor.now() && data == modification {
      return
    }
  }
  accessor.invalidate (| invalidator | {
    invalidator.peek(handle).invalidate_after (accessor.now(), invalidator);
  });
  accessor.modify (handle, move |timeline| {
    timeline.remove_from (accessor.now());
    timeline.changes.push ((accessor.handle().clone(), modification));
  });
}
pub fn unmodify_simple_timeline <Data: StewardData, Steward: TimeSteward, Accessor: EventAccessor <Steward = Steward>> (accessor: & Accessor, handle: & DataTimelineHandle <SimpleTimeline <Data, Steward::Basics>>) {
  if let Some((time, _)) = accessor.query (handle, &GetValue, QueryOffset::After) { if &time == accessor.now() {
    accessor.invalidate (| invalidator | {
      invalidator.peek(handle).invalidate_after (accessor.now(), invalidator);
    });
    accessor.modify (handle, move |timeline| {
      timeline.remove_from (accessor.now());
    });
  }}
}

} //mod

  };
}


