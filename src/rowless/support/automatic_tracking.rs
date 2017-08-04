use ::DeterministicRandomId;

use std::hash::Hash;
use serde::Serialize;
use serde::de::DeserializeOwned;
use std::io::{Read, Write};
use std::any::Any;
use std::fmt::{self, Debug};
use std::collections::BTreeSet;
use std::mem;
use std::cell::RefCell;

use super::super::api::*;

#[derive (Copy, Clone, PartialEq, Eq, Hash, Serialize, Deserialize, Debug)]
struct GetValue;
impl StewardData for GetValue{}

struct SimpleTimeline <Data: StewardData, Steward: TimeSteward> {
  changes: Vec<(DynamicEventHandle, Option <Data>)>,
  other_dependent_events: RefCell<BTreeSet<DynamicEventHandle>>,
  snapshots_data: (),
}

impl <Data: StewardData, Steward: TimeSteward> SimpleTimeline <Data, Steward> {
  fn invalidate_after <Accessor: InvalidationAccessor> (&self, time: ExtendedTime <Steward::Basics>, accessor: & Accessor) {
    let mut dependencies = self.other_dependent_events.borrow_mut();
    let removed = split_off_greater_set (dependencies, time);
    for event in removed {
      event.invalidate_with (accessor);
    }
    for change in self.changes.iter().rev() {
      let event = &change.0;
      if event.time() <= time {
        break
      }
      accessor.invalidate_dynamic(&event);
    }
  }
  
  fn remove_from (&mut self, time: ExtendedTime <Steward::Basics>) {
    while let Some (change) = self.changes.pop() {
      if change.0.time() <= time {
        self.changes.push (change);
        break
      }
    }
  }
}

impl <Data: StewardData, Steward: TimeSteward> DataTimeline for SimpleTimeline <Data, Steward> {
  type Steward = Steward;
  
  fn serialize <Context: SerializationContext> (&self, serializer: &mut Context, snapshot: & <Self::Steward as TimeSteward>::Snapshot) {
    unimplemented!()
  }
  fn deserialize <Context: DeserializationContext> (deserializer: &mut Context, time: &ExtendedTime <<Self::Steward as TimeSteward>::Basics>)->Self {
    unimplemented!()
  }
  
  fn forget_before (&mut self, time: &ExtendedTime <<Self::Steward as TimeSteward>::Basics>) {
    let mut dependencies = self.other_dependent_events.borrow_mut();
    let retained = dependencies.split_off (time);
    mem::replace (dependencies, retained);
  }
  fn forget_snapshot (&mut self, snapshot: & <Self::Steward as TimeSteward>::Snapshot) {
    unimplemented!()//snapshots_data.remove (snapshot);
  }
}
impl <Data: StewardData, Steward: TimeSteward> DataTimelineQueriableWith<GetValue> for SimpleTimeline <Data, Steward> {
  type QueryResult = Option <(ExtendedTime <Steward::Basics>, Option <Data>)>;

  fn query (&self, query: &GetValue, time: &ExtendedTime <<Self::Steward as TimeSteward>::Basics>, offset: QueryOffset)->Self::QueryResult {
    let previous_change_index = match self.changes.binary_search_by_key (time, | change | &change.0) {
      Ok(index) => match offset {QueryOffset::After => index, QueryOffset::Before => index.wrapping_sub (1)},
      Err (index) => index.wrapping_sub (1),
    };
    self.changes.get (previous_change_index).map (| (event, data) | (event.time(), data))
  }
  
  fn snapshot_query (&self, query: &GetValue, snapshot: & <Self::Steward as TimeSteward>::Snapshot)->Self::QueryResult {
    unimplemented!()
    //self.query (query, snapshot)
  }
}


fn query_simple_timeline <Data: StewardData, Steward: TimeSteward, Accessor: EventAccessor <Steward = Steward>> (accessor: & Accessor, handle: & DataTimelineHandle <SimpleTimeline <Data, Steward>>, offset: QueryOffset)->Option <(ExtendedTime <Steward::Basics>, Option <Data>)> {
  accessor.modify (handle, &move |timeline| {
    let mut dependencies = timeline.other_dependent_events.borrow_mut();
    match dependencies.binary_search_by_key (accessor.now(), | event | event.time()) {
      Ok(index) => match offset {QueryOffset::After => index, Before => index.wrapping_sub (1)},
      Err (index) => index.wrapping_sub (1),
    }
  });
  accessor.query (handle, GetValue, offset)
}
fn modify_simple_timeline <Data: StewardData, Steward: TimeSteward, Accessor: EventAccessor <Steward = Steward>> (accessor: & Accessor, handle: & DataTimelineHandle <SimpleTimeline <Data, Steward>>, modification: Option <Data>) {
  if accessor.query (handle, GetValue, QueryOffset::After) != (accessor.now(), modification) {
    accessor.invalidate (| invalidator | {
      invalidator.peek(handle).invalidate_after (accessor.now(), invalidator);
    });
    accessor.modify (handle, &move |timeline| {
      timeline.remove_from (accessor.now());
      timeline.changes.push ((accessor.handle(), modification));
    });
  }
}
fn unmodify_simple_timeline <Data: StewardData, Steward: TimeSteward, Accessor: EventAccessor <Steward = Steward>> (accessor: & Accessor, handle: & DataTimelineHandle <SimpleTimeline <Data, Steward>>) {
  if accessor.query (handle, GetValue, QueryOffset::After).0 == accessor.now() {
    accessor.invalidate (| invalidator | {
      invalidator.peek(handle).invalidate_after (accessor.now(), invalidator);
    });
    accessor.modify (handle, &move |timeline| {
      timeline.remove_from (accessor.now());
    });
  }
}




