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



#[derive (Clone, Serialize, Deserialize, Debug)]
pub struct SimpleTimeline <VaryingData: StewardData, B: Basics> {
  // Hacky workaround for https://github.com/rust-lang/rust/issues/41617 (see https://github.com/serde-rs/serde/issues/943)
  #[serde(deserialize_with = "::serde::Deserialize::deserialize")]
  changes: Vec<(EventHandle <B>, Option <VaryingData>)>,
  #[serde(deserialize_with = "::serde::Deserialize::deserialize")]
  other_dependent_events: RefCell<BTreeSet<EventHandle<B>>>,
}

impl <VaryingData: StewardData, B: Basics> SimpleTimeline <VaryingData, B> {
  pub fn new ()->Self {
    SimpleTimeline {
      changes: Vec::new(),
      other_dependent_events: RefCell::new (BTreeSet::new()),
    }
  }
  
  fn invalidate_after <Steward: TimeSteward <Basics = B, EventHandle = EventHandle<B>>, Accessor: InvalidationAccessor<Steward = Steward>> (&self, time: &ExtendedTime <B>, accessor: & Accessor) {
    let mut dependencies = self.other_dependent_events.borrow_mut();
    let removed = split_off_greater_set (&mut *dependencies, time);
    for event in removed {
      accessor.invalidate(&event);
    }
    for change in self.changes.iter().rev() {
      let event = &change.0;
      if event.extended_time() <= time {
        break
      }
      accessor.invalidate(&event);
    }
  }
  
  fn remove_from (&mut self, time: &ExtendedTime <B>) {
    while let Some (change) = self.changes.pop() {
      if change.0.extended_time() < time {
        self.changes.push (change);
        break
      }
    }
  }
  
  fn search_changes (&self, time: &ExtendedTime <B>) -> Result <usize, usize> {
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

impl <VaryingData: StewardData, B: Basics> DataTimeline for SimpleTimeline <VaryingData, B> {
  type Basics = B;
  
  fn clone_for_snapshot (&self, time: &ExtendedTime <Self::Basics>)->Self {
    let slice = match self.search_changes(&time) {
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
    
    if self.changes.len() > 0 && self.changes [self.changes.len()/2].0.extended_time() < time {
      let keep_from_index = match self.search_changes(&time) {
        Ok (index) => index,
        Err(index) => index - 1,
      };
      self.changes = self.changes.split_off (keep_from_index);
    }
  }
}
impl <VaryingData: StewardData, B: Basics> DataTimelineQueriableWith<GetVarying> for SimpleTimeline <VaryingData, B> {
  type QueryResult = Option <(ExtendedTime <B>, VaryingData)>;

  fn query (&self, _: &GetVarying, time: &ExtendedTime <Self::Basics>, offset: QueryOffset)->Self::QueryResult {
    let previous_change_index = match self.search_changes(&time) {
      Ok(index) => match offset {QueryOffset::After => index, QueryOffset::Before => index.wrapping_sub (1)},
      Err (index) => index.wrapping_sub (1),
    };
    self.changes.get (previous_change_index).and_then (| change | change.1.as_ref().map (| data | (change.0.extended_time().clone(), data.clone())))
  }
}


pub fn tracking_query <B: Basics, VaryingData: StewardData, Steward: TimeSteward <Basics = B, EventHandle = EventHandle<B>>, Accessor: EventAccessor <Steward = Steward>> (accessor: & Accessor, handle: & DataTimelineCell <SimpleTimeline <VaryingData, Steward::Basics>>, offset: QueryOffset)->Option <(ExtendedTime <Steward::Basics>, VaryingData)> {
  accessor.modify (handle, move |timeline| {
    let mut dependencies = timeline.other_dependent_events.borrow_mut();
    dependencies.insert (accessor.handle().clone());
  });
  accessor.query (handle, &GetVarying, offset)
}
pub fn modify_simple_timeline <B: Basics, VaryingData: StewardData, Steward: TimeSteward <Basics = B, EventHandle = EventHandle<B>>, Accessor: EventAccessor <Steward = Steward>> (accessor: & Accessor, handle: & DataTimelineCell <SimpleTimeline <VaryingData, Steward::Basics>>, modification: Option <VaryingData>) {
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
  accessor.invalidate (| invalidator | {
    invalidator.peek(handle).invalidate_after (accessor.extended_now(), invalidator);
  });
  accessor.modify (handle, move |timeline| {
    timeline.remove_from (accessor.extended_now());
    timeline.changes.push ((accessor.handle().clone(), modification));
  });
}
pub fn unmodify_simple_timeline <B: Basics, VaryingData: StewardData, Steward: TimeSteward <Basics = B, EventHandle = EventHandle<B>>, Accessor: EventAccessor <Steward = Steward>> (accessor: & Accessor, handle: & DataTimelineCell <SimpleTimeline <VaryingData, Steward::Basics>>) {
  if let Some((time, _)) = accessor.query (handle, &GetVarying, QueryOffset::After) { if &time == accessor.extended_now() {
    accessor.invalidate (| invalidator | {
      invalidator.peek(handle).invalidate_after (accessor.extended_now(), invalidator);
    });
    accessor.modify (handle, move |timeline| {
      timeline.remove_from (accessor.extended_now());
    });
  }}
}

} //mod

  };
}


