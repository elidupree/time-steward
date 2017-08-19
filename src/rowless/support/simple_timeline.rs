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
pub struct GetConstant;
impl StewardData for GetConstant {}

#[derive (Copy, Clone, PartialEq, Eq, Hash, Serialize, Deserialize, Debug)]
pub struct GetVarying;
impl StewardData for GetVarying {}



#[derive (Clone, Serialize, Deserialize, Debug)]
pub struct ConstantTimeline <Data: StewardData, B: Basics> {
  // Hacky workaround for https://github.com/rust-lang/rust/issues/41617 (see https://github.com/serde-rs/serde/issues/943)
  #[serde(deserialize_with = "::serde::Deserialize::deserialize")]
  data: Data,
  marker: PhantomData <B>,
}

impl <Data: StewardData, B: Basics> ConstantTimeline <Data, B> {
  pub fn new (data: Data)->Self {
    ConstantTimeline {
      data: data,
      marker: PhantomData,
    }
  }
}

impl <Data: StewardData, B: Basics> DataTimeline for ConstantTimeline <Data, B> {
  type Basics = B;
  
  fn clone_for_snapshot (&self, _: &ExtendedTime <Self::Basics>)->Self {
    self.clone()
  }
  
  fn forget_before (&mut self, _: &ExtendedTime <Self::Basics>) {}
}
impl <Data: StewardData, B: Basics> DataTimelineQueriableWith<GetConstant> for ConstantTimeline <Data, B> {
  type QueryResult = Data;

  fn query (&self, _: & GetConstant, _: &ExtendedTime <Self::Basics>, _: QueryOffset)->Self::QueryResult {
    self.data.clone()
  }
}



#[derive (Clone, Serialize, Deserialize, Debug)]
pub struct SimpleTimeline <ConstantData: StewardData, VaryingData: StewardData, B: Basics> {
  // Hacky workaround for https://github.com/rust-lang/rust/issues/41617 (see https://github.com/serde-rs/serde/issues/943)
  #[serde(deserialize_with = "::serde::Deserialize::deserialize")]
  constant: ConstantData,
  #[serde(deserialize_with = "::serde::Deserialize::deserialize")]
  changes: Vec<(DynamicEventHandle <B>, Option <VaryingData>)>,
  #[serde(deserialize_with = "::serde::Deserialize::deserialize")]
  other_dependent_events: RefCell<BTreeSet<DynamicEventHandle<B>>>,
}

impl <ConstantData: StewardData, VaryingData: StewardData, B: Basics> SimpleTimeline <ConstantData, VaryingData, B> {
  pub fn new (constant: ConstantData)->Self {
    SimpleTimeline {
      constant: constant,
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
      if event.extended_time() <= time {
        break
      }
      accessor.invalidate_dynamic(&event);
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
}

impl <ConstantData: StewardData, VaryingData: StewardData, B: Basics> DataTimeline for SimpleTimeline <ConstantData, VaryingData, B> {
  type Basics = B;
  
  fn clone_for_snapshot (&self, time: &ExtendedTime <Self::Basics>)->Self {
    let slice = match self.changes.binary_search_by_key (&time, | change | change.0.extended_time()) {
      Ok(index) => &self.changes [index.saturating_sub (1).. index+1],
      Err (index) => &self.changes [index.saturating_sub (1)..index],
    };
    SimpleTimeline {
      constant: self.constant.clone(),
      changes: slice.to_vec(),
      other_dependent_events: RefCell::new (BTreeSet::new()),
    }
  }
  
  fn forget_before (&mut self, time: &ExtendedTime <Self::Basics>) {
    let mut dependencies = self.other_dependent_events.borrow_mut();
    let retained = dependencies.split_off (time);
    mem::replace (&mut*dependencies, retained);
    
    if self.changes.len() > 0 && self.changes [self.changes.len()/2].0.extended_time() < time {
      let keep_from_index = match self.changes.binary_search_by_key (&time, | change | change.0.extended_time()) {
        Ok (index) => index,
        Err(index) => index - 1,
      };
      self.changes = self.changes.split_off (keep_from_index);
    }
  }
}
impl <ConstantData: StewardData, VaryingData: StewardData, B: Basics> DataTimelineQueriableWith<GetConstant> for SimpleTimeline <ConstantData, VaryingData, B> {
  type QueryResult = ConstantData;

  fn query (&self, _: & GetConstant, _: &ExtendedTime <Self::Basics>, _: QueryOffset)->Self::QueryResult {
    self.constant.clone()
  }
}
impl <ConstantData: StewardData, VaryingData: StewardData, B: Basics> DataTimelineQueriableWith<GetVarying> for SimpleTimeline <ConstantData, VaryingData, B> {
  type QueryResult = Option <(ExtendedTime <B>, VaryingData)>;

  fn query (&self, _: &GetVarying, time: &ExtendedTime <Self::Basics>, offset: QueryOffset)->Self::QueryResult {
    let previous_change_index = match self.changes.binary_search_by_key (&time, | change | change.0.extended_time()) {
      Ok(index) => match offset {QueryOffset::After => index, QueryOffset::Before => index.wrapping_sub (1)},
      Err (index) => index.wrapping_sub (1),
    };
    self.changes.get (previous_change_index).and_then (| change | change.1.as_ref().map (| data | (change.0.extended_time().clone(), data.clone())))
  }
}


pub fn tracking_query <ConstantData: StewardData, VaryingData: StewardData, Steward: TimeSteward, Accessor: EventAccessor <Steward = Steward>> (accessor: & Accessor, handle: & DataTimelineHandle <SimpleTimeline <ConstantData, VaryingData, Steward::Basics>>, offset: QueryOffset)->Option <(ExtendedTime <Steward::Basics>, VaryingData)> {
  accessor.modify (handle, move |timeline| {
    let mut dependencies = timeline.other_dependent_events.borrow_mut();
    dependencies.insert (accessor.handle().clone());
  });
  accessor.query (handle, &GetVarying, offset)
}
pub fn modify_simple_timeline <ConstantData: StewardData, VaryingData: StewardData, Steward: TimeSteward, Accessor: EventAccessor <Steward = Steward>> (accessor: & Accessor, handle: & DataTimelineHandle <SimpleTimeline <ConstantData, VaryingData, Steward::Basics>>, modification: Option <VaryingData>) {
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
pub fn unmodify_simple_timeline <ConstantData: StewardData, VaryingData: StewardData, Steward: TimeSteward, Accessor: EventAccessor <Steward = Steward>> (accessor: & Accessor, handle: & DataTimelineHandle <SimpleTimeline <ConstantData, VaryingData, Steward::Basics>>) {
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


