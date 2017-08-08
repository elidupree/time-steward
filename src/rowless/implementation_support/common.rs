use ::DeterministicRandomId;
use std::hash::Hash;
use serde::Serialize;
use serde::de::DeserializeOwned;
use std::io::{Read, Write};
use std::any::Any;
use std::fmt::Debug;
use std::cmp::Ordering;
use std::borrow::Borrow;

#[doc (hidden)]
#[macro_export]
macro_rules! time_steward_common_impls_for_handles {
  () => {

impl <T: Event> Borrow<ExtendedTime <<T::Steward as TimeSteward>::Basics>> for EventHandle <T> {
  fn borrow (&self)->& ExtendedTime <<T::Steward as TimeSteward>::Basics> {self.time()}
}
impl <B: Basics> Borrow<ExtendedTime <B>> for DynamicEventHandle<B> {
  fn borrow (&self)->& ExtendedTime <B> {self.time()}
}

/*impl<T: Event> Clone for EventHandle <T> {
  fn clone(&self) -> Self {
    unimplemented!()
  }
}
impl<B: Basics> Clone for DynamicEventHandle<B> {
  fn clone(&self) -> Self {
    unimplemented!()
  }
}*/
impl<T: Event> Ord for EventHandle <T> {
  fn cmp(&self, other: &Self) -> Ordering {
    self.time().cmp(other.time())
  }
}
impl<B: Basics> Ord for DynamicEventHandle<B> {
  fn cmp(&self, other: &Self) -> Ordering {
    self.time().cmp(other.time())
  }
}
impl<T: Event> Eq for EventHandle <T> {}
impl<B: Basics> Eq for DynamicEventHandle<B> {}
impl<T: Event> PartialEq for EventHandle <T> {
  fn eq(&self, other: &Self) -> bool {
    self.time().eq(other.time())
  }
}
impl<B: Basics> PartialEq for DynamicEventHandle<B> {
  fn eq(&self, other: &Self) -> bool {
    self.time().eq(other.time())
  }
}
impl<T: Event> PartialEq <DynamicEventHandle<<T::Steward as TimeSteward>::Basics>> for EventHandle <T> {
  fn eq(&self, other: &DynamicEventHandle<<T::Steward as TimeSteward>::Basics>) -> bool {
    self.time().eq(other.time())
  }
}
impl<T: Event> PartialEq <EventHandle <T>> for DynamicEventHandle<<T::Steward as TimeSteward>::Basics> {
  fn eq(&self, other: &EventHandle <T>) -> bool {
    self.time().eq(other.time())
  }
}

impl<T: Event> PartialOrd<DynamicEventHandle<<T::Steward as TimeSteward>::Basics>> for EventHandle <T> {
  fn partial_cmp(&self, other: &DynamicEventHandle<<T::Steward as TimeSteward>::Basics>) -> Option<Ordering> {
    Some(self.time().cmp(other.time()))
  }
}
impl<T: Event> PartialOrd<EventHandle <T>> for DynamicEventHandle<<T::Steward as TimeSteward>::Basics> {
  fn partial_cmp(&self, other: &EventHandle <T>) -> Option<Ordering> {
    Some(self.time().cmp(other.time()))
  }
}
impl<T: Event> PartialOrd for EventHandle <T> {
  fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
    Some(self.cmp(other))
  }
}
impl<B: Basics> PartialOrd for DynamicEventHandle<B> {
  fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
    Some(self.cmp(other))
  }
}

  };
}

/*pub trait DeserializationContext {
  fn deserialize_data <T: DeserializeOwned> (&mut self)->T;
  fn deserialize_timeline_handle <T: DataTimeline> (&mut self)->DataTimelineHandle <T>;
  fn deserialize_prediction_handle <T: Event> (&mut self)->PredictionHandle <T>;
  fn deserialize_event_handle <T: Event> (&mut self)->EventHandle <T>;
  fn deserialize_dynamic_event_handle (&mut self)->DynamicEventHandle;
}*/

