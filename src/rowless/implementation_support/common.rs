use super::super::api::*;
use std::cmp::Ordering;
use ::DeterministicRandomId;
use rand::ChaChaRng;

#[doc (hidden)]
#[macro_export]
macro_rules! time_steward_common_impls_for_event_handle {
  ([$($bounds:tt)*] [$($concrete:tt)*] [$($basics:tt)*]) => {
  

impl <$($bounds)*> Borrow<ExtendedTime <$($basics)*>> for $($concrete)* {
  fn borrow (&self)->& ExtendedTime <$($basics)*> {self.time()}
}

impl<$($bounds)*> Ord for $($concrete)* {
  fn cmp(&self, other: &Self) -> Ordering {
    self.time().cmp(other.time())
  }
}
impl<$($bounds)*> Eq for $($concrete)* {}
impl<$($bounds)*> PartialEq for $($concrete)* {
  fn eq(&self, other: &Self) -> bool {
    self.time().eq(other.time())
  }
}
impl<$($bounds)*> PartialOrd for $($concrete)* {
  fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
    Some(self.cmp(other))
  }
}

  
  };
}

#[doc (hidden)]
#[macro_export]
macro_rules! time_steward_crossover_impls_for_event_handles {
  ([$($bounds:tt)*] [$($concrete_1:tt)*] [$($concrete_2:tt)*]) => {
    time_steward_crossover_impls_for_event_handles!(directional [$($bounds)*] [$($concrete_1)*] [$($concrete_2)*]);
    time_steward_crossover_impls_for_event_handles!(directional [$($bounds)*] [$($concrete_2)*] [$($concrete_1)*]);
  };
  (directional [$($bounds:tt)*] [$($concrete_1:tt)*] [$($concrete_2:tt)*]) => {
  

impl<T: Event> PartialEq <$($concrete_1)*> for $($concrete_2)* {
  fn eq(&self, other: &$($concrete_1)*) -> bool {
    self.time().eq(other.time())
  }
}

impl<T: Event> PartialOrd<$($concrete_1)*> for $($concrete_2)* {
  fn partial_cmp(&self, other: &$($concrete_1)*) -> Option<Ordering> {
    Some(self.time().cmp(other.time()))
  }
}

  
  };
}

#[doc (hidden)]
#[macro_export]
macro_rules! time_steward_common_impls_for_handles {
  () => {

time_steward_common_impls_for_event_handle! ([T: Event] [EventHandle <T>] [<T::Steward as TimeSteward>::Basics]);
time_steward_common_impls_for_event_handle! ([B: Basics] [DynamicEventHandle <B>] [B]);
time_steward_common_impls_for_event_handle! ([T: Event] [PredictionHandle <T>] [<T::Steward as TimeSteward>::Basics]);

time_steward_crossover_impls_for_event_handles! ([T: Event] [EventHandle <T>] [DynamicEventHandle<<T::Steward as TimeSteward>::Basics>]);
time_steward_crossover_impls_for_event_handles! ([T: Event] [PredictionHandle <T>] [DynamicEventHandle<<T::Steward as TimeSteward>::Basics>]);
time_steward_crossover_impls_for_event_handles! ([T: Event] [EventHandle <T>] [PredictionHandle <T>]);

  };
}

/*pub trait DeserializationContext {
  fn deserialize_data <T: DeserializeOwned> (&mut self)->T;
  fn deserialize_timeline_handle <T: DataTimeline> (&mut self)->DataTimelineHandle <T>;
  fn deserialize_prediction_handle <T: Event> (&mut self)->PredictionHandle <T>;
  fn deserialize_event_handle <T: Event> (&mut self)->EventHandle <T>;
  fn deserialize_dynamic_event_handle (&mut self)->DynamicEventHandle;
}*/

fn generator_for_event(id: DeterministicRandomId) -> ChaChaRng {
  ChaChaRng::from_seed(&[(id.data()[0] >> 32) as u32,
                        (id.data()[0] & 0xffffffff) as u32,
                        (id.data()[1] >> 32) as u32,
                        (id.data()[1] & 0xffffffff) as u32])
}

pub struct GenericEventAccessor<B: Basics> {
  pub generator: ChaChaRng,
}
impl<B: Basics> GenericEventAccessor<B> {
  pub fn new(now: &ExtendedTime<B>) -> Self {
    let generator = generator_for_event(now.id);
    GenericEventAccessor {
      generator: generator,
    }
  }
}

pub fn extended_time_of_fiat_event<B: Basics>(time: B::Time,
                                                  id: DeterministicRandomId)
                                                  -> ExtendedTime<B> {
  ExtendedTime {
    base: time,
    iteration: 0,
    id: id.for_fiat_event_internal(),
  }
}

pub fn extended_time_of_predicted_event<B: Basics>
  (id: DeterministicRandomId,
   event_base_time: B::Time,
   from: &ExtendedTime<B>)
   -> Option<ExtendedTime<B>> {
  let iteration = match event_base_time.cmp(&from.base) {
    Ordering::Less => return None, // short-circuit
    Ordering::Greater => 0,
    Ordering::Equal => {
      if id > from.id {
        from.iteration
      } else {
        if from.iteration >= B::MAX_ITERATION {
          panic!("Too many iterations at the same base time; probably an infinite loop")
        }
        from.iteration + 1
      }
    }
  };
  Some(ExtendedTime {
    base: event_base_time,
    iteration: iteration,
    id: id,
  })
}
