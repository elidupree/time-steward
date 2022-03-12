use std::borrow::Borrow;
use std::collections::{BTreeMap, BTreeSet};

pub fn split_off_greater<K: Ord + Borrow<Q> + Clone, V, Q: Ord + ?Sized>(
  input: &mut BTreeMap<K, V>,
  split: &Q,
) -> BTreeMap<K, V> {
  // BTreeMap::split_off() DOES remove this splitting key, while we want to NOT include that key.
  // TODO: will Rust eventually make this easier?
  let mut result = input.split_off(split);
  let mut transfer = None;
  if let Some(whoops) = result.iter().next() {
    if whoops.0.borrow() == split {
      transfer = Some(whoops.0.clone());
    }
  }
  if let Some(key) = transfer {
    input.insert(key, result.remove(split).unwrap());
  }
  result
}

pub fn split_off_greater_set<K: Ord + Borrow<Q>, Q: Ord + ?Sized>(
  input: &mut BTreeSet<K>,
  split: &Q,
) -> BTreeSet<K> {
  // BTreeMap::split_off() DOES remove this splitting key, while we want to NOT include that key.
  // TODO: will Rust eventually make this easier?
  let mut result = input.split_off(split);
  if let Some(whoops) = result.take(split) {
    input.insert(whoops);
  }
  #[cfg(debug_assertions)]
  {
    if result.take(split).is_some() {
      panic!("Some code broke the Ord/Borrow rules for BTreeSet calls")
    }
  }
  result
}

/*
impl<
    PublicImmutableData: SimulationStateData + Default,
    PrivateTimeStewardData: PrivateTimeStewardDataTrait + Default,
  > Default for DataHandle<PublicImmutableData, PrivateTimeStewardData>
{
  fn default() -> Self {
    struct InProgress;
    thread_local! {
      static DEFAULTS: RefCell<HashMap <TypeId, Box <dyn Any>>> = RefCell::new (HashMap::new());
    }
    DEFAULTS.with(|defaults| {
      let id = TypeId::of::<Self>();
      if let Some(existing) = defaults.borrow().get(&id) {
        if existing.is::<InProgress>() {
          panic!(
            "Infinite recursion in Default impl of TimeSteward simulation data type {}",
            std::any::type_name::<Self>()
          )
        } else if let Some(concrete) = existing.downcast_ref::<Self>() {
          concrete.clone()
        } else {
          unreachable!("Wrong type stored in DataHandle defaults map")
        }
      } else {
        defaults.borrow_mut().insert(id, Box::new(InProgress));
        let handle = Self::new_nonreplicable(
          PublicImmutableData::default(),
          PrivateTimeStewardData::default(),
        );
        defaults.borrow_mut().insert(id, Box::new(handle.clone()));
        handle
      }
    })
  }
}
 */

/*pub trait DeserializationContext {
fn deserialize_data <T: DeserializeOwned> (&mut self)->T;
fn deserialize_timeline_handle <T: Entity> (&mut self)->EntityHandle <T>;
fn deserialize_prediction_handle <T: Event> (&mut self)->PredictionHandle <T>;
fn deserialize_event_handle <T: Event> (&mut self)->EventHandle <T>;
fn deserialize_dynamic_event_handle (&mut self)->DynamicEventHandle;
}*/
