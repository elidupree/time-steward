//! Serialization for TimeSteward data.
//!
//! TimeSteward has a few special requirements for serialization:
//! * DataTimelineHandle objects get special consideration, to support DAGs and cyclic data structures.
//! * The serialization needs to not block other operations for more than O(1) time at a time.
//! * The serialization must be lossless and platform-independent. For this reason, we always use bincode in low-endian mode.

use std::mem;
use std::ptr;

macro_rules! implement_serialization_for_handle {
  () => {

thread_local! {
  static DESERIALIZATION_CONTEXT: RefCell<Option <DeserializationContext>> = RefCell::new (None);
}


impl <T: DataTimeline> Serialize for DataTimelineHandle <T> {
  fn serialize <S: Serializer> (&self, serializer: S)->Result <S::Ok, S::Error> {
    // TODO minor security thing: transform the value using runtime-fixed salt to avoid leaking pointers
    (self.inner as usize).serialize (serializer)
  }
}

impl <'a, T: DataTimeline> Deserialize <'a> for DataTimelineHandle <T> {
  fn deserialize <D: Deserializer> (deserializer: D)->Result <Self, D::Error> {
    let old_pointer = usize::deserialize (deserializer)?;
    DESERIALIZATION_CONTEXT.with (| cell | {
      let mut guard = cell.borrow_mut();
      let mut map = &mut guard.unwrap().map;
      Ok(map.entry (old_pointer).or_insert_with (| | {
        DataTimelineHandle {
          inner: Arc::new (unsafe {mem::uninitialized()}),
        }
      }).clone())
    })
  }
}

impl DeserializationContext {
  fn deserialize_(deserializer: D)->Result <(), D::Error> {
    let old_pointer = usize::deserialize (deserializer)?;
    DESERIALIZATION_CONTEXT.with (| cell | {
      let mut guard = cell.borrow_mut();
      let mut map = &mut guard.unwrap().map;
      let new_pointer = map.entry (old_pointer).or_insert_with (| | {
        DataTimelineHandle {
          inner: Arc::new (unsafe {mem::uninitialized()}),
        }
      }).inner;
      unsafe {ptr::write (new_pointer as *mut DataTimelineHandle, DataTimeline::deserialize (D))?;}
    })?;
    Ok(())
  }
}


  };
}
