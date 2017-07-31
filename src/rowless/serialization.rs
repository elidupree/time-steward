//! Serialization for TimeSteward data.
//!
//! TimeSteward has a few special requirements for serialization:
//! * DataTimelineHandle objects get special consideration, to support DAGs and cyclic data structures.
//! * The serialization needs to not block other operations for more than O(1) time at a time.
//! * The serialization must be lossless and platform-independent. For this reason, we always use bincode in low-endian mode.





struct SerializationContext <DataTimelineHandle: ???> {
  timeline_index: HashMap<DataTimelineHandle, TimelineInfo>,
}

impl SerializationContext {
  fn new ()->Self {
  
  }
  fn <T: TimeStewardSerialize> serialize (&mut self, data: & T) {
    
  }
}