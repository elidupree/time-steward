//! Serialization for TimeSteward data.
//!
//! TimeSteward has a few special requirements for serialization:
//! * DataHandle objects get special consideration, to support DAGs and cyclic data structures.
//! * The serialization needs to not block other operations for more than O(1) time at a time.
//! * It must be possible for the serialization to be lossless and platform-independent – although we also support other serializers, such as the lossy serde_json, because it's useful to be able to look at a human-readable version of the data.
/*!
Technical details:

To prevent large blocking operations, we return a future that does a little bit of the work each time you poll it.

To deal with the cyclic nature of the mutable data of DataHandles, we first use Default to create placeholders for all of the objects, then deserialize the mutable data into place using interior mutability.

We can't do the same thing for the immutable data of DataHandles (because their types don't have interior mutability), but fortunately, they also cannot be cyclic (for the same reason you can't have cycles in Rc without interior mutability). Thus, they form a DAG. In order to deserialize them without unsafe code, they need to be serialized in topological order. This can be done using a depth-first search. Since the DAG may have arbitrary depth, we can't use the function call stack for this; we make our own stack on the heap.

2019-11-06 note: DataHandle probably wants to have its generic parameters refactored; either <ImmutableData, MutableData> or just one <Contents>. MutableData/Contents needs to implement a trait that takes care of substituting in the deserialized version. TimeSteward privacy can be dealt with separately - at worst, just by making Contents have private fields. I also got sidetracked worrying about whether DataHandle should always store a deterministic unique id; I can put off making this decision if I simply preserve the possibility of adding a id() method to the Contents trait later.

*/


use crate::{SimulationStateData};
use crate::implementation_support::common::{DataHandle, PrivateTimeStewardDataTrait};


struct Annotated

struct SerializeHandleContents<'a, T>(T);
impl<
    PublicImmutableData: SimulationStateData,
    PrivateTimeStewardData: PrivateTimeStewardDataTrait,
  > Serialize for SerializeHandleContents(DataHandle<PublicImmutableData, PrivateTimeStewardData>)
{
  fn serialize<S: Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
    let mut serializer = serializer.serialize_struct_variant("TimeStewardSerializationItem", 0, "HandleContents", );
    serializer.serialize_field (
  }
}

impl<
    PublicImmutableData: SimulationStateData,
    PrivateTimeStewardData: PrivateTimeStewardDataTrait,
  > Serialize for DataHandle<PublicImmutableData, PrivateTimeStewardData>
{
  fn serialize<S: Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
    maybe_apply_visitor!(self, serializer);
    with_serialization_context (| context | {
      let next_serial_number = &mut context.next_serial_number;
      let serial_number = match context.serial_numbers_by_handle.get(self) {
        Some (number) => *number,
        None => {
          let serial_number = context.next_serial_number;
          context.next_serial_number += 1;
          context.handles_to_serialize.insert (self.clone());
          serial_number
        }
      };
      serial_number.serialize (serializer)
    })
  }
}

impl<
    'a,
    PublicImmutableData: SimulationStateData,
    PrivateTimeStewardData: PrivateTimeStewardDataTrait,
  > Deserialize<'a> for DataHandle<PublicImmutableData, PrivateTimeStewardData>
{
  fn deserialize<D: Deserializer<'a>>(deserializer: D) -> Result<Self, D::Error> {
    let serial_number = u64::deserialize(deserializer);
    with_deserialization_context (| context | {
      context.handles_by_serial_number.get(&serial_number).cloned().ok_or_else (|| D::Error::custom("deserializing a DataHandle to data that hasn't already been deserialized"))
    })
  }
}


impl<
    'a,
    PublicImmutableData: SimulationStateData,
    PrivateTimeStewardData: PrivateTimeStewardDataTrait,
  > DataHandle<PublicImmutableData, PrivateTimeStewardData>
{
  fn deserialize_immutable_only<D: Deserializer<'a>>(serial_number: u64, deserializer: D) -> Result<Self, D::Error> {
    let immutable = PublicImmutableData::deserialize(deserializer);
    let handle = Self::new_nonreplicable (immutable, PrivateTimeStewardData::default());
    // actually, probably put this in the caller:
    with_deserialization_context (| context | {
      context.handles_by_serial_number.insert (serial_number, handle.clone());
    })
    Ok (handle)
  }
}





pub struct SnapshotSerializer <'a, A: SnapshotAccessor, S: Serializer> {
  snapshot: &A,
  serializer: S::SerializeSeq,
  
}

impl<'a, A: SnapshotAccessor, S: Serializer> Future for SnapshotSerializer<'a, A, S> {
  fn new(snapshot: &A, serializer: S)->Poll <S::Ok, S::Error> {
    
    SnapshotSerializer {
      serializer.serialize_seq()?
    }
  }
}

impl<'a, A: SnapshotAccessor, S: Serializer> Future for SnapshotSerializer<'a, A, S> {
  type Item = S::Ok,
  type Error = S::Error,
  fn poll (&mut self)->Poll <Self::Item, Self::Error> {
    if let Some(handle) = self.handle_stack.pop() {
      handle.finish_finding (self)?;
      Ok(Async::NotReady)
    }
    else if let Some(handle) = self.events_iter().next() {
      self.find_handle(handle)?;
      Ok(Async::NotReady)
    }
    else if let Some(entity_handle) = self.entity_handle_queue.pop() {
      entity_handle.serialize_mutable_data (self)?;
      Ok(Async::NotReady)
    }
    else {
      let result = self.serializer.end()?;
      Ok(Async::Ready(result))
    }
  }
}

trait DataHandleSerializeExt<'a, A: SnapshotAccessor, S: Serializer> {
  fn find(&self, serializer: &mut SnapshotSerializer <'a, A, S>)->Result<(), S::Error>;
  fn finish_finding(&self, serializer: &mut SnapshotSerializer <'a, A, S>)->Result<(), S::Error>;
  fn maybe_serialize_mutable_data(&self, serializer: &mut SnapshotSerializer <'a, A, S>)->Result<(), S::Error>;
}

impl<'a, A: SnapshotAccessor, S: Serializer, PublicImmutableData: SimulationStateData,
    PrivateTimeStewardData: PrivateTimeStewardDataTrait> DataHandleSerializeExt<'a, A, S> for DataHandle<PublicImmutableData, PrivateTimeStewardData> {
  fn find(&self, serializer: &mut SnapshotSerializer <'a, A, S>)->Result<(), S::Error> {
    if let Some(info) = serializer.handles.get(self) {
      if info.finding_in_process {
        return Err (S::Error::Custom ("Found cycle in DataHandles' immutable data (this should not be possible unless the simulation uses forbidden interior mutability)"))
      }
    }
    else {
      serializer.handles.insert (Box::new (self.clone()), HandleInfo {
        finding_in_process: true,
      });
      
      struct Visitor<'a>(&'a mut SnapshotSerializer);
      impl<'a> Visit<DataHandle> for Visitor<'a> {
        fn visit(&mut self, value: &DataHandle) {
          serializer.handle_stack.push (Box::new (target.clone()));
        }
      }
      visit_all(Visitor(serializer));
    }
    Ok (())
  }
  fn finish_finding(&self, serializer: &mut SnapshotSerializer <'a, A, S>)->Result<(), S::Error> {
    let mut info = serializer.handles.get_mut(&self).unwrap();
    info.finding_in_process = false;
    serializer.serializer.serialize_element (&self.snapshot.query (entity_handle))?;
  }
  fn maybe_serialize_mutable_data(&self, serializer: &mut SnapshotSerializer <'a, A, S>)->Result<(), S::Error> {
    
  }
}

