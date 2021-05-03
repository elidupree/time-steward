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

2019-11-06 note: DataHandle probably wants to have its generic parameters refactored; either <ImmutableData, MutableData> or just one <Contents>. MutableData/Contents needs to implement a trait that takes care of substituting in the deserialized version. TimeSteward privacy can be dealt with separately - at worst, just by making Contents have private fields. I also got sidetracked worrying about whether DataHandle should always store a deterministic unique id; I can put off making this decision if I simply preserve the possibility of adding a id() method to the Contents trait later. (Storing an ID has a trivial time cost at runtime, plus 128 bits of memory usage for every DataHandle, BUT in order to usefully share them over the network, you'd also have to have a lookup mechanism – a map from id to DataHandle, probably implemented as a HAMT – which would add an additional cost to the creation of each DataHandle. That's probably fine – the average DataHandle is probably created once and then witnesses many events, so the creation cost is fine. Except for the handles for speculative future events that never actually happen – those handles are just created and then destroyed, and it happens a lot of times. There are some open questions about how to make that more efficient...)



So:

When serializing: When we find a DataHandle, we want to serialize it as a serial number rather than something else. The data itself must have already been serialized, since we deserialize it in the same order. Let's assume that the serial number is simply its position in the ordering of serialized data.

Since we have to serialize things in such an ordering, we can't just start serializing – we need to use the visitor hack. So the first operation is to do an async DFS algorithm using the visitor hack, to make a topological ordering of the DataHandles (on the implicit graph of DataHandles whose *immutable* data contains another DataHandle). Once we have the topological ordering, we can do one pass to serialize all the immutable data, then a second pass to serialize all the mutable data. (We could even serialize the immutable data as we put each element into the ordering. Would that be an optimization? Not sure, and it makes the code messier / less encapsulated.)

This DFS algorithm is slightly more complicated than the typical DFS algorithm for topological sorting, because we don't have a list of all vertices - we have to *find* the vertices by looking through the mutable data that exists at the snapshot time, as well as the immutable data. That's straightforward enough: at each node, when you've finished looking through the immutable data and inserted the node into the ordering, you then also look through all its mutable data.

The second pass can be done in an arbitrary order. Options include:
* The topological order (seems like a reasonable default)
* The arbitrary order of the found_handles map (might let me save a little bit of runtime cost by not keeping a separate Vec of handles, if I also didn't keep it for the first pass; theoretically adds to security weaknesses because it might leak information about pointer values (seems impractical to use for an attack, but why add weaknesses if you don't have to?))
* Some smart order to cache-optimize de/serialization later
** we could interleave the immutable and mutable parts where possible to make the immutable and mutable parts of each handle as close to each other as possible (but this may not actually be an optimization)
** we could group together handles of the same type (might improve caching of instructions)

We currently use Box<dyn DataHandleSerializeExt>, where DataHandleSerializeExt is implemented for
DataHandle rather than the contents of DataHandle; this is a double indirection that feels unnecessary,
so maybe it could be resolved by implementing the trait for the contents with `self: Rc<Self>`?
*/

use serde::{
  de::{Deserialize, Deserializer},
  ser::{Serialize, Serializer},
};

use crate::implementation_support::common::{DataHandle, PrivateTimeStewardDataTrait};
use crate::implementation_support::serialization::SnapshotSorterHandleState::SearchingDependencies;
use crate::type_utils::visit_serialized::{visit_all, Visit};
use crate::{Accessor, SimulationStateData, SnapshotAccessor};
use std::collections::hash_map::Entry;
use std::collections::HashMap;
use std::mem;

enum SnapshotSorterStep<'a, A> {
  FindHandle(Box<dyn DataHandleSerializeExt<'a, A>>),
  HandleDependenciesDone(Box<dyn DataHandleSerializeExt<'a, A>>),
}
#[derive(PartialEq, Eq, Debug)]
enum SnapshotSorterHandleState {
  SearchingDependencies,
  PositionedInOrdering(usize),
}
struct SnapshotSorter<'a, A> {
  accessor: &'a A,
  stack: Vec<SnapshotSorterStep<'a, A>>,
  found_handles: HashMap<*const (), SnapshotSorterHandleState>,
  topological_ordering: Vec<Box<dyn DataHandleSerializeExt<'a, A>>>,
}

impl<'a, A: Accessor> SnapshotSorter<'a, A> {
  fn find_handle<
    PublicImmutableData: SimulationStateData,
    PrivateTimeStewardData: PrivateTimeStewardDataTrait,
  >(
    &mut self,
    handle: &DataHandle<PublicImmutableData, PrivateTimeStewardData>,
  ) {
    match self.found_handles.entry(handle.address()) {
      Entry::Vacant(entry) => {
        entry.insert(SnapshotSorterHandleState::SearchingDependencies);

        struct Visitor<'a, 'b, A>(&'b mut SnapshotSorter<'a, A>);
        impl<'a, 'b, A, PID2: SimulationStateData, PTD2: PrivateTimeStewardDataTrait>
          Visit<DataHandle<PID2, PTD2>> for Visitor<'a, 'b, A>
        {
          fn visit(&mut self, value: &DataHandle<PID2, PTD2>) {
            let new_handle: Box<dyn DataHandleSerializeExt<'a, A>> = Box::new(value.clone());
            self
              .0
              .stack
              .push(SnapshotSorterStep::FindHandle(new_handle));
          }
        }

        // we want to handle the actual dependencies of the public part first,
        // then finalize this handle itself, then visit the soft links in the mutable part.
        // so, we put them on the stack in the reverse of that order:
        visit_all(&mut Visitor(self), &*self.accessor.query(handle));
        self
          .stack
          .push(SnapshotSorterStep::HandleDependenciesDone(Box::new(
            handle.clone(),
          )));
        visit_all(&mut Visitor(self), handle.public());
      }
      Entry::Occupied(entry) => {
        assert_ne!(*entry.into_mut(), SearchingDependencies, "Found cycle in DataHandles' immutable data (this should not be possible unless the simulation uses forbidden interior mutability)");
      }
    }
  }

  fn step(&mut self) -> Option<Vec<Box<dyn DataHandleSerializeExt<'a, A>>>> {
    match self.stack.pop() {
      None => Some(mem::take(&mut self.topological_ordering)),
      Some(step) => {
        match step {
          SnapshotSorterStep::FindHandle(handle) => {
            handle.call_snapshot_sorter_find_handle(self);
          }
          SnapshotSorterStep::HandleDependenciesDone(handle) => {
            let state = self.found_handles.get_mut(&handle.address()).expect(
              "shouldn't have constructed a HandleDependenciesDone for a handle we didn't find yet",
            );
            assert_eq!(*state, SnapshotSorterHandleState::SearchingDependencies, "shouldn't have constructed a HandleDependenciesDone more than once for the same handle");
            *state =
              SnapshotSorterHandleState::PositionedInOrdering(self.topological_ordering.len());
            self.topological_ordering.push(handle);
          }
        }
        None
      }
    }
  }
}

// We would like to make this trait not have any generic parameters,
// and put the parameters on the individual methods, but we need it to be object-safe
trait DataHandleSerializeExt<'a, A> {
  fn address(&self) -> *const ();
  fn call_snapshot_sorter_find_handle(&self, sorter: &mut SnapshotSorter<'a, A>)
  where
    A: Accessor;
}

impl<
    PublicImmutableData: SimulationStateData,
    PrivateTimeStewardData: PrivateTimeStewardDataTrait,
  > DataHandleSerializeExt for DataHandle<PublicImmutableData, PrivateTimeStewardData>
{
  fn address(&self) -> *const () {
    self.address()
  }
  fn call_snapshot_sorter_find_handle(&self, sorter: &mut SnapshotSorter<'a, A>)
  where
    A: Accessor,
  {
    sorter.find_handle(self)
  }
}

/*
//struct Annotated

struct SerializeHandleContents<T>(T);
impl<
    PublicImmutableData: SimulationStateData,
    PrivateTimeStewardData: PrivateTimeStewardDataTrait,
  > Serialize for SerializeHandleContents<DataHandle<PublicImmutableData, PrivateTimeStewardData>>
{
  fn serialize<S: Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
    let mut serializer = serializer.serialize_struct_variant("TimeStewardSerializationItem", 0, "HandleContents", );
    //serializer.serialize_field (
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
  snapshot: &'a A,
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
*/
