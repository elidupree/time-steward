//! Serialization for TimeSteward data.
//!
//! TimeSteward has a few special requirements for serialization:
//! * DataTimelineHandle objects get special consideration, to support DAGs and cyclic data structures.
//! * The serialization needs to not block other operations for more than O(1) time at a time.
//! * The serialization must be lossless and platform-independent. For this reason, we always use bincode in low-endian mode.

#[doc(hidden)]
#[macro_export]
macro_rules! time_steward_serialization_impls {
  () => {

  #[allow (unused_variables)]
  pub trait TimeStewardStructuresVisitor <Steward: TimeSteward> {
    fn visit_data_handle <T: SimulationStateData + PersistentlyIdentifiedType> (&mut self, handle: & DataHandle <T>) {}
    fn visit_event_handle (&mut self, handle: & Steward::EventHandle) {}
    fn visit_data_timeline_cell <T: DataTimeline> (&mut self, cell: & DataTimelineCell <T>) {}
  }

  pub trait TimeStewardStructuresVisitable <Steward: TimeSteward> {
    fn visit_all <T: TimeStewardStructuresVisitor <Steward>>(&self, visitor: T);
  }

  impl <Steward: TimeSteward, Data: SimulationStateData> TimeStewardStructuresVisitable<Steward> for Data {
    fn visit_all <T: TimeStewardStructuresVisitor <Steward>>(&self, visitor: T) {
      self.serialize (&mut TimeStewardStructuresVisitingSerializeHack::<T,Steward>(visitor, PhantomData)).unwrap();
    }
  }

  use serde::ser;
  use std::fmt::{self,Display};
  use std::marker::PhantomData;
  struct TimeStewardStructuresVisitingSerializeHack<T, Steward>(T, PhantomData<Steward>);

  trait MaybeVisitSerializeHack <Steward: TimeSteward> {
    fn visit_event_handle (&mut self, handle: & Steward::EventHandle)->bool;
    fn visit_data_timeline_cell <T: DataTimeline> (&mut self, cell: & DataTimelineCell <T>)->bool;
  }
  impl <Steward: TimeSteward, MaybeVisitor> MaybeVisitSerializeHack <Steward> for MaybeVisitor {
    default fn visit_event_handle (&mut self, _handle: & Steward::EventHandle)->bool {false}
    default fn visit_data_timeline_cell <T: DataTimeline> (&mut self, _cell: & DataTimelineCell <T>)->bool {false}
  }
  impl <'a, Steward: TimeSteward, Visitor: TimeStewardStructuresVisitor <Steward>> MaybeVisitSerializeHack <Steward> for &'a mut TimeStewardStructuresVisitingSerializeHack<Visitor, Steward> {
    fn visit_event_handle (&mut self, handle: & Steward::EventHandle)->bool {
      TimeStewardStructuresVisitor::<Steward>::visit_event_handle (&mut self.0, handle);
      true
    }
    fn visit_data_timeline_cell <T: DataTimeline> (&mut self, cell: & DataTimelineCell <T>)->bool {
      TimeStewardStructuresVisitor::<Steward>::visit_data_timeline_cell (&mut self.0, cell);
      true
    }
  }

  trait MaybeVisitSerializeHackUntyped {
    fn visit_data_handle <T: SimulationStateData + PersistentlyIdentifiedType> (&mut self, handle: & DataHandle <T>)->bool;
  }
  impl <MaybeVisitor> MaybeVisitSerializeHackUntyped for MaybeVisitor {
    default fn visit_data_handle <T: SimulationStateData + PersistentlyIdentifiedType> (&mut self, _handle: & DataHandle <T>)->bool {false}
  }
  impl <'a, Steward: TimeSteward, Visitor: TimeStewardStructuresVisitor <Steward>> MaybeVisitSerializeHackUntyped for &'a mut TimeStewardStructuresVisitingSerializeHack<Visitor, Steward> {
    fn visit_data_handle <T: SimulationStateData + PersistentlyIdentifiedType> (&mut self, handle: & DataHandle <T>)->bool {
      TimeStewardStructuresVisitor::<Steward>::visit_data_handle (&mut self.0, handle);
      true
    }
  }



  #[allow (unreachable_code, unreachable_patterns)]
  #[derive (Debug)]
  struct NeverError(!);
  impl ser::Error for NeverError {
    fn custom<T: Display>(_msg: T) -> Self {
        panic!()
    }
}
impl ::std::error::Error for NeverError {
    fn description(&self) -> &str {
        unreachable!()
    }
}
impl Display for NeverError {
    fn fmt(&self, _formatter: &mut fmt::Formatter) -> fmt::Result {
        unreachable!()
    }
}
  impl<'a, Steward: TimeSteward, Visitor: TimeStewardStructuresVisitor <Steward>> ser::Serializer for &'a mut TimeStewardStructuresVisitingSerializeHack<Visitor, Steward> {
    type Ok = ();
    type Error = NeverError;

    type SerializeSeq = Self;
    type SerializeTuple = Self;
    type SerializeTupleStruct = Self;
    type SerializeTupleVariant = Self;
    type SerializeMap = Self;
    type SerializeStruct = Self;
    type SerializeStructVariant = Self;

    fn serialize_bool(self, _: bool) -> Result<(),NeverError> { Ok(()) }
    fn serialize_i8(self, _: i8) -> Result<(),NeverError> { Ok(()) }
    fn serialize_i16(self, _: i16) -> Result<(),NeverError> { Ok(()) }
    fn serialize_i32(self, _: i32) -> Result<(),NeverError> { Ok(()) }
    fn serialize_i64(self, _: i64) -> Result<(),NeverError> { Ok(()) }
    fn serialize_u8(self, _: u8) -> Result<(),NeverError> { Ok(()) }
    fn serialize_u16(self, _: u16) -> Result<(),NeverError> { Ok(()) }
    fn serialize_u32(self, _: u32) -> Result<(),NeverError> { Ok(()) }
    fn serialize_u64(self, _: u64) -> Result<(),NeverError> { Ok(()) }
    fn serialize_f32(self, _: f32) -> Result<(),NeverError> { Ok(()) }
    fn serialize_f64(self, _: f64) -> Result<(),NeverError> { Ok(()) }
    fn serialize_char(self, _: char) -> Result<(),NeverError> { Ok(()) }
    fn serialize_str(self, _: &str) -> Result<(),NeverError> { Ok(()) }
    fn serialize_bytes(self, _: &[u8]) -> Result<(),NeverError> { Ok(()) }
    fn serialize_none(self) -> Result<(),NeverError> { Ok(()) }
    fn serialize_some<T>(self, value: &T) -> Result<(),NeverError>
        where T: ?Sized + Serialize { value.serialize(self) }
    fn serialize_unit(self) -> Result<(),NeverError> { Ok(()) }
    fn serialize_unit_struct(self, _name: &'static str) -> Result<(),NeverError> { Ok(()) }
    fn serialize_unit_variant(
        self,
        _name: &'static str,
        __variant_index: u32,
        _variant: &'static str
    ) -> Result<(),NeverError> { Ok(()) }
    fn serialize_newtype_struct<T>(self, _name: &'static str, value: &T) -> Result<(),NeverError>
        where T: ?Sized + Serialize{ value.serialize(self) }
    fn serialize_newtype_variant<T>(
        self,
        _name: &'static str,
        _variant_index: u32,
        _variant: &'static str,
        value: &T
    )  -> Result<(),NeverError>
        where T: ?Sized + Serialize { value.serialize(self) }

    fn serialize_seq(self, _len: Option<usize>) -> Result<Self::SerializeSeq,NeverError> { Ok(self) }
    fn serialize_tuple(self, _len: usize) -> Result<Self::SerializeTuple,NeverError> { Ok(self) }
    fn serialize_tuple_struct(
        self,
        _name: &'static str,
        _len: usize
    ) -> Result<Self::SerializeTupleStruct,NeverError> { Ok(self) }
    fn serialize_tuple_variant(
        self,
        _name: &'static str,
        _variant_index: u32,
        _variant: &'static str,
        _len: usize
    ) -> Result<Self::SerializeTupleVariant,NeverError> { Ok(self) }
    fn serialize_map(self, _len: Option<usize>) -> Result<Self::SerializeMap,NeverError> { Ok(self) }
    fn serialize_struct(
        self,
        _name: &'static str,
        _len: usize
    ) -> Result<Self::SerializeStruct,NeverError> { Ok(self) }
    fn serialize_struct_variant(
        self,
        _name: &'static str,
        _variant_index: u32,
        _variant: &'static str,
        _len: usize
    ) -> Result<Self::SerializeStructVariant,NeverError> { Ok(self) }
}
impl<'a, Steward: TimeSteward, Visitor: TimeStewardStructuresVisitor <Steward>> ser::SerializeSeq for &'a mut TimeStewardStructuresVisitingSerializeHack<Visitor, Steward>{
    type Ok = ();
    type Error = NeverError;
    fn serialize_element<T>(&mut self, value: &T) -> Result<(),NeverError>
        where T: ?Sized + Serialize { value.serialize(&mut**self) }
    fn end(self) -> Result<(),NeverError> { Ok(()) }
}
impl<'a, Steward: TimeSteward, Visitor: TimeStewardStructuresVisitor <Steward>> ser::SerializeTuple for &'a mut TimeStewardStructuresVisitingSerializeHack<Visitor, Steward>{
    type Ok = ();
    type Error = NeverError;
    fn serialize_element<T>(&mut self, value: &T) -> Result<(),NeverError>
        where T: ?Sized + Serialize { value.serialize(&mut**self) }
    fn end(self) -> Result<(),NeverError> { Ok(()) }
}
impl<'a, Steward: TimeSteward, Visitor: TimeStewardStructuresVisitor <Steward>> ser::SerializeTupleStruct for &'a mut TimeStewardStructuresVisitingSerializeHack<Visitor, Steward>{
    type Ok = ();
    type Error = NeverError;
    fn serialize_field<T>(&mut self, value: &T) -> Result<(),NeverError>
        where T: ?Sized + Serialize { value.serialize(&mut**self) }
    fn end(self) -> Result<(),NeverError> { Ok(()) }
}
impl<'a, Steward: TimeSteward, Visitor: TimeStewardStructuresVisitor <Steward>> ser::SerializeTupleVariant for &'a mut TimeStewardStructuresVisitingSerializeHack<Visitor, Steward>{
    type Ok = ();
    type Error = NeverError;
    fn serialize_field<T>(&mut self, value: &T) -> Result<(),NeverError>
        where T: ?Sized + Serialize { value.serialize(&mut**self) }
    fn end(self) -> Result<(),NeverError> { Ok(()) }
}
impl<'a, Steward: TimeSteward, Visitor: TimeStewardStructuresVisitor <Steward>> ser::SerializeMap for &'a mut TimeStewardStructuresVisitingSerializeHack<Visitor, Steward>{
    type Ok = ();
    type Error = NeverError;
    fn serialize_key<T>(&mut self, key: &T) -> Result<(),NeverError>
        where T: ?Sized + Serialize { key.serialize(&mut**self)  }
    fn serialize_value<T>(&mut self, value: &T) -> Result<(),NeverError>
        where T: ?Sized + Serialize { value.serialize(&mut**self) }
    fn end(self) -> Result<(),NeverError> { Ok(()) }
}
impl<'a, Steward: TimeSteward, Visitor: TimeStewardStructuresVisitor <Steward>> ser::SerializeStruct for &'a mut TimeStewardStructuresVisitingSerializeHack<Visitor, Steward>{
    type Ok = ();
    type Error = NeverError;
    fn serialize_field<T>(&mut self, _key: &'static str, value: &T) -> Result<(),NeverError>
        where T: ?Sized + Serialize { value.serialize(&mut**self) }
    fn end(self) -> Result<(),NeverError> { Ok(()) }
}
impl<'a, Steward: TimeSteward, Visitor: TimeStewardStructuresVisitor <Steward>> ser::SerializeStructVariant for &'a mut TimeStewardStructuresVisitingSerializeHack<Visitor, Steward>{
    type Ok = ();
    type Error = NeverError;
    fn serialize_field<T>(&mut self, _key: &'static str, value: &T) -> Result<(),NeverError>
        where T: ?Sized + Serialize { value.serialize(&mut**self) }
    fn end(self) -> Result<(),NeverError> { Ok(()) }
}


  trait StaticDowncast <T> {
    fn static_downcast (self)->T;
  }
  impl <T> StaticDowncast <T> for T {
    fn static_downcast (self)->T {self}
  }
  impl <T, U> StaticDowncast <T> for U {
    default fn static_downcast (self)->T {panic!("Tried to do TimeSteward serialization with non-bincode serializer/deserializer. TimeSteward serialization only supports bincode")}
  }
  fn static_downcast <T, U> (input: T)->U {
    StaticDowncast::<U>::static_downcast (input)
  }

  fn bincode_error_to_generic <T, U> (result: $crate::bincode::Result <T>)->Result <T, U> {
    result.map_err (static_downcast::<$crate::bincode::Error, U>)
  }
  fn generic_error_to_bincode <T, U> (result: Result <T, U>)->$crate::bincode::Result <T> {
    result.map_err (static_downcast::<U, $crate::bincode::Error>)
  }

  use $crate::serde::{Serialize};
  use $crate::serde::ser::Error;

  #[derive (Serialize, Deserialize, Debug)]
  enum SerializationElement {
    DataHandleData(u64, PersistentTypeId),
    EventHandleData(u64, PersistentTypeId),
    Finished,
  }

  trait SerializeTargetInto {
    fn serialize_target_into(&self, writer: &mut Write, object_id: u64)->$crate::bincode::Result <()>;
  }
  trait SerializeInto {
    fn serialize_into(&self, writer: &mut Write)->$crate::bincode::Result <()>;
  }
  impl<T: SimulationStateData + PersistentlyIdentifiedType> SerializeTargetInto for DataHandle <T> {
    fn serialize_target_into(&self, writer: &mut Write, object_id: u64)->$crate::bincode::Result <()> {
      $crate::bincode::serialize_into (writer, & SerializationElement::DataHandleData (object_id, T::ID), $crate::bincode::Infinite)?;
      $crate::bincode::serialize_into (writer, &*self.data, $crate::bincode::Infinite)
    }
  }
  fn data_handle_initialize_function <T: SimulationStateData + PersistentlyIdentifiedType>(reader: &mut Read, object_id: u64)->$crate::bincode::Result <()> {
    with_deserialization_context (| context | {
      context.uninitialized_handles.remove(&object_id);
      let handle = context.find_handle::<_, DataHandle <T>> (object_id, || {
        Box::new (DataHandle{data:Rc::<T>::new(unsafe {::std::mem::uninitialized()})})
      })?;
      unsafe {::std::ptr::write (
        &*handle.data as *const T as *mut T,
        $crate::bincode::deserialize_from (reader, $crate::bincode::Infinite)?
      );}
      Ok(())
    })
  }

  impl<B: Basics> SerializeTargetInto for EventHandle <B> {
    fn serialize_target_into(&self, writer: &mut Write, object_id: u64)->$crate::bincode::Result <()> {
      $crate::bincode::serialize_into (writer, & SerializationElement::EventHandleData (object_id, self.data.data.persistent_type_id()), $crate::bincode::Infinite)?;
      $crate::bincode::serialize_into (writer, self.extended_time(), $crate::bincode::Infinite)?;
      self.data.data.serialize_into (writer)
    }
  }
  impl<T: Serialize> SerializeInto for T {
    fn serialize_into(&self, writer: &mut Write)->$crate::bincode::Result <()> {
      $crate::bincode::serialize_into (writer, self, $crate::bincode::Infinite)
    }
  }
  fn event_handle_initialize_function <B: Basics, T: Event <Steward = Steward <B> >>(reader: &mut Read, object_id: u64)->$crate::bincode::Result <()> {
    with_deserialization_context (| context | {
      context.uninitialized_handles.remove(&object_id);
      let now = context.time.downcast_ref::<ExtendedTime <B>>().unwrap().clone();
      let handle = context.find_handle::<_, <T::Steward as TimeSteward>::EventHandle> (object_id, || {
        let handle_box = Box::<<T::Steward as TimeSteward>::EventHandle>::new (EventHandle { data: Rc::new(unsafe {::std::mem::uninitialized()})});
        handle_box.data.links.set(0);
        handle_box as Box<Any>
      })?.clone();
      let time: ExtendedTime <B> = ::bincode::deserialize_from (reader, $crate::bincode::Infinite)?;
      let in_future = time > now;
      if in_future {context.predictions.insert (object_id);}
      let data: T = ::bincode::deserialize_from (reader, $crate::bincode::Infinite)?;
      unsafe {::std::ptr::write (
        &*handle.data as *const EventInner<B> as *mut EventInner<B>,
        deserialization_create_event_inner(time.clone(), data, in_future, handle.data.links.clone())
      );}
      Ok(())
    })
  }

  struct SerializationContext {
    snapshot: Box <Any>,
    handle_targets_observed: HashMap <usize, u64>,
    handles_to_serialize_target: Vec<(u64, Box <SerializeTargetInto>)>,
    next_object_identifier: u64,
  }
  struct DeserializationContext {
    time: Box <Any>,
    data_handle_initialize_functions: ::std::collections::HashMap <
      PersistentTypeId, fn (&mut Read, u64)->$crate::bincode::Result <()>>,
    event_handle_initialize_functions: ::std::collections::HashMap <
      PersistentTypeId, fn (&mut Read, u64)->$crate::bincode::Result <()>>,
    handles: ::std::collections::HashMap <u64, Box <Any>>,
    uninitialized_handles: ::std::collections::HashSet <u64>,
    predictions: ::std::collections::HashSet <u64>,
    success: bool,
  }

  impl Drop for DeserializationContext {
    fn drop (&mut self) {
      // Hack: if there was a deserialization error,
      // the handles may be in a broken intermediate state;
      // leak memory instead of segfaulting
      // (there's probably a proper way to do free the memory, but
      //   this whole system will probably be rewritten anyway)
      if !self.success {
        mem::forget (mem::replace (&mut self.handles, HashMap::new()));
      }
    }
  }

  thread_local! {
    static SERIALIZATION_CONTEXT: RefCell<Option <SerializationContext>> = RefCell::new (None);
    static DESERIALIZATION_CONTEXT: RefCell<Option <DeserializationContext>> = RefCell::new (None);
  }

  fn with_serialization_context <R, F: FnOnce (&mut SerializationContext)->Result <R, $crate::bincode::Error>> (callback: F)->Result <R, $crate::bincode::Error> {
    SERIALIZATION_CONTEXT.with (| cell | {
      let mut guard = cell.borrow_mut();
      let context = match guard.as_mut() {Some(x)=>x, None=>return Err($crate::bincode::Error::custom("Tried to serialize TimeSteward data without a serialization context."))};
      callback (context)
    })
  }
  fn with_deserialization_context <R, F: FnOnce (&mut DeserializationContext)->Result <R, $crate::bincode::Error>> (callback: F)->Result <R, $crate::bincode::Error> {
    DESERIALIZATION_CONTEXT.with (| cell | {
      let mut guard = cell.borrow_mut();
      let context = match guard.as_mut() {Some(x)=>x, None=>return Err($crate::bincode::Error::custom("Tried to deserialize TimeSteward data without a deserialization context."))};
      callback (context)
    })
  }

  impl SerializationContext {
    fn find_handle <F: FnOnce()->Box <SerializeTargetInto>, T> (&mut self, pointer: usize, create_serializable: F)->Result<u64, $crate::bincode::Error> {
      let handle_targets_observed = &mut self.handle_targets_observed;
      let handles_to_serialize_target = &mut self.handles_to_serialize_target;
      let next_object_identifier = &mut self.next_object_identifier;
      let object_identifier = *handle_targets_observed.entry (pointer).or_insert_with (|| {
        let result = *next_object_identifier;
        *next_object_identifier += 1;
        handles_to_serialize_target.push((result, create_serializable()));
        result
      });
      Ok(object_identifier)
    }
  }

  impl DeserializationContext {
    fn find_handle <F: FnOnce()->Box <Any>, T: Any> (&mut self, object_identifier: u64, create_uninitialized: F)->Result<&mut T, $crate::bincode::Error> {
      let handles = &mut self.handles;
      let uninitialized_handles = &mut self.uninitialized_handles;
      let entry = handles.entry (object_identifier).or_insert_with (|| {
        let created = create_uninitialized();
        uninitialized_handles.insert (object_identifier);
        created
      });
      match (*entry).downcast_mut::<T>() {
        Some (result) => Ok(result),
        None => Err($crate::bincode::Error::custom("In the serialized snapshot, DataHandles of different types pointed to the same object.")),
      }
    }
  }

  impl <T: SimulationStateData + PersistentlyIdentifiedType> $crate::serde::Serialize for DataHandle <T> {
    fn serialize <S: $crate::serde::Serializer> (&self, mut serializer: S)->Result <S::Ok, S::Error> {
      if MaybeVisitSerializeHackUntyped::visit_data_handle(&mut serializer, self) {return serializer.serialize_none()}
      bincode_error_to_generic(with_serialization_context (| context | {
        let object_identifier = context.find_handle::<_, DataHandle <T>> (&*self.data as *const _ as usize, || {
          Box::new (self.clone())
        })?;
        generic_error_to_bincode(object_identifier.serialize (serializer))
      }))
    }
  }
  impl <'a, T: SimulationStateData + PersistentlyIdentifiedType> $crate::serde::Deserialize <'a> for DataHandle <T> {
    fn deserialize <D: $crate::serde::Deserializer<'a>> (deserializer: D)->Result <Self, D::Error> {
      bincode_error_to_generic(with_deserialization_context (| context | {
        let object_identifier = generic_error_to_bincode(u64::deserialize (deserializer))?;
        Ok(context.find_handle::<_, DataHandle <T>> (object_identifier, || {
          Box::<DataHandle <T>>::new (DataHandle{data:Rc::new(unsafe {::std::mem::uninitialized()})}) as Box<Any>
        })?.clone())
      }))
    }
  }

  impl <B: Basics> $crate::serde::Serialize for EventHandle <B> {
    fn serialize <S: $crate::serde::Serializer> (&self, mut serializer: S)->Result <S::Ok, S::Error> {
      if MaybeVisitSerializeHack::<Steward<B>>::visit_event_handle(&mut serializer, self) {return serializer.serialize_none()}
      bincode_error_to_generic(with_serialization_context (| context | {
        let object_identifier = context.find_handle::<_, EventHandle <B>> (&*self.data as *const _ as usize, || {
          Box::new (self.clone())
        })?;
        generic_error_to_bincode(object_identifier.serialize (serializer))
      }))
    }
  }
  impl <'a, B: Basics> $crate::serde::Deserialize <'a> for EventHandle <B> {
    fn deserialize <D: $crate::serde::Deserializer<'a>> (deserializer: D)->Result <Self, D::Error> {
      bincode_error_to_generic(with_deserialization_context (| context | {
        let object_identifier = generic_error_to_bincode(u64::deserialize (deserializer))?;
        let handle = context.find_handle::<_, EventHandle <B>> (object_identifier, || {
          let handle_box = Box::<EventHandle <B>>::new (EventHandle { data: Rc::new(unsafe {::std::mem::uninitialized()})});
          handle_box.data.links.set(0);
          handle_box as Box<Any>
        })?.clone();
        handle.data.links.set (handle.data.links.get () + 1);
        Ok(handle)
      }))
    }
  }

  impl <T: DataTimeline> $crate::serde::Serialize for DataTimelineCell <T> {
    fn serialize <S: $crate::serde::Serializer> (&self, mut serializer: S)->Result <S::Ok, S::Error> {
      if MaybeVisitSerializeHack::<Steward<T::Basics>>::visit_data_timeline_cell(&mut serializer, self) {return serializer.serialize_none()}
      let foo = bincode_error_to_generic(with_serialization_context (| context | {
        Ok(context.snapshot.downcast_ref::<SnapshotHandle <T::Basics>>().unwrap().clone())
      }))?;
      let clone = foo.get_clone (&self);
      let guard = clone.data.borrow();
      guard.serialize (serializer)
    }
  }
  impl <'a, T: DataTimeline> $crate::serde::Deserialize <'a> for DataTimelineCell <T> {
    fn deserialize <D: $crate::serde::Deserializer<'a>> (deserializer: D)->Result <Self, D::Error> {
      Ok(Self::new (T::deserialize (deserializer)?))
    }
  }


  fn serialize_snapshot <B: Basics, W: Write> (writer: &mut W, snapshot: SnapshotHandle <B>)->$crate::bincode::Result <()> {
    SERIALIZATION_CONTEXT.with (| cell | {
      {
        let mut guard = cell.borrow_mut();
        assert!(guard.is_none(), "serializing recursively breaks my hacks and probably makes no sense");
        *guard = Some(SerializationContext {
          snapshot: Box::new (snapshot.clone()),
          handle_targets_observed: HashMap::new(),
          handles_to_serialize_target: Vec::new(),
          next_object_identifier: 0,
        });
      }
      // serialize inside a closure so that errors can be collected and we still clear the context afterwards
      let result = (|| {
        $crate::bincode::serialize_into (writer, snapshot.extended_now(), $crate::bincode::Bounded (::std::mem::size_of::<ExtendedTime <B>>() as u64))?;
        $crate::bincode::serialize_into (writer, snapshot.globals(), $crate::bincode::Infinite)?;

        while let Some((object_identifier, handle_box)) = cell.borrow_mut().as_mut().unwrap().handles_to_serialize_target.pop() {
          handle_box.serialize_target_into (writer, object_identifier)?;
        }

        $crate::bincode::serialize_into (writer, &SerializationElement::Finished, $crate::bincode::Infinite)
      })();

      {
        let mut guard = cell.borrow_mut();
        *guard = None;
      }
      result
    })
  }

  trait MaybeEvent { fn visit (context: &mut DeserializationContext); }
  trait MaybeData { fn visit (context: &mut DeserializationContext); }
  impl <T> MaybeEvent for T {default fn visit (_: &mut DeserializationContext) {} }
  impl <T> MaybeData for T {default fn visit (_: &mut DeserializationContext) {} }
  impl <B: Basics, T: Event <Steward = Steward <B>>> MaybeEvent for T {
    fn visit (context: &mut DeserializationContext) {
      context.event_handle_initialize_functions.insert (T::ID, event_handle_initialize_function::<B, T>);
    }
  }
  impl <T: SimulationStateData + PersistentlyIdentifiedType> MaybeData for T {
    fn visit (context: &mut DeserializationContext) {
      context.data_handle_initialize_functions.insert (T::ID, data_handle_initialize_function::<T>);
    }
  }
  impl ListOfTypesVisitor for DeserializationContext {
    fn visit <T> (&mut self) {
      <T as MaybeEvent>::visit (self);
      <T as MaybeData>::visit (self);
    }
  }

  fn deserialize_something <B: Basics, R: Read> (reader: &mut R)->$crate::bincode::Result <Steward <B>> {
    let time: ExtendedTime <B> = $crate::bincode::deserialize_from (reader, $crate::bincode::Bounded (::std::mem::size_of::<ExtendedTime <B>>() as u64))?;
    DESERIALIZATION_CONTEXT.with (| cell | {
      {
        let mut guard = cell.borrow_mut();
        assert!(guard.is_none(), "deserializing recursively breaks my hacks and probably makes no sense");
        let mut context = DeserializationContext {
          time: Box::new (time.clone()),
          data_handle_initialize_functions: ::std::collections::HashMap::new(),
          event_handle_initialize_functions: ::std::collections::HashMap::new(),
          handles: ::std::collections::HashMap::new(),
          uninitialized_handles: ::std::collections::HashSet::new(),
          predictions: ::std::collections::HashSet::new(),
          success: false,
        };
        B::Types::visit_all (&mut context);
        *guard = Some(context);
      }
      // deserialize inside a closure so that errors can be collected and we still clear the context afterwards
      let result = (|| {
        let globals: B::Globals = $crate::bincode::deserialize_from (reader, $crate::bincode::Infinite)?;

        while !cell.borrow().as_ref().unwrap().uninitialized_handles.is_empty() {
          // TODO: use actual size limits
          let next: SerializationElement = $crate::bincode::deserialize_from (reader, $crate::bincode::Infinite)?;
          //printlnerr!("{:?}", next);
          match next {
            SerializationElement::DataHandleData (object_id, type_id) => {
              let deserialize_function = *cell.borrow().as_ref().unwrap().data_handle_initialize_functions.get (&type_id).ok_or_else (|| $crate::bincode::Error::custom("Tried to deserialize a type that wasn't listed"))?;
              deserialize_function(reader, object_id)?;
            }
            SerializationElement::EventHandleData (object_id, type_id) => {
              let deserialize_function = *cell.borrow().as_ref().unwrap().event_handle_initialize_functions.get (&type_id).ok_or_else (|| $crate::bincode::Error::custom("Tried to deserialize a type that wasn't listed"))?;
              deserialize_function(reader, object_id)?;
            }
            SerializationElement::Finished => {
              return Err($crate::bincode::Error::custom("Premature end of serialized snapshot"))
            }
          };
        }

        let next: SerializationElement = $crate::bincode::deserialize_from (reader, $crate::bincode::Infinite)?;
        match next {
          SerializationElement::Finished => (),
          _ => {
            return Err($crate::bincode::Error::custom("Serialized snapshot included additional elements after successfully deserializing the state"))
          }
        };

        let mut steward = Steward::from_globals (globals/*, ValidSince::Before (time)*/);
        let mut guard = cell.borrow_mut();
        let context = guard.as_mut().unwrap();
        for prediction in context.predictions.iter() {
          deserialization_create_prediction(&mut steward, context.handles.get (prediction).unwrap().downcast_ref::<EventHandle <B>>().unwrap().clone());
        }
        steward.invalid_before = ValidSince::Before (time.base.clone()) ;
        context.success = true;
        Ok(steward)
      })();

      {
        let mut guard = cell.borrow_mut();
        *guard = None;
      }
      result
    })
  }


  };
}
