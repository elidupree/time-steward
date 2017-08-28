//! Serialization for TimeSteward data.
//!
//! TimeSteward has a few special requirements for serialization:
//! * DataTimelineHandle objects get special consideration, to support DAGs and cyclic data structures.
//! * The serialization needs to not block other operations for more than O(1) time at a time.
//! * The serialization must be lossless and platform-independent. For this reason, we always use bincode in low-endian mode.

macro_rules! time_steward_serialization_impls_for_handle {
  (
    [$($bounds:tt)*]
    [$($concrete:tt)*]
    (&$self_hack: ident)
    Data located at (| $handle: ident | &mut $data: expr)
  ) => {

/*thread_local! {
  static SERIALIZATION_CONTEXT: RefCell<Option <SerializationContext>> = RefCell::new (None);
  static DESERIALIZATION_CONTEXT: RefCell<Option <DeserializationContext>> = RefCell::new (None);
}*/


impl <$($bounds)*> $crate::serde::Serialize for $($concrete)* {
  fn serialize <S: $crate::serde::Serializer> (&$self_hack, serializer: S)->Result <S::Ok, S::Error> {
    unimplemented!()
    /*
    SERIALIZATION_CONTEXT.with (| cell | {
      let mut guard = cell.borrow_mut();
      let mut map = &mut guard.unwrap().map;
      map.insert (
    })
    identifier.serialize (serializer)
    */
  }
}

impl <'a, $($bounds)*> $crate::serde::Deserialize <'a> for $($concrete)* {
  fn deserialize <D: $crate::serde::Deserializer<'a>> (deserializer: D)->Result <Self, D::Error> {
    unimplemented!()
    /*
    let old_identifier = Identifier::deserialize (deserializer)?;
    DESERIALIZATION_CONTEXT.with (| cell | {
      let mut guard = cell.borrow_mut();
      let mut map = &mut guard.unwrap().map;
      Ok(map.entry (old_pointer).or_insert_with (| | {
        unimplemented!() 
        /*DataTimelineHandle {
          inner: Arc::new (unsafe {::std::mem::uninitialized()}),
        }*/
      }).clone().downcast::<T> ())
    })
    */
  }
}

/*
impl DeserializationContext {
  fn deserialize_(deserializer: D)->Result <(), D::Error> {
    let old_pointer = usize::deserialize (deserializer)?;
    DESERIALIZATION_CONTEXT.with (| cell | {
      let mut guard = cell.borrow_mut();
      let mut map = &mut guard.unwrap().map;
      let $handle = map.entry (old_pointer).or_insert_with (| | {
        unimplemented!()
        /*DataTimelineHandle {
          inner: Arc::new (unsafe {::std::mem::uninitialized()}),
        }*/
      });
      let new_pointer = &mut $data;
      unsafe {::std::ptr::write (
        new_pointer as *mut DataTimelineHandle,
        unimplemented!() //DataTimeline::deserialize (D)
      )?;}
    })?;
    Ok(())
  }
}*/


  };
}

#[doc (hidden)]
#[macro_export]
macro_rules! time_steward_serialization_impls {
  () => {
  
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
  
  use $crate::serde::{Serialize, Deserialize};
  use $crate::serde::ser::Error;
    
  #[derive (Serialize, Deserialize)]
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
  impl<T: StewardData> SerializeTargetInto for DataHandle <T> {
    fn serialize_target_into(&self, writer: &mut Write, object_id: u64)->$crate::bincode::Result <()> {
      $crate::bincode::serialize_into (writer, & SerializationElement::DataHandleData (object_id, T::ID), $crate::bincode::Infinite)?;
      $crate::bincode::serialize_into (writer, &*self.data, $crate::bincode::Infinite)
    }
  }
  fn data_handle_initialize_function <T: StewardData>(reader: &mut Read, object_id: u64)->$crate::bincode::Result <()> {
    with_deserialization_context (| context | {
      let handle = context.find_handle::<_, DataHandle <T>> (object_id, || {
        Box::new (DataHandle::new(unsafe {::std::mem::uninitialized()}))
      })?;
      unsafe {::std::ptr::write (
        &mut*handle.data as *mut T,
        $crate::bincode::deserialize_from (reader, $crate::bincode::Infinite)?
      );}
      Ok(())
    })
  }
  
  impl<B: Basics> SerializeTargetInto for EventHandle <B> {
    fn serialize_target_into(&self, writer: &mut Write, object_id: u64)->$crate::bincode::Result <()> {
      $crate::bincode::serialize_into (writer, & SerializationElement::DataHandleData (object_id, self.data.data.persistent_type_id()), $crate::bincode::Infinite)?;
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
      let handle = context.find_handle::<_, <T::Steward as TimeSteward>::EventHandle> (object_id, || {
        Box::new (EventHandle { data: unsafe {::std::mem::uninitialized()}})
      })?;
      let time: ExtendedTime <B> = ::bincode::deserialize_from (reader, $crate::bincode::Infinite)?;
      let in_future = time > *context.time.downcast_ref::<ExtendedTime <B>>().unwrap();
      let data: T = ::bincode::deserialize_from (reader, $crate::bincode::Infinite)?;
      unsafe {::std::ptr::write (
        &mut*handle.data as *mut EventInner<B>,
        EventInner {
          time: time.clone(),
          data: Box::new (data),
          should_be_executed: Cell::new (in_future),
          is_prediction: in_future,
          prediction_destroyed_by: RefCell::new (None),
          execution_state: RefCell::new (None),
        }
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
      PersistentTypeId, fn (&mut Write)->$crate::bincode::Result <()>>,
    event_handle_initialize_functions: ::std::collections::HashMap <
      PersistentTypeId, fn (&mut Write)->$crate::bincode::Result <()>>,
    handles: ::std::collections::HashMap <u64, Box <Any>>,
    uninitialized_handles: ::std::collections::HashSet <u64>,
  }
  
  thread_local! {
    static SERIALIZATION_CONTEXT: RefCell<Option <SerializationContext>> = RefCell::new (None);
    static DESERIALIZATION_CONTEXT: RefCell<Option <DeserializationContext>> = RefCell::new (None);
  }
  
  fn with_serialization_context <R, F: FnOnce (&mut SerializationContext)->Result <R, $crate::bincode::Error>> (callback: F)->Result <R, $crate::bincode::Error> {
    SERIALIZATION_CONTEXT.with (| cell | {
      let mut guard = cell.borrow_mut();
      let mut context = match guard.as_mut() {Some(x)=>x, None=>return Err($crate::bincode::Error::custom("Tried to serialize TimeSteward data without a serialization context."))};
      callback (context)
    })
  }
  fn with_deserialization_context <R, F: FnOnce (&mut DeserializationContext)->Result <R, $crate::bincode::Error>> (callback: F)->Result <R, $crate::bincode::Error> {
    DESERIALIZATION_CONTEXT.with (| cell | {
      let mut guard = cell.borrow_mut();
      let mut context = match guard.as_mut() {Some(x)=>x, None=>return Err($crate::bincode::Error::custom("Tried to deserialize TimeSteward data without a deserialization context."))};
      callback (context)
    })
  }
  
  impl SerializationContext {
    fn find_handle <F: FnOnce()->Box <SerializeTargetInto>, T> (&mut self, pointer: usize, create_serializable: F)->Result<u64, $crate::bincode::Error> {
      let object_identifier = *self.handle_targets_observed.entry (pointer).or_insert_with (|| {
        let result = self.next_object_identifier;
        self.next_object_identifier += 1;
        self.handles_to_serialize_target.push((result, create_serializable()));
        result
      });
      Ok(object_identifier)
    }
  }
  
  impl DeserializationContext {
    fn find_handle <F: FnOnce()->Box <Any>, T: Any> (&mut self, object_identifier: u64, create_uninitialized: F)->Result<&mut T, $crate::bincode::Error> {
      let entry = self.handles.entry (object_identifier).or_insert_with (|| {
        let created = create_uninitialized();
        self.uninitialized_handles.insert (object_identifier);
        created
      });
      match (*entry).downcast_mut::<T>() {
        Some (result) => Ok(result),
        None => Err($crate::bincode::Error::custom("In the serialized snapshot, DataHandles of different types pointed to the same object.")),
      }
    }
  }
  
  impl <T: StewardData> $crate::serde::Serialize for DataHandle <T> {
    fn serialize <S: $crate::serde::Serializer> (&self, serializer: S)->Result <S::Ok, S::Error> {
      bincode_error_to_generic(with_serialization_context (| context | {
        let object_identifier = context.find_handle::<_, DataHandle <T>> (&*self.data as *const _ as usize, || {
          Box::new (self.clone())
        })?;
        generic_error_to_bincode(object_identifier.serialize (serializer))
      }))
    }
  }
  impl <'a, T: StewardData> $crate::serde::Deserialize <'a> for DataHandle <T> {
    fn deserialize <D: $crate::serde::Deserializer<'a>> (deserializer: D)->Result <Self, D::Error> {
      bincode_error_to_generic(with_deserialization_context (| context | {
        let object_identifier = generic_error_to_bincode(u64::deserialize (deserializer))?;
        Ok(context.find_handle::<_, DataHandle <T>> (object_identifier, || {
          Box::new (DataHandle::new(unsafe {::std::mem::uninitialized()}))
        })?.clone())
      }))
    }
  }
  
  impl <B: Basics> $crate::serde::Serialize for EventHandle <B> {
    fn serialize <S: $crate::serde::Serializer> (&self, serializer: S)->Result <S::Ok, S::Error> {
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
        Ok(context.find_handle::<_, EventHandle <B>> (object_identifier, || {
          Box::new (EventHandle {data:unsafe {::std::mem::uninitialized()}})
        })?.clone())
      }))
    }
  }
  
  impl <T: DataTimeline> $crate::serde::Serialize for DataTimelineCell <T> {
    fn serialize <S: $crate::serde::Serializer> (&self, serializer: S)->Result <S::Ok, S::Error> {
      bincode_error_to_generic(with_serialization_context (| context | {
        generic_error_to_bincode(context.snapshot.downcast_ref::<SnapshotHandle <T::Basics>>().unwrap().get_clone (&self).serialize (serializer))
      }))
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
        let guard = cell.borrow_mut();
        assert!(guard.is_none(), "serializing recursively breaks my hacks and probably makes no sense");
        *guard = Some(SerializationContext {
          snapshot: Box::new (snapshot),
          handle_targets_observed: HashMap::new(),
          handles_to_serialize_target: Vec::new(),
          next_object_identifier: 0,
        });
      }
      // serialize inside a closure so that errors can be collected and we still clear the context afterwards
      let result = (|| {
        $crate::bincode::serialize_into (writer, snapshot.extended_now(), $crate::bincode::Bounded (::std::mem::size_of::<ExtendedTime <B>>() as u64))?;
        $crate::bincode::serialize_into (writer, snapshot.globals(), $crate::bincode::Infinite)?;
    
        while let Some((object_identifier, handle_box)) = cell.borrow().unwrap().handles_to_serialize_target.pop() {
          handle_box.serialize_target_into (writer, object_identifier);
        }
        
        $crate::bincode::serialize_into (writer, &SerializationElement::Finished, $crate::bincode::Infinite)
      })();

      {
        let guard = cell.borrow_mut();
        *guard = None;
      }
      result
    })
  }
  
  fn deserialize_something <B: Basics, R: Read> (reader: &mut R)->$crate::bincode::Result <Steward <B>> {
    let time: ExtendedTime <B> = $crate::bincode::deserialize_from (reader, $crate::bincode::Bounded (::std::mem::size_of::<ExtendedTime <B>>() as u64))?;
    DESERIALIZATION_CONTEXT.with (| cell | {
      {
        let guard = cell.borrow_mut();
        assert!(guard.is_none(), "deserializing recursively breaks my hacks and probably makes no sense");
        *guard = Some(DeserializationContext {
          time: Box::new (time.clone()),
          data_handle_initialize_functions: ::std::collections::HashMap::new(),
          event_handle_initialize_functions: ::std::collections::HashMap::new(),
          handles: ::std::collections::HashMap::new(),
          uninitialized_handles: ::std::collections::HashSet::new(),
        });
      }
      // deserialize inside a closure so that errors can be collected and we still clear the context afterwards
      let result = (|| {
        let globals: B::Globals = $crate::bincode::deserialize_from (reader, $crate::bincode::Infinite)?;
    
        while !cell.borrow().as_ref().unwrap().uninitialized_handles.is_empty() {
          // TODO: use actual size limits
          let next: SerializationElement = $crate::bincode::deserialize_from (reader, $crate::bincode::Infinite)?;
          match next {
            SerializationElement::DataHandleData (object_id, type_id) => {
              let deserialize_function = cell.borrow().as_ref().unwrap().data_handle_initialize_functions.get (type_id);
              (*deserialize_function)(reader, object_id);
            }
            SerializationElement::EventHandleData (object_id, type_id) => {
              let deserialize_function = cell.borrow().as_ref().unwrap().event_handle_initialize_functions.get (type_id);
              (*deserialize_function)(reader, object_id);
            }
            SerializationElement::Finished => {
              // TODO: clean up broken state
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
        
        Ok(Steward::from_globals (globals, ValidSince::Before (time)))
      })();

      {
        let guard = cell.borrow_mut();
        *guard = None;
      }
      result
    })
  }
  
  
  };
}


