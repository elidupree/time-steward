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
    fn serialize_into(&self, writer: &mut Write, object_id: u64)->$crate::bincode::Result <()> {
      $crate::bincode::serialize_into (writer, SerializationElement::DataHandleData (T::ID), $crate::bincode::Infinite)?;
      $crate::bincode::serialize_into (writer, &*self.data, $crate::bincode::Infinite)?;
    }
  }
  fn data_handle_initialize_function <T: StewardData>(reader: &mut Read, object_id: u64) {
    with_deserialization_context (| context | {
      let handle = context.find_handle<_, DataHandle <T>> (object_id, || {
        Box::new (DataHandle::new(unsafe {::std::mem::uninitialized()}))
      })?;
      unsafe {::std::ptr::write (
        handle.data as *mut T,
        ::bincode::deserialize_from (reader, $crate::bincode::Infinite)
      )?;}
    })
  }
  
  impl<B: Basics> SerializeTargetInto for EventHandle <B> {
    fn serialize_into(&self, writer: &mut Write, object_id: u64)->$crate::bincode::Result <()> {
      $crate::bincode::serialize_into (writer, SerializationElement::DataHandleData (self.data.data.persistent_type_id()), $crate::bincode::Infinite)?;
      $crate::bincode::serialize_into (writer, self.extended_time(), $crate::bincode::Infinite)?;
      self.data.data.serialize_into (writer)
    }
  }
  impl<T: EventInnerTrait> SerializeInto for T {
    fn serialize_into(&self, writer: &mut Write)->$crate::bincode::Result <()> {
      $crate::bincode::serialize_into (writer, self, $crate::bincode::Infinite)?;
    }
  }
  fn event_handle_initialize_function <T: Event>(reader: &mut Read, object_id: u64) {
    with_deserialization_context (| context | {
      let handle = context.find_handle<_, <T::Steward as TimeSteward>::EventHandle> (object_id, || {
        Box::new (EventHandle::new(unsafe {::std::mem::uninitialized()}))
      })?;
      let time = ::bincode::deserialize_from (reader, $crate::bincode::Infinite);
      let data = ::bincode::deserialize_from (reader, $crate::bincode::Infinite);
      unsafe {::std::ptr::write (
        handle.data as *mut T,
        EventInner {
          time: time.clone(),
          data: Box::new <Data>,
          should_be_executed: Cell::new (time > context.time),
          is_prediction: time > context.time,
          prediction_destroyed_by: RefCell::new (None),
          execution_state: RefCell::new (None),
        }
      )?;}
    })
  }
  
  struct SerializationContext {
    snapshot: Box <Any>,
    handle_targets_observed: HashMap <*const (), u64>,
    handles_to_serialize_target: Vec<(u64, Box <SerializeTargetInto>)>,
    next_object_identifier: u64,
  }
  struct DeserializationContext {
    data_handle_initialize_functions: ::std::collections::HashMap <
      PersistentTypeId, fn (&mut Write)>,
    event_handle_initialize_functions: ::std::collections::HashMap <
      PersistentTypeId, fn (&mut Write)>,
    handles: ::std::collections::HashMap <u64, Box <Any>>,
    uninitialized_handles: ::std::collections::HashSet <u64>,
  }
  
  thread_local! {
    static SERIALIZATION_CONTEXT: RefCell<Option <SerializationContext>> = RefCell::new (None);
    static DESERIALIZATION_CONTEXT: RefCell<Option <DeserializationContext>> = RefCell::new (None);
  }
  
  fn with_serialization_context <S: $crate::serde::Serializer, R, F: FnOnce (&mut SerializationContext <Steward>)->R> (callback: F)->Result <R, S::Error> {
    SERIALIZATION_CONTEXT.with (| cell | {
      let mut guard = cell.borrow_mut();
      let mut context = match guard.as_mut() {Some(x)=>x, None=>return S::Error::Custom("Tried to serialize TimeSteward data without a serialization context.")};
      callback (context)
    })
  }
  fn with_deserialization_context <D: $crate::serde::Deserializer, Steward: TimeSteward, R, F: FnOnce (&mut DeserializationContext <Steward>)->R> (callback: F)->Result <R, D::Error> {
    DESERIALIZATION_CONTEXT.with (| cell | {
      let mut guard = cell.borrow_mut();
      let mut context = match guard.as_mut() {Some(x)=>x, None=>return D::Error::Custom("Tried to deserialize TimeSteward data without a deserialization context.")};
      callback (context)
    })
  }
  
  impl SerializationContext {
    fn find_handle <S: $crate::serde::Serializer, F: FnOnce()->Box <Any>, T> (&mut self, serializer: S, initialize: F)->Result<(), S::Error> {
      let object_identifier = self.handle_targets_observed.entry (self.data as *const ()).or_insert_with (|| {
        let result = self.next_object_identifier;
        self.next_object_identifier += 1;
        self.handles_to_serialize_target.insert (object_identifier, initialize());
        result
      });
      object_identifier.serialize (serializer)
    }
  }
  
  impl DeserializationContext {
    fn find_handle <D: $crate::serde::Deserializer, F: FnOnce()->Box <Any>, T> (&mut self, object_identifier: u64, initialize: F)->Result<&mut T, D::Error> {
      let entry = self.data_handles.entry (object_identifier).or_insert_with (|| {
        let created = initialize();
        context.uninitialized_data_handles.insert (created.clone());
        created
      });
      match (*entry).downcast_mut::<T>() {
        Some (result) => Ok(result),
        None => D::Error::Custom("In the serialized snapshot, DataHandles of different types pointed to the same object."),
      }
    }
  }
  
  impl <T: StewardData> $crate::serde::Serialize for DataHandle <T> {
    fn serialize <S: $crate::serde::Serializer> (&self, serializer: S)->Result <S::Ok, S::Error> {
      with_serialization_context (| context | {
        context.find_handle<S, _, DataHandle <T>> (serializer, || {
          Box::new (self.clone())
        })
      })
    }
  }
  impl <'a, T: StewardData> $crate::serde::Deserialize <'a> for DataHandle <T> {
    fn deserialize <D: $crate::serde::Deserializer<'a>> (deserializer: D)->Result <Self, D::Error> {
      with_deserialization_context (| context | {
        let object_identifier = u64::deserialize (deserializer)?;
        context.find_handle<_, DataHandle <T>> (object_identifier, || {
          Box::new (DataHandle::new(unsafe {::std::mem::uninitialized()}))
        })?.clone()
      })
    }
  }
  
  impl <B: Basics> $crate::serde::Serialize for EventHandle <B> {
    with_serialization_context (| context | {
      let object_identifier = u64::deserialize (deserializer)?;
      context.find_handle<S, _, DataHandle <T>> (serializer, || {
        Box::new (self.clone())
      })
    })
  }
  impl <'a, B: Basics> $crate::serde::Deserialize <'a> for EventHandle <B> {
    with_deserialization_context (| context | {
      let object_identifier = u64::deserialize (deserializer)?;
      context.find_handle<D, _, EventHandle <B>> (object_identifier, || {
        Box::new (EventHandle::new(unsafe {::std::mem::uninitialized()}))
      })?.clone()
    })
  }
  
  impl <T: DataTimeline> $crate::serde::Serialize for DataTimelineCell <T> {
    fn serialize <S: $crate::serde::Serializer> (&self, serializer: S)->Result <S::Ok, S::Error> {
      with_serialization_context (| context | {
        context.snapshot.get_clone (&self).serialize (serializer)
      })
    }
  }
  impl <'a, T: DataTimeline> $crate::serde::Deserialize <'a> for DataTimelineCell <T> {
    fn deserialize <D: $crate::serde::Deserializer<'a>> (deserializer: D)->Result <Self, D::Error> {
      Self::new (T::deserialize (deserializer))
    }
  }

  
  fn serialize_snapshot <B: Basics, W: Write> (writer: &mut W, snapshot: Snapshot <B>)->$crate::bincode::Result <()> {
    SERIALIZATION_CONTEXT.with (| cell | {
      {
        let guard = cell.borrow_mut();
        assert!(guard.is_none(), "serializing recursively breaks my hacks and probably makes no sense");
        *guard = Some(SerializationContext {
        
        });
      }
      // serialize inside a closure so that errors can be collected and we still clear the context afterwards
      let result = || {
        $crate::bincode::serialize_into (writer, time, ::std::mem::size_of::<ExtendedTime <B>>())?;
        $crate::bincode::serialize_into (writer, globals, $crate::bincode::Infinite)?;
    
        loop {
          if let (object_identifier, handle_box) = cell.borrow().data_handles_to_serialize_target.pop() {
            handle_box.serialize_into (writer);
          }
          if let (object_identifier, event_handle) = cell.borrow().event_handles_to_serialize_target.pop() {
            event_handle.serialize_into (writer);
          }
        }
        
        $crate::bincode::serialize_into (writer, SerializationElement::Finished, $crate::bincode::Infinite)?;
        
      }();

      {
        let guard = cell.borrow_mut();
        *guard = None;
      }
      result
    })
  }
  
  fn deserialize_something <R: Read> (reader: &mut R)->$crate::bincode::Result <()> {
    DESERIALIZATION_CONTEXT.with (| cell | {
      {
        let guard = cell.borrow_mut();
        assert!(guard.is_none(), "deserializing recursively breaks my hacks and probably makes no sense");
        *guard = Some(DeserializationContext::new());
      }
      // deserialize inside a closure so that errors can be collected and we still clear the context afterwards
      let result = || {
        let time: ExtendedTime <B> = deserialize_from (reader, ::std::mem::size_of::<ExtendedTime <B>>())?;
        let globals: B::Globals = deserialize_from (reader, $crate::bincode::Infinite)?;
    
        while !cell.borrow().uninitialized_handles.is_empty() {
          // TODO: use actual size limits
          let next: SerializationElement = $crate::bincode::deserialize_from (reader, $crate::bincode::Infinite)?;
          match next {
            SerializationElement::DataTimeline (object_id, type_id) => {
              let deserialize_function = cell.borrow().data_handle_initialize_functions.get (type_id);
              (*deserialize_function)(reader, object_id);
            }
            SerializationElement::Event (object_id, type_id) => {
              let deserialize_function = cell.borrow().event_handle_initialize_functions.get (type_id);
              (*deserialize_function)(reader, object_id);
            }
            SerializationElement::Finished => {
              // TODO: clean up broken state
              return $crate::bincode::Result::Custom("Premature end of serialized snapshot")
            }
          };
        }
        
        let next: SerializationElement = $crate::bincode::deserialize_from (reader, $crate::bincode::Infinite)?;
        match next {
          SerializationElement::Finished => (),
          _ => {
            return $crate::bincode::Result::Custom("Serialized snapshot included additional elements after successfully deserializing the state")
          }
        };
        
        Ok(Steward::from_globals (globals, ValidSince::Before (time)))
      }();

      {
        let guard = cell.borrow_mut();
        *guard = None;
      }
      result
    })
  }
  
  
  };
}


