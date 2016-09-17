use std::collections::HashMap;
use std::hash::{Hash, Hasher, SipHasher};
use std::any::Any;
use std::sync::Arc;
use std::cmp::Ordering;
use std::fmt::{self, Debug};
use std::borrow::Borrow;
use std::io::{self, Write, Read};
use rand::{Rng, ChaChaRng};
use serde::{Serialize, Deserialize};
use bincode;

use implementation_support::list_of_types::{ColumnList, EventList, PredictorList};

/// A 128-bit random ID used for rows and ExtendedTimes.
#[derive (Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Debug, Serialize, Deserialize)]
pub struct DeterministicRandomId {
  data: [u64; 2],
}
pub struct SiphashIdGenerator {
  data: [SipHasher; 2],
}
impl Write for SiphashIdGenerator {
  fn write(&mut self, bytes: &[u8]) -> io::Result<usize> {
    self.data[0].write(bytes);
    self.data[1].write(bytes);
    Ok(bytes.len())
  }
  fn flush(&mut self) -> io::Result<()> {
    Ok(())
  }
}
impl SiphashIdGenerator {
  pub fn generate(&self) -> DeterministicRandomId {
    DeterministicRandomId { data: [self.data[0].finish(), self.data[1].finish()] }
  }
  pub fn new() -> SiphashIdGenerator {
    SiphashIdGenerator {
      data: [SipHasher::new_with_keys(0xb82a9426fd1a574f, 0x9d9d5b703dcb1bcc),
             SipHasher::new_with_keys(0x03e0d6037ff980a4, 0x65b790a0825b83bd)],
    }
  }
}
impl DeterministicRandomId {
  /// Generates a new DeterministicRandomId from any Serialize type, using a cryptographic hash function.
  ///
  /// Generally, calling this with unique inputs is guaranteed to give unique outputs.
  /// However, take note: the serialization doesn't generally include any representation of the
  /// *type* of data serialized, so (for instance) two different structs with the
  /// same fields will produce the same result. If you want to make sure and ID is unique,
  /// a good technique is to use a tuple containing a unique numeric literal
  /// (randomly-generated to be unique to that part of your code):
  ///
  /// ```rust
  /// # type YourDataType = Vec<u64>;
  /// # use time_steward::DeterministicRandomId;
  /// let id = DeterministicRandomId::new(&(0xdefacab1e_bad_1du64, YourDataType::new()));
  /// ```
  /// 
  /// Why do we use Serialize rather than Hash?
  /// We need to make sure that DeterministicRandomIds do not vary with CPU endianness.
  /// The trait [Hasher](https://doc.rust-lang.org/std/hash/trait.Hasher.html) "represents the ability to hash an arbitrary stream of bytes",
  /// and typical implementations of Hash submit the bytes in the order they appear
  /// on the current system, not in a standardized order.
  /// Therefore, we generate IDs from Serialize implementors,
  /// because Serialize IS meant to be compatible between platforms.
  pub fn new<T: Serialize>(data: &T) -> DeterministicRandomId {
    let mut writer = SiphashIdGenerator::new();
    bincode::serde::serialize_into(&mut writer, data, bincode::SizeLimit::Infinite).unwrap();
    writer.generate()
  }
  /// Rather than implement Rand for this type, we make sure that it can
  /// ONLY be generated from specific RNGs known to be cryptographically secure.
  pub fn from_rng(rng: &mut ChaChaRng) -> DeterministicRandomId {
    DeterministicRandomId { data: [rng.gen::<u64>(), rng.gen::<u64>()] }
  }
  /// TimeSteward implementors use this internally to make sure fiat events have unique ids.
  ///
  /// We combine fiat event ids with unique random data so that TimeSteward impls
  /// can trust them not to collide with *other* TimeIds.
  /// We use + instead of XOR so that this won't fail if the user accidentally
  /// or maliciously calls this BEFORE passing the ids in, too.
  #[doc (hidden)]
  pub fn for_fiat_event_internal(&self) -> DeterministicRandomId {
    DeterministicRandomId {
      data: [self.data[0].wrapping_add(0xc1d40daaee67461d),
             self.data[1].wrapping_add(0xb23ce1f459edefff)],
    }
  }
  /// Returns the internal data of the DeterministicRandomId.
  pub fn data(&self) -> &[u64; 2] {
    &self.data
  }
}
impl fmt::Display for DeterministicRandomId {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    write!(f, "id:{:016x}{:016x}", self.data[0], self.data[1])
  }
}

pub type RowId = DeterministicRandomId;
pub type TimeId = DeterministicRandomId;

/// The ID type for implementors of trait Column<span class="inline_random_id" data-idtype="ColumnId"></span>.
///
/// <div class="random_ids"></div>
#[derive (Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
pub struct ColumnId(pub u64);

/// The ID type for implementors of trait Predictor<span class="inline_random_id" data-idtype="PredictorId"></span>.
///
/// <div class="random_ids"></div>
#[derive (Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
pub struct PredictorId(pub u64);

/// The ID type for implementors of trait Event<span class="inline_random_id" data-idtype="EventId"></span>.
///
/// <div class="random_ids"></div>
#[derive (Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
pub struct EventId(pub u64);

impl fmt::Debug for ColumnId {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    write!(f, "ColumnId(0x{:016x})", self.0)
  }
}
impl fmt::Debug for PredictorId {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    write!(f, "PredictorId(0x{:016x})", self.0)
  }
}
impl fmt::Debug for EventId {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    write!(f, "EventId(0x{:016x})", self.0)
  }
}


pub trait Column: Any {
  type FieldType: Any + Send + Sync + Clone + Eq + Serialize + Deserialize + Debug;// = Self;

  /**
  Returns a constant identifier for the type, which must be 64 bits of random data.
  
  TODO: change this into an associated constant once associated constants become stable.
  
  <div class="random_ids"></div>
  */
  fn column_id() -> ColumnId;
}


#[derive (Copy, Clone, PartialEq, Eq, Hash, Debug, Serialize, Deserialize)]
pub struct FieldId {
  pub row_id: RowId,
  pub column_id: ColumnId,
}
impl FieldId {
  pub fn new(row_id: RowId, column_id: ColumnId) -> FieldId {
    FieldId {
      row_id: row_id,
      column_id: column_id,
    }
  }
}

// I'm not sure exactly what synchronization properties we will need for these callbacks,
// so I'm requiring both Send and Sync for now to future-proof them.
// Serialize is required for synchronization checking.
// Serialize + Deserialize is needed for fiat events in order to transmit them.
// Clone makes things easier for crossverified time stewards, and
//   shouldn't be too hard for a Serialize + Deserialize type.
// I don't have plans to use Deserialize for other events/predictors,
// but it's possible that I might, so I included it for more future-proofing.
// I'm not sure if 'static (from Any) is strictly necessary, but it makes things easier, and
// wanting a non-'static callback (which still must live at least as long as the TimeSteward)
// seems like a very strange situation.
pub trait Event
  : Any + Send + Sync + Clone + Eq + Serialize + Deserialize + Debug {
  type Basics: Basics;
  fn call<M: Mutator<Basics = Self::Basics>>(&self, mutator: &mut M);
  
  /**
  Returns a constant identifier for the type, which must be 64 bits of random data.
  
  TODO: change this into an associated constant once associated constants become stable.
  
  <div class="random_ids"></div>
  */
  fn event_id() -> EventId;
}
pub trait Predictor
  : Any + Send + Sync + Clone + Eq + Serialize + Deserialize + Debug {
  type Basics: Basics;
  fn call<PA: PredictorAccessor<Basics = Self::Basics>>(accessor: &mut PA, id: RowId);
  
  /**
  Returns a constant identifier for the type, which must be 64 bits of random data.
  
  TODO: change this into an associated constant once associated constants become stable.
  
  <div class="random_ids"></div>
  */
  fn predictor_id() -> PredictorId;
  
  type WatchedColumn: Column;
}

#[macro_export]
macro_rules! time_steward_predictor {
  ([$($privacy:tt)*] struct $Struct: ident
    <$([$Parameter: ident $($bounds:tt)*]),*>,
    $B: ty, $predictor_id: expr, watching $Column: ty,
    | $accessor_name: ident, $row_name: ident | $contents: expr) => {
    
    #[derive (Clone, PartialEq, Eq, Serialize, Deserialize, Debug)]
    $($privacy)* struct $Struct<$($Parameter $($bounds)*),*>(::std::marker::PhantomData <($($Parameter),*)>);
    impl<$($Parameter $($bounds)*),*> $crate::Predictor for $Struct <$($Parameter),*> {
      type Basics = $B;
      fn call <P: $crate::PredictorAccessor <Basics = $B>> ($accessor_name: &mut P, $row_name: RowId) {
        $contents
      }
      #[inline (always)]
      fn predictor_id()->$crate::PredictorId {$predictor_id}
      type WatchedColumn = $Column;
    }
  };
  ([$($privacy:tt)*] struct $Struct: ident
    <$([$Parameter: ident $($bounds:tt)*]),*>,
    $B: ty, $predictor_id: expr, watching $Column: ty,
    $generic_function: ident) => {
    time_steward_predictor! ([$($privacy)*] struct $Struct <$([$Parameter $($bounds)*]),*>, $B, $predictor_id, watching $Column, | accessor, id | $generic_function::<$($Parameter),*>(accessor, id));
  };
  ([$($privacy:tt)*] struct $Struct: ident,
    $B: ty, $predictor_id: expr, watching $Column: ty,
    $($rest:tt)*) => {
    time_steward_predictor! ([$($privacy)*] struct $Struct <>, $B, $predictor_id, watching $Column, $($rest)*);
  };
  (pub struct $($rest:tt)*) => {
    time_steward_predictor! ([pub] struct $($rest)*);
  };
  (struct $($rest:tt)*) => {
    time_steward_predictor! ([] struct $($rest)*);
  };
}

#[macro_export]
macro_rules! time_steward_event {
  ([$($privacy:tt)*] struct $Struct: ident <$([$Parameter: ident $($bounds:tt)*]),*>{$($field_name: ident: $field_type: ty),*}, $B: ty, $event_id: expr, | &$self_name: ident, $mutator_name: ident | $contents: expr) => {
    #[derive (Clone, PartialEq, Eq, Serialize, Deserialize, Debug)]
    $($privacy)* struct $Struct<$($Parameter $($bounds)*),*>{
      $($field_name: $field_type),*
    }
    impl<$($Parameter $($bounds)*),*> $crate::Event for $Struct <$($Parameter),*> {
      type Basics = $B;
      fn call <M: $crate::Mutator <Basics = $B>> (&$self_name, $mutator_name: &mut M) {
        $contents
      }
      #[inline (always)]
      fn event_id()->$crate::EventId {$event_id}
    }
    impl<$($Parameter $($bounds)*),*> $Struct <$($Parameter),*> {
      #[allow (dead_code)]
      $($privacy)* fn new($($field_name: $field_type),*)->Self {$Struct {$($field_name: $field_name),*}}
    }
  };
  ([$($privacy:tt)*] struct $Struct: ident <$([$Parameter: ident $($bounds:tt)*]),*>{$($field_name: ident: $field_type: ty),*}, $B: ty, $event_id: expr, $generic_function: ident) => {
    time_steward_event! ([$($privacy)*] struct $Struct <$([$Parameter $($bounds)*]),*>{$($field_name: $field_type),*}, $B, $event_id, | &self, mutator | $generic_function::<$($Parameter),*>(self, mutator, $Struct));
  };
  ([$($privacy:tt)*] struct $Struct: ident{$($field_name: ident: $field_type: ty),*}, $B: ty, $event_id: expr, $($rest:tt)*) => {
    time_steward_event! ([$($privacy)*] struct $Struct <>{$($field_name: $field_type),*}, $B, $event_id, $($rest)*);
  };
  (pub struct $($rest:tt)*) => {
    time_steward_event! ([pub] struct $($rest)*);
  };
  (struct $($rest:tt)*) => {
    time_steward_event! ([] struct $($rest)*);
  };
}


#[macro_export]
macro_rules! time_steward_basics {
  ([$($privacy:tt)*] struct $Basics: ident {$($contents:tt)*}) => {
    #[derive (Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize, Debug, Default)]
    $($privacy)* struct $Basics;
    impl $crate::Basics for $Basics {
      $($contents)*
    }
  };
  (pub struct $($rest:tt)*) => {
    time_steward_basics! ([pub] struct $($rest)*);
  };
  (struct $($rest:tt)*) => {
    time_steward_basics! ([] struct $($rest)*);
  };
}


/**
This is intended to be implemented on an empty struct. Requiring Clone etc. is a hack to work around [a compiler weakness](https://github.com/rust-lang/rust/issues/26925).
*/
pub trait Basics
  : Any + Send + Sync + Copy + Clone + Ord + Hash + Serialize + Deserialize + Debug + Default {
  type Time: Any + Send + Sync + Clone + Ord + Hash + Serialize + Deserialize + Debug;
  type Constants: Any + Send + Sync + Clone + Eq + Serialize + Deserialize + Debug;
  type IncludedTypes: ColumnList + EventList<Self> + PredictorList<Self>;
  fn allow_floats_unsafe() -> bool {
    false
  }
}

pub type IterationType = u32;
#[derive (Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Debug, Serialize, Deserialize)]
pub struct ExtendedTime<B: Basics> {
  pub base: B::Time,
  pub iteration: IterationType,
  pub id: TimeId,
}

pub type StewardRc<T> = Arc<T>;
pub type FieldRc = StewardRc<Any>;

pub fn unwrap_field<'a, C: Column>(field: &'a FieldRc) -> &'a C::FieldType {
  field.downcast_ref::<C::FieldType>().expect("a field had the wrong type for its column").borrow()
}

pub trait Accessor {
  type Basics: Basics;
  fn generic_data_and_extended_last_change(&self,
                                           id: FieldId)
                                           -> Option<(&FieldRc, &ExtendedTime<Self::Basics>)>;
  fn data_and_extended_last_change<C: Column>
    (&self,
     id: RowId)
     -> Option<(&C::FieldType, &ExtendedTime<Self::Basics>)> {
    self.generic_data_and_extended_last_change(FieldId::new(id, C::column_id()))
      .map(|pair| (unwrap_field::<C>(pair.0), pair.1))
  }
  fn data_and_last_change<C: Column>
    (&self,
     id: RowId)
     -> Option<(&C::FieldType, &<<Self as Accessor>::Basics as Basics>::Time)> {
    self.generic_data_and_extended_last_change(FieldId::new(id, C::column_id()))
      .map(|pair| (unwrap_field::<C>(pair.0), &pair.1.base))
  }
  fn get<C: Column>(&self, id: RowId) -> Option<&C::FieldType> {
    self.generic_data_and_extended_last_change(FieldId::new(id, C::column_id()))
      .map(|p| unwrap_field::<C>(p.0))
  }
  fn last_change<C: Column>(&self,
                            id: RowId)
                            -> Option<&<<Self as Accessor>::Basics as Basics>::Time> {
    self.generic_data_and_extended_last_change(FieldId::new(id, C::column_id())).map(|p| &p.1.base)
  }
  fn constants(&self) -> &<<Self as Accessor>::Basics as Basics>::Constants;

  /**
  In general, predictions may NOT depend on the time the predictor is called.
  However, in some cases, you may want to have a predictor that does something like
  "predict an event to happen at the beginning of any minute", which isn't technically
  time-dependent – but if you did it in a time-independent way, you'd have to predict
  infinitely many events. unsafe_now() is provided to enable you to only compute one of them.
  
  When you call unsafe_now() in a predictor, you promise that you will
  make the SAME predictions for ANY given return value of unsafe_now(), UNLESS:
  1. The return value is BEFORE the last change time of any of the fields you access, or
  2. You predict an event, and the return value is AFTER that event.
  
  This function is provided by Accessor rather than PredictorAccessor so that functions can be generic in whether they are used in a predictor or not.
  */
  fn unsafe_now(&self) -> &<<Self as Accessor>::Basics as Basics>::Time;
}

pub trait MomentaryAccessor: Accessor {
  fn now(&self) -> &<<Self as Accessor>::Basics as Basics>::Time {
    self.unsafe_now()
  }
}

pub trait Mutator: MomentaryAccessor + Rng {
  fn extended_now(&self) -> &ExtendedTime<<Self as Accessor>::Basics>;
  fn set<C: Column>(&mut self, id: RowId, data: Option<C::FieldType>);
  fn gen_id(&mut self) -> RowId;
}
pub trait PredictorAccessor: Accessor {
  fn predict_at_time<E: Event<Basics = Self::Basics >>(&mut self, time: <<Self as Accessor>::Basics as Basics>::Time, event: E);

  /// A specific use of unsafe_now() that is guaranteed to be safe
  fn predict_immediately<E: Event<Basics = <Self as Accessor>::Basics>>(&mut self, event: E) {
    let time = self.unsafe_now().clone();
    self.predict_at_time(time, event)
  }
}
pub type SnapshotEntry<'a, B: Basics> = (FieldId, (&'a FieldRc, &'a ExtendedTime<B>));
// where for <'a> & 'a Self: IntoIterator <Item = SnapshotEntry <'a, B>>
pub trait Snapshot: MomentaryAccessor + Any {
  fn num_fields(&self) -> usize;
  // with slightly better polymorphism we could do this more straightforwardly
  // type Iter<'a>: Iterator<(FieldId, (&'a FieldRc, &'a ExtendedTime<B>))>;
  // fn iter (&self)->Iter;
}

#[derive (Clone, Debug)]
pub struct FiatSnapshot<B: Basics> {
  now: B::Time,
  constants: B::Constants,
  fields: HashMap<FieldId, (FieldRc, ExtendedTime<B>)>,
}
impl<B: Basics> Accessor for FiatSnapshot<B> {
  type Basics = B;
  fn generic_data_and_extended_last_change(&self,
                                           id: FieldId)
                                           -> Option<(&FieldRc, &ExtendedTime<B>)> {
    self.fields.get(&id).map(|pair| (&pair.0, &pair.1))
  }
  fn constants(&self) -> &B::Constants {
    &self.constants
  }
  fn unsafe_now(&self) -> &B::Time {
    &self.now
  }
}
impl<B: Basics> MomentaryAccessor for FiatSnapshot<B> {}
impl<B: Basics> Snapshot for FiatSnapshot<B> {
  fn num_fields(&self) -> usize {
    self.fields.len()
  }
}
impl<B: Basics> FiatSnapshot<B> {
  pub fn from_snapshot<'a, S: Snapshot<Basics = B>>(snapshot: &'a S) -> Self
    where &'a S: IntoIterator<Item = SnapshotEntry<'a, B>>
  {
    FiatSnapshot {
      now: snapshot.now().clone(),
      constants: snapshot.constants().clone(),
      fields: snapshot.into_iter()
        .map(|(id, stuff)| (id, (stuff.0.clone(), stuff.1.clone())))
        .collect(),
    }
  }
}
use std::collections::hash_map;
pub struct FiatSnapshotIter<'a, B: Basics>(hash_map::Iter<'a, FieldId, (FieldRc, ExtendedTime<B>)>);
impl<'a, B: Basics> Iterator for FiatSnapshotIter<'a, B> {
  type Item = (FieldId, (&'a FieldRc, &'a ExtendedTime<B>));
  fn next(&mut self) -> Option<Self::Item> {
    (self.0).next().map(|(id, stuff)| (id.clone(), (&stuff.0, &stuff.1)))
  }
  fn size_hint(&self) -> (usize, Option<usize>) {
    self.0.size_hint()
  }
}
impl<'a, B: Basics> IntoIterator for &'a FiatSnapshot<B> {
  type Item = (FieldId, (&'a FieldRc, &'a ExtendedTime<B>));
  type IntoIter = FiatSnapshotIter<'a, B>;
  fn into_iter(self) -> Self::IntoIter {
    FiatSnapshotIter(self.fields.iter())
  }
}


pub fn serialize_snapshot<'a, B: Basics, Shot: Snapshot<Basics = B>, W: Any + Write>
  (snapshot: &'a Shot,
   writer: &mut W,
   size_limit: bincode::SizeLimit)
   -> bincode::serde::SerializeResult<()>
  where &'a Shot: IntoIterator<Item = SnapshotEntry<'a, B>>
{
  use bincode::serde::serialize_into;
  try! (serialize_into (writer, snapshot.now(), size_limit));
  try! (serialize_into (writer, snapshot.constants(), size_limit));
  try! (serialize_into (writer, &snapshot.num_fields(), size_limit));
  for (id, (data, changed)) in snapshot {
    try! (serialize_into (writer, &id, size_limit));
    try! (::implementation_support::common::serialize_field::<B, W> (id.column_id, writer, data, size_limit));
    try! (serialize_into (writer, changed, size_limit));
  }
  Ok(())
}

pub fn deserialize_snapshot<B: Basics, R: Any + Read>
  (reader: &mut R,
   size_limit: bincode::SizeLimit)
   -> bincode::serde::DeserializeResult<FiatSnapshot<B>> {
  use bincode::serde::deserialize_from;
  let now = try! (deserialize_from (reader, size_limit));
  let constants = try! (deserialize_from(reader, size_limit));
  let num_fields = try! (deserialize_from::<R, usize> (reader, size_limit));
  println! ("{}", num_fields);
  let mut fields = HashMap::new();
  for _ in 0..num_fields {
    let id: FieldId = try! (deserialize_from (reader, size_limit));
    let field =
      try! (::implementation_support::common::deserialize_field::<B, R> (id.column_id, reader, size_limit));
    let changed = try! (deserialize_from(reader, size_limit));
    println! ("{:?}", (id, (&field, &changed)));
    fields.insert(id, (field, changed));
  }
  Ok(FiatSnapshot {
    now: now,
    constants: constants,
    fields: fields,
  })
}



#[derive (Copy, Clone, PartialEq, Eq, Debug)]
pub enum FiatEventOperationError {
  InvalidInput,
  InvalidTime,
}

// This exists to support a variety of time stewards
// along with allowing BaseTime to be dense (e.g. a
// rational number rather than an integer).
// It is an acceptable peculiarity that even for integer times,
// After(2) < Before(3).
// #[derive (Copy, Clone, PartialEq, Eq, Hash)]
#[derive (Clone, PartialEq, Eq, Hash, Serialize, Deserialize, Debug)]
pub enum ValidSince<BaseTime> {
  TheBeginning,
  Before(BaseTime),
  After(BaseTime),
}
impl<B: fmt::Display> fmt::Display for ValidSince<B> {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    match self {
      &ValidSince::TheBeginning => write!(f, "TheBeginning"),
      &ValidSince::Before(ref something) => write!(f, "Before({})", something),
      &ValidSince::After(ref something) => write!(f, "After({})", something),
    }
  }
}

impl<T: Ord> Ord for ValidSince<T> {
  fn cmp(&self, other: &Self) -> Ordering {
    match (self, other) {
      (&ValidSince::TheBeginning, &ValidSince::TheBeginning) => Ordering::Equal,
      (&ValidSince::TheBeginning, _) => Ordering::Less,
      (_, &ValidSince::TheBeginning) => Ordering::Greater,
      (&ValidSince::Before(ref something), &ValidSince::Before(ref anything)) => {
        something.cmp(anything)
      }
      (&ValidSince::After(ref something), &ValidSince::After(ref anything)) => {
        something.cmp(anything)
      }
      (&ValidSince::Before(ref something), &ValidSince::After(ref anything)) => {
        if something <= anything {
          Ordering::Less
        } else {
          Ordering::Greater
        }
      }
      (&ValidSince::After(ref something), &ValidSince::Before(ref anything)) => {
        if something < anything {
          Ordering::Less
        } else {
          Ordering::Greater
        }
      }
    }
  }
}
impl<T> PartialEq<T> for ValidSince<T> {
  fn eq(&self, _: &T) -> bool {
    false
  }
}

impl<T: Ord> PartialOrd<T> for ValidSince<T> {
  fn partial_cmp(&self, other: &T) -> Option<Ordering> {
    Some(match self {
      &ValidSince::TheBeginning => Ordering::Less,
      &ValidSince::Before(ref something) => {
        if something <= other {
          Ordering::Less
        } else {
          Ordering::Greater
        }
      }
      &ValidSince::After(ref something) => {
        if something < other {
          Ordering::Less
        } else {
          Ordering::Greater
        }
      }
    })
  }
}
impl<T: Ord> PartialOrd for ValidSince<T> {
  fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
    Some(self.cmp(other))
  }
}
// impl <T: Ord> PartialOrd <ValidSince <T>> for T {
//  fn partial_cmp (&self, other: & ValidSince <T>)->Option <Ordering> {
//    Some (other.partial_cmp (self).unwrap().reverse());
//  }
// }

/// The core trait for the TimeSteward simulation interface.
pub trait TimeSteward: Any {
  type Basics: Basics;
  type Snapshot: Snapshot<Basics = Self::Basics>;

  /**
  You are allowed to call snapshot_before(), insert_fiat_event(),
  and remove_fiat_event() for times >= valid_since().
  
  TimeSteward implementors are permitted, but not required, to discard old data in order to save memory. This may make the TimeSteward unusable at some points in its history.
  
  All implementors must obey certain restrictions on how other TimeSteward methods may change the result of valid_since(). Implementors may have their own methods that can alter this in customized ways, which should be documented with those individual methods.
  

  */
  fn valid_since(&self) -> ValidSince<<<Self as TimeSteward>::Basics as Basics>::Time>;

  /**
  Inserts a fiat event at some point in the history.
  
  If time < valid_since(), this does nothing and returns Err(InvalidTime). If there is already a fiat event with the same time and distinguisher, this does nothing and returns Err(InvalidInput). Otherwise, it inserts the event and returns Ok.
  
  steward.insert_fiat_event(time, _) must not return InvalidTime if time > steward.valid_since().
  steward.insert_fiat_event() may not change steward.valid_since().
  */
  fn insert_fiat_event<E: Event<Basics = Self::Basics>>(&mut self,
                                      time: <<Self as TimeSteward>::Basics as Basics>::Time,
                                      id: DeterministicRandomId,
                                      event: E)
                                      -> Result<(), FiatEventOperationError>;

  /**
  Removes a fiat event that has been inserted previously.
  
  If time < valid_since(), this does nothing and returns Err(InvalidTime). If there is no fiat event with the specified time and distinguisher, this does nothing and returns Err(InvalidInput). Otherwise, it removes the event and returns Ok.
  
  steward.remove_fiat_event(time, _) must not return InvalidTime if time > steward.valid_since().
  steward.remove_fiat_event() may not change steward.valid_since().
  */
  fn remove_fiat_event(&mut self,
                      time: &<<Self as TimeSteward>::Basics as Basics>::Time,
                      id: DeterministicRandomId)
                      -> Result<(), FiatEventOperationError>;

  /** Returns a "snapshot" into the TimeSteward.
  
  The snapshot is guaranteed to be valid and unchanging for the full lifetime of the TimeSteward. It is specific to both the time argument, and the current collection of fiat events. Callers may freely call mutable methods of the same TimeSteward after taking a snapshot, without changing the contents of the snapshot.
  
  Each TimeSteward implementor determines exactly how to provide these guarantees. Implementors should provide individual guarantees about the processor-time bounds of snapshot operations.
  
  steward.snapshot_before(time) must return Some if time > steward.valid_since().
  steward.snapshot_before(time) may not increase steward.valid_since() beyond Before(time).
  */
  fn snapshot_before(&mut self, time: &<<Self as TimeSteward>::Basics as Basics>::Time) -> Option<Self::Snapshot>;
}

/// A TimeSteward that can be constructed empty, given only the simulation constants.
pub trait TimeStewardFromConstants: TimeSteward {
  /**
  Creates a new, empty TimeSteward.
  
  from_constants().valid_since() must equal TheBeginning.
  */
  fn from_constants(constants: <<Self as TimeSteward>::Basics as Basics>::Constants) -> Self;
}

/// A TimeSteward that can be constructed from only a snapshot.
pub trait TimeStewardFromSnapshot: TimeSteward {
  /**
  Creates a new TimeSteward from a snapshot.
  
  from_snapshot(snapshot).valid_since() must equal Before(snapshot.now()),
  and must never go lower than that.
  */
  fn from_snapshot<'a, S: Snapshot<Basics = Self::Basics>>(snapshot: &'a S) -> Self
    where &'a S: IntoIterator<Item = SnapshotEntry<'a, Self::Basics>>;
}

/// A TimeSteward that can be instructed to do a small amount of computation at a time.
///
/// This can be useful to avoid blocking the UI in single-threaded simulations.
pub trait IncrementalTimeSteward: TimeSteward {
  /// Does a single chunk of computation.
  ///
  /// The cost of this should generally be O(1). It may involve a callback
  /// to a Predictor or Event, so it may be a more expensive operation if
  /// you have very expensive individual Predictors or Events.
  /// 
  /// steward.step() may increase steward.valid_since() up to After(steward.updated_until_before()),
  /// but not farther than that.
  ///
  /// If steward.valid since() is currently greater than steward.updated_until_before(), steward.step() may not increase steward.valid_since() at all. Thus, you can be assured of eventually being able to take out a snapshot inexpensively.
  fn step(&mut self);
  
  /// Returns the latest time for which the TimeSteward can provide a Snapshot immediately.
  ///
  /// steward.updated_until_before() is NOT necessarily guaranteed to be later than steward.valid_since().
  /// A flat TimeSteward could be in the middle of processing a moment,
  /// such that the beginning of the moment is already out of date,
  /// but the end of the moment is not yet available.
  /// However, for a FullTimeSteward constructed by from_constants() or from_snapshot(),
  /// this will always return a valid time when a Snapshot can be taken.
  fn updated_until_before(&self) -> Option<<<Self as TimeSteward>::Basics as Basics>::Time>;
}

use std::collections::BTreeMap;

/// A protocol used by stewards::simply_synchronized.
///
/// The current protocol only supports synchronizing two clients at a time, and
/// has no resilience against malicious input. It will likely be replaced with something
/// more refined, so it should be considered unstable.
pub trait SimpleSynchronizableTimeSteward: TimeStewardFromConstants + FullTimeSteward {
  fn begin_checks (&mut self, start: <<Self as TimeSteward>::Basics as Basics>::Time, stride: <<Self as TimeSteward>::Basics as Basics>::Time);
  fn checksum(&mut self, chunk: i64) -> u64;
  fn debug_dump(&self, chunk: i64) -> BTreeMap<(ExtendedTime<<Self as TimeSteward>::Basics>, PredictorId, RowId), u64>;
  fn callback_details(&self, time: &ExtendedTime<<Self as TimeSteward>::Basics>, predictor_id: PredictorId, row_id: RowId) -> String;
}

/// A marker trait indicating that the TimeSteward promises that calling snapshot_before() or step() will not change valid_since()
pub trait FullTimeSteward: TimeSteward {}

/// A marker trait. Every CanonicalTimeSteward implementor must behave exactly the same way.
///
/// That is, given any Steward0: CanonicalTimeSteward and Steward1: CanonicalTimeSteward,
/// and any sequence of inputs that is valid for both Steward0 and Steward1,
/// all outputs of from_snapshot() must be identical.
///
/// A TimeSteward that does not implement CanonicalTimeSteward may behave differently.
/// For example, simply_synchronized::Steward transforms its inputs and includes input
/// from the remote client, without that input being explicitly passed to it by the local client.
pub trait CanonicalTimeSteward: TimeSteward {}

#[cfg (test)]
mod tests {
  use super::*;
  use serde::Serialize;
  use std::fmt::Debug;

  fn test_id_endianness_impl<T: Serialize + Debug>(thing: T, confirm: DeterministicRandomId) {
    println!("DeterministicRandomId::new({:?}) = {:?}", thing, DeterministicRandomId::new(& thing));
    assert_eq! (DeterministicRandomId::new(& thing), confirm);
  }

  #[test]
  fn test_id_endianness() {
    test_id_endianness_impl((),
                            DeterministicRandomId {
                              data: [18033283813966546569, 10131395250899649866],
                            });
    test_id_endianness_impl(1337,
                            DeterministicRandomId {
                              data: [3453333590764588377, 1257515737963236726],
                            });
    let a: (Option<Option<i32>>,) = (Some(None),);
    test_id_endianness_impl(a,
                            DeterministicRandomId {
                              data: [16808472249412258235, 2826611911447572457],
                            });
    test_id_endianness_impl(DeterministicRandomId::new(&0x70f7b85b08ba4fd5u64),
                            DeterministicRandomId {
                              data: [12393903562314107346, 11644372085838480024],
                            });
  }
}
