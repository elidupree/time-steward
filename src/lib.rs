/*
Future TimeSteward API goals:

– groups 
  I made up API functions for this before, but I'm not sure they were perfect.
  We can use RowId's for group ids, but what kinds of things can be stored IN a group? Previously I said only RowId's could, which doesn't seem ideal. I think it would work to allow anything hashable. That would basically make a group behave like a HashSet. But then, why not make it behave like a HashMap instead? But accessor itself already behaves like a HashMap over RowId's – the essential thing groups do is to allow iterating particular subsets of that HashMap.

– conveniences for serializing snapshots (not sure what)

– be able to construct a new TimeSteward from any snapshot
  Issues: should snapshots expose the predictors? Also, the predictors can't be serialized. So you'd probably actually have to construct a TimeSteward from snapshot + predictors. This requires a way to iterate all fields (and group data if we add groups) in a snapshot, which is similar to the previous 2 items (iterating the whole set, which is a special case of iterating a subset; and serializing requires you to iterate everything as well)

– parallelism support for predictors and events
  When an event or predictor gets invalidated while it is still running, it would be nice for it to save time by exiting early.
  Moreover, it would probably be more efficient to discard invalidated fields than to preserve them for predictors/events that are in process. The natural way for the accessors to handle this is to have get() return None, which would mean that you can never safely unwrap() the result. We could provide a "unwrap or return" macro.
  
– Optimization features
  one possibility: user can provide a function FieldId->[(PredictorId, RowId)] that lists predictors you KNOW will be invalidated by a change to that field, then have that predictor run its get() calls with an input called "promise_inferred" or something so that we don't spend time and memory recording the dependency
  another: a predictor might have a costly computation to find the exact time of a future event, which it won't need to do if it gets invalidated long before that time comes. For that, we can provide a defer_until(time) method
  

*/

extern crate rand;

use std::hash::{Hash, Hasher, SipHasher};
use std::any::Any;
use std::rc::Rc;
use std::cmp::Ordering;
use std::fmt;
use rand::{ChaChaRng, SeedableRng};

mod insert_only;

// TODO check whether hashes in rust vary by CPU endianness?
// Answer: they do, so maybe stop using Hash for this sometime
// ( https://doc.rust-lang.org/std/hash/trait.Hasher.html
// "represents the ability to hash an arbitrary stream of bytes").
#[derive (Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
pub struct DeterministicRandomId {
  data: [u64; 2],
}
pub struct SiphashIdGenerator {
  data: [SipHasher; 2],
}
impl Hasher for SiphashIdGenerator {
  fn finish(&self) -> u64 {
    panic!()
  }//Hack: this is actually for generating DeterministicRandomId, not 64-bit
  fn write(&mut self, bytes: &[u8]) {
    self.data[0].write(bytes);
    self.data[1].write(bytes);
  }
  // TODO: should we implement the others for efficiency?
}
impl SiphashIdGenerator {
  fn generate(&self) -> DeterministicRandomId {
    DeterministicRandomId { data: [self.data[0].finish(), self.data[1].finish()] }
  }
  fn new() -> SiphashIdGenerator {
    SiphashIdGenerator {
      data: [SipHasher::new_with_keys(0xb82a9426fd1a574f, 0x9d9d5b703dcb1bcc),
             SipHasher::new_with_keys(0x03e0d6037ff980a4, 0x65b790a0825b83bd)],
    }
  }
}
impl DeterministicRandomId {
  pub fn new<T: Hash>(data: &T) -> DeterministicRandomId {
    let mut s = SiphashIdGenerator::new();
    data.hash(&mut s);
    s.generate()
  }
}
impl fmt::Display for DeterministicRandomId {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    write!(f, "id:{:016x}{:016x}", self.data[0], self.data[1])
  }
}


// TODO: Shouldn't "ordered type with minimum and maximum values" be a trait common to other situations as well? Find out if there's a standard name for that.
// pub trait BaseTime: Clone + Ord {
//   fn min_time() -> Self;
//   fn max_time() -> Self;
// }
// impl BaseTime for i64 {
//   fn min_time() -> i64 {
//     i64::min_value()
//   }
//   fn max_time() -> i64 {
//     i64::max_value()
//   }
// }


// Database analogy: time stewards kind of contain a database table.
// Rows' ids are special; there isn't an "id" column.
// Row ids are random 128 bit ids.
// Rows aren't explicitly inserted and deleted.
// Only the existence or nonexistence of a (row, column) pair
// -- a field -- is meaningful.
// Generally, space usage is proportional to the number of
// existent fields.  For example, a row with only one existent field
// only takes up the space of one field.

pub type RowId = DeterministicRandomId;
type TimeId = DeterministicRandomId;

#[derive (Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
pub struct ColumnId(u64);

#[derive (Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
pub struct PredictorId(u64);


pub trait Column {
  type FieldType: Any;// = Self;

  /**
  Returns a constant identifier for the type, which must be 64 bits of random data.
  
  Thanks to the [birthday problem](https://en.wikipedia.org/wiki/Birthday_problem), this would have a >1% chance of a collision with a mere 700 million Column implementors. I don't think we need to worry about this. 128 bit IDs are necessary for rows, because computers can generate billions of them easily, but this isn't the same situation.
  
  It might seem desirable to default to a hash of the TypeId of Self, for the convenience of some implementations. However, Rust does not guarantee that the TypeId remains the same across different compilations or different compiler versions. And it certainly doesn't remain the same when you add or remove types from your code, which would be desirable for compatibility between different versions of your program. Also, the Rust interface for getting the TypeId is currently unstable. Using an explicit constant is simply better.
  
  TODO: change this into an associated constant once associated constants become stable.
  */
  fn column_id() -> ColumnId;

  //   /**
  //   Implementors MAY return true if first and second are indistinguishable.
  //
  //   This is labeled "unsafe" because imperfect implementations can easily cause nondeterminism in the TimeSteward. Using the default is fine, and implementing it is only an optimization. Don't implement this unless you know what you're doing.
  //
  //   This is similar to requiring FieldType to be PartialEq, but with an important difference. PartialEq only requires the comparison to be an equivalence relation, but does NOT require that (first == second) guarantee that there is no observable difference between the values. Therefore, trusting arbitrary implementations of PartialEq would be unsafe (in the sense that it would allow the TimeSteward to behave non-deterministically).
  //
  //   TODO: perhaps we can create default implementations of this for POD types.
  //   TODO: can we create automated tests for implementations of this function?
  //   */
  //   fn guaranteed_equal__unsafe(first: &Self::FieldType, second: &Self::FieldType) -> bool {
  //     false
  //   }
}

#[derive (Copy, Clone, PartialEq, Eq, Hash)]
struct FieldId {
  row_id: RowId,
  column_id: ColumnId,
}

type IterationType = u32;
#[derive (Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
struct GenericExtendedTime<Base: Ord> {
  base: Base,
  iteration: IterationType,
  id: TimeId,
}

// fn beginning_of_moment<Base: Ord>(base_time: Base) -> GenericExtendedTime<Base> {
//  GenericExtendedTime {
//    base: base_time,
//    iteration: 0,
//    id: TimeId { data: [0, 0] },
//  }
// }

fn extended_time_of_fiat_event<BaseTime: Ord>(time: BaseTime,
                                              id: TimeId)
                                              -> GenericExtendedTime<BaseTime> {
  GenericExtendedTime {
    base: time,
    iteration: 0,
    id: id,
  }
}

fn time_id_for_predicted_event(predictor_id: PredictorId,
                               row_id: RowId,
                               iteration: IterationType,
                               dependencies_hash: DeterministicRandomId)
                               -> TimeId {
  TimeId::new(&(predictor_id, row_id, iteration, dependencies_hash))
}
fn next_extended_time_of_predicted_event<BaseTime: Ord>
  (predictor_id: PredictorId,
   row_id: RowId,
   dependencies_hash: DeterministicRandomId,
   event_base_time: BaseTime,
   from: &GenericExtendedTime<BaseTime>)
   -> Option<GenericExtendedTime<BaseTime>> {
  let (iteration, id) = match event_base_time.cmp(&from.base) {
    Ordering::Less => return None, // short-circuit
    Ordering::Greater => {
      (0, time_id_for_predicted_event(predictor_id, row_id, 0, dependencies_hash))
    }
    Ordering::Equal => {
      let id = time_id_for_predicted_event(predictor_id, row_id, from.iteration, dependencies_hash);
      if id > from.id {
        (from.iteration, id)
      } else {
        if from.iteration == IterationType::max_value() {
          panic!("Too many iterations at the same base time; probably an infinite loop")
        }
        (from.iteration + 1,
         time_id_for_predicted_event(predictor_id, row_id, from.iteration + 1, dependencies_hash))
      }
    }
  };
  Some(GenericExtendedTime {
    base: event_base_time,
    iteration: iteration,
    id: id,
  })
}

pub type EventRng = ChaChaRng;

fn generator_for_event(id: TimeId) -> EventRng {
  EventRng::from_seed(&[(id.data[0] >> 32) as u32,
                        (id.data[0] & 0xffffffff) as u32,
                        (id.data[1] >> 32) as u32,
                        (id.data[1] & 0xffffffff) as u32])
}

/**
This is intended to be implemented on an empty struct. Requiring Clone is a hack to work around [a compiler weakness](https://github.com/rust-lang/rust/issues/26925).
*/
pub trait Basics: Clone + 'static {
  type Time: Clone + Ord;
  type Constants;
}
type ExtendedTime<B: Basics> = GenericExtendedTime<B::Time>;

// Note: in the future, we expect we might use a custom hash table type that knows it can rely on DeterministicRandomId to already be random, so we don't need to hash it again. This also applies to FieldId, although there may be some complications in that case.
// (for now, we need to be able to hash full siphash ids to create other siphash ids.)
// ( Also -- do we ever try to hash GenericExtendedTime with id of 0 from beginning_of_moment.... )
// impl Hash for DeterministicRandomId {
//  fn hash<H: Hasher>(&self, state: &mut H) {
//    self.data[0].hash(state);
//  }
// }
// impl<Base: BaseTime> Hash for GenericExtendedTime<Base> {
//  fn hash<H: Hasher>(&self, state: &mut H) {
//    self.id.hash(state);
//  }
// }

pub trait Accessor<B: Basics> {
  fn data_and_last_change<C: Column>(&mut self, id: RowId) -> Option<(&C::FieldType, & B::Time)>;
  fn get<C: Column>(&mut self, id: RowId) -> Option<&C::FieldType> {self.data_and_last_change::<C> (id).map (|p| p.0)}
  fn last_change <C: Column>(&mut self, id: RowId) -> Option<& B::Time> {self.data_and_last_change::<C> (id).map (|p| p.1)}
  fn constants(&self) -> &B::Constants;
}

pub trait MomentaryAccessor<B: Basics>: Accessor<B> {
  fn now(&self) -> &B::Time;
}


pub trait Mutator<B: Basics>: MomentaryAccessor<B> {
  // fn get_mut<C: Column>(&mut self, id: RowId) -> Option<&mut C::FieldType> where C::FieldType: Clone;
  fn set<C: Column>(&mut self, id: RowId, data: Option<C::FieldType>);
  fn rng(&mut self) -> &mut EventRng;
  fn random_id(&mut self) -> RowId;
}
pub trait PredictorAccessor<B: Basics, EventFn:?Sized>: Accessor<B> {
  // it is unclear whether predict_immediately is sketchy
  fn predict_immediately(&mut self, event: Rc<EventFn>);
  fn predict_at_time(&mut self, time: &B::Time, event: Rc<EventFn>);
}
pub trait Snapshot<B: Basics>: MomentaryAccessor<B> {}


#[derive (Copy, Clone, PartialEq, Eq)]
pub enum FiatEventOperationResult {
  Success,
  InvalidInput,
  InvalidTime,
}

pub enum ValidSince<BaseTime> {
  TheBeginning,
  Before(BaseTime),
  After(BaseTime),
}

// This exists to support a variety of time stewards
// along with allowing BaseTime to be dense (e.g. a
// rational number rather than an integer).
// It is an acceptable peculiarity that even for integer times,
// After(2) < Before(3).
// #[derive (Copy, Clone, PartialEq, Eq, Hash)]
// pub enum TimeBoundary<BaseTime> {
//  Before(BaseTime),
//  After(BaseTime),
// }
// //impl PartialOrd

/// TimeStewardLifetimedMethods is a hack to make it possible
/// for trait TimeSteward to have associated types that are
/// parameterized by lifetime.
/// Example impl if you have types Steward<B> and Snapshot<'a, B>:
///
/// impl<'a, B: Basics> TimeStewardLifetimedMethods<'a, B> for Steward<B> {
///   type Snapshot = Snapshot<'a, B>;
///   fn snapshot_before(&'a mut self, time: &B::Time) -> Option<Self::Snapshot> {
///     // your implementation
///   }
/// }
pub trait TimeStewardLifetimedMethods<'a, B: Basics>: TimeStewardStaticMethods <B> {
  type Mutator: Mutator <B> + 'a;
  type PredictorAccessor: PredictorAccessor <B, <Self as TimeStewardStaticMethods <B>>::EventFn> + 'a;
}
pub trait TimeStewardStaticMethods <B: Basics>: 'static {
  type EventFn:?Sized;
  type PredictorFn:?Sized;
  type Snapshot: Snapshot <B>;

  /**
  You are allowed to call snapshot_before(), insert_fiat_event(),
  and erase_fiat_event() for times >= valid_since().
  
  TimeSteward implementors are permitted, but not required, to discard old data in order to save memory. This may make the TimeSteward unusable at some points in its history.
  
  All implementors must obey certain restrictions on how other TimeSteward methods may change the result of valid_since(). Implementors may have their own methods that can alter this in customized ways, which should be documented with those individual methods.
  */
  fn valid_since(&self) -> ValidSince<B::Time>;

  /**
  Creates a new, empty TimeSteward.
  
  new_empty().valid_since() must equal TheBeginning.
  */
  fn new_empty(constants: B::Constants, predictors: Vec<Predictor <Self::PredictorFn>>) -> Self;

  /**
  Inserts a fiat event at some point in the history.
  
  If time < valid_since(), this does nothing and returns InvalidTime. If there is already a fiat event with the same time and distinguisher, this does nothing and returns InvalidInput. Otherwise, it inserts the event and returns Success.
  
  steward.insert_fiat_event(time, _) must not return InvalidTime if time > steward.valid_since().
  steward.insert_fiat_event() may not change steward.valid_since().
  */
  fn insert_fiat_event(&mut self,
                       time: B::Time,
                       id: DeterministicRandomId,
                       event: Rc<Self::EventFn>)
                       -> FiatEventOperationResult;

  /**
  Erases a fiat event that has been inserted previously.
  
  If time < valid_since(), this does nothing and returns InvalidTime. If there is no fiat event with the specified time and distinguisher, this does nothing and returns InvalidInput. Otherwise, it erases the event and returns Success.
  
  steward.erase_fiat_event(time, _) must not return InvalidTime if time > steward.valid_since().
  steward.erase_fiat_event() may not change steward.valid_since().
  */
  fn erase_fiat_event(&mut self,
                      time: &B::Time,
                      id: DeterministicRandomId)
                      -> FiatEventOperationResult;

  /** Returns a "snapshot" into the TimeSteward.
  
  The snapshot is guaranteed to be valid and unchanging for the full lifetime of the TimeSteward. It is specific to both the time argument, and the current collection of fiat events. Callers may freely call mutable methods of the same TimeSteward after taking a snapshot, without changing the contents of the snapshot.
  
  Each TimeSteward implementor determines exactly how to provide these guarantees. Implementors should provide individual guarantees about the processor-time bounds of snapshot operations.
  
  steward.snapshot_before(time) must return Some if time > steward.valid_since().
  steward.snapshot_before(time) may not increase steward.valid_since() beyond time.
  
  note: we implement "before" and not "after" because we might be banning events that happen during max_time
  */
  fn snapshot_before <'b> (& 'b mut self, time: & 'b B::Time) -> Option<Self::Snapshot>;
}
pub trait TimeSteward <B: Basics>: for<'a> TimeStewardLifetimedMethods<'a, B> {}



// #[derive (Clone)]
// enum Prediction<B: Basics, E> {
//   Nothing,
//   Immediately(E),
//   At(B::Time, E),
// }

//#[derive (Clone)]
pub struct Predictor<PredictorFn:?Sized> {
  predictor_id: PredictorId,
  column_id: ColumnId,
  function: Rc<PredictorFn>,
}
//explicitly implement Clone to work around [a compiler weakness](https://github.com/rust-lang/rust/issues/26925).
impl<PredictorFn:?Sized>  Clone for Predictor <PredictorFn> {
fn clone (&self)->Self {Predictor {predictor_id: self.predictor_id, column_id: self.column_id, function: self.function.clone()}}
}

// type Event<M> = Rc<for<'d> Fn(&'d mut M)>;
// type PredictorFn<B: Basics, M: Mutator<B>, PA: PredictorAccessor<B>> =
//  Rc<for<'b, 'c> Fn(&'b mut PA, RowId) -> Prediction<B, Event<M>>>;


pub mod inefficient_flat_time_steward;
pub mod memoized_flat_time_steward;

//pub mod crossverified_time_stewards;

pub mod collision_detection;

pub mod examples {
  pub mod handshakes;
  //pub mod bouncy_circles;
}

#[test]
fn it_works() {
  // panic!("works!!!!!!!!!!!");
}
