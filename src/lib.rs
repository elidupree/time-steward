use std::hash::{Hash, Hasher, SipHasher};
use std::any::Any;
use std::rc::Rc;

#[derive (Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct SiphashID {
  data: [u64; 2],
}
pub struct SiphashID_Generator {
  data: [SipHasher; 2],
}
impl Hasher for SiphashID_Generator {
  fn finish(&self) -> u64 {
    panic!()
  }//Hack: this is actually for generating SiphashID, not 64-bit
  fn write(&mut self, bytes: &[u8]) {
    self.data[0].write(bytes);
    self.data[1].write(bytes);
  }
  // TODO: should we implement the others for efficiency?
}
impl SiphashID_Generator {
  fn generate(&self) -> SiphashID {
    SiphashID { data: [self.data[0].finish(), self.data[1].finish()] }
  }
}


// TODO: Shouldn't "ordered type with minimum and maximum values" be a trait common to other situations as well? Find out if there's a standard name for that.
pub trait BaseTime: Clone + Ord {
  fn min_time() -> Self;
  fn max_time() -> Self;
}
impl BaseTime for i64 {
  fn min_time() -> i64 {
    i64::min_value()
  }
  fn max_time() -> i64 {
    i64::max_value()
  }
}

pub trait Field {
  type Data: Any;// = Self;

  /**
  Returns a constant identifier for the type, which must be 64 bits of random data.
  
  Thanks to the [birthday problem](https://en.wikipedia.org/wiki/Birthday_problem), this would have a >1% chance of a collision with a mere 700 million Field implementors. I don't think we need to worry about this. 128 bit IDs are necessary for entities, because computers can generate billions of them easily, but this isn't the same situation.
  
  It might seem desirable to default to a hash of the TypeId of Self, for the convenience of some implementations. However, Rust does not guarantee that the TypeId remains the same across different compilations or different compiler versions. And it certainly doesn't remain the same when you add or remove types from your code, which would be desirable for compatibility between different versions of your program. Also, the Rust interface for getting the TypeId is currently unstable. Using an explicit constant is simply better.
  
  TODO: change this into an associated constant once associated constants become stable.
  */
  fn unique_identifier() -> u64;

  /**
  Implementors MAY return true if first and second are indistinguishable.
  
  This is labeled "unsafe" because imperfect implementations can easily cause nondeterminism in the TimeSteward. Using the default is fine, and implementing it is only an optimization. Don't implement this unless you know what you're doing.
  
  This is similar to requiring Data to be PartialEq, but with an important difference. PartialEq only requires the comparison to be an equivalence relation, but does NOT require that (first == second) guarantee that there is no observable difference between the values. Therefore, trusting arbitrary implementations of PartialEq would be unsafe (in the sense that it would allow the TimeSteward to behave non-deterministically).
  
  TODO: perhaps we can create default implementations of this for POD types.
  TODO: can we create automated tests for implementations of this function?
  */
  fn guaranteed_equal__unsafe(first: &Self::Data, second: &Self::Data) -> bool {
    false
  }
}

#[derive (Clone, PartialEq, Eq, Hash)]
struct FieldID {
  entity: SiphashID,
  field: u64,
}

#[derive (Clone, PartialEq, Eq, PartialOrd, Ord)]
struct GenericExtendedTime<Base: BaseTime> {
  base: Base,
  iteration: u64,
  id: SiphashID,
}

fn beginning_of_moment<Base: BaseTime>(base_time: Base) -> GenericExtendedTime<Base> {
  GenericExtendedTime {
    base: base_time,
    iteration: 0,
    id: SiphashID { data: [0, 0] },
  }
}

/**
This is intended to be implemented on an empty struct. Requiring Clone is a hack to work around [a compiler weakness](https://github.com/rust-lang/rust/issues/26925).
*/
pub trait Basics: Clone {
  type Time: BaseTime;
  type Constants;
}
type ExtendedTime<B: Basics> = GenericExtendedTime<B::Time>;

// Note: in the future, we expect we might use a custom hash table type that knows it can rely on SiphashID to already be random, so we don't need to hash it again. This also applies to FieldID, although there may be some complications in that case.
impl Hash for SiphashID {
  fn hash<H: Hasher>(&self, state: &mut H) {
    self.data[0].hash(state);
  }
}
impl<Base: BaseTime> Hash for GenericExtendedTime<Base> {
  fn hash<H: Hasher>(&self, state: &mut H) {
    self.id.hash(state);
  }
}

pub trait Accessor<B: Basics> {
  fn get<F: Field>(&mut self, id: SiphashID) -> Option<&F::Data>;
  fn constants(&self) -> &B::Constants;
}

pub trait MomentaryAccessor<B: Basics>: Accessor<B> {
  fn now(&self) -> &B::Time;
}


pub trait Mutator<B: Basics>: MomentaryAccessor<B> {
  fn get_mut<F: Field>(&mut self, id: SiphashID) -> Option<&mut F::Data> where F::Data: Clone;
  fn set<F: Field>(&mut self, id: SiphashID, data: Option<F::Data>);
  fn random_bits(&mut self, num_bits: u32) -> u64;
  fn random_id(&mut self) -> SiphashID;
}
pub trait PredictorAccessor<B: Basics>: Accessor<B> {}
pub trait Snapshot<B: Basics>: MomentaryAccessor<B> {}

pub enum FiatEventOperationResult {
  Success,
  InvalidInput,
  InvalidTime,
}

pub trait TimeSteward<'a, B: Basics> {
  type S: Snapshot<B>;
  type Event;

  /**
  TimeSteward implementors are permitted, but not required, to discard old data in order to save memory. This may make the TimeSteward unusable at some points in its history.
  
  Newly created stewards should generally have valid_from() == B::Time::min_time().
  
  All implementors must obey certain restrictions on how other TimeSteward methods may change the result of valid_from(). Implementors may have their own methods that can alter this in customized ways, which should be documented with those individual methods.
  */
  fn valid_from(&self) -> B::Time;

  /**
  Inserts a fiat event at some point in the history.
  
  If time < valid_from(), this does nothing and returns InvalidTime. If there is already a fiat event with the same time and distinguisher, this does nothing and returns InvalidInput. Otherwise, it inserts the event and returns Success.
  
  steward.insert_fiat_event(time, _) must not return InvalidTime if time >= steward.valid_from().
  steward.insert_fiat_event() may not change steward.valid_from().
  */
  fn insert_fiat_event(&mut self,
                       time: B::Time,
                       distinguisher: u64,
                       event: Self::Event)
                       -> FiatEventOperationResult;

  /**
  Erases a fiat event that has been inserted previously.
  
  If time < valid_from(), this does nothing and returns InvalidTime. If there is no fiat event with the specified time and distinguisher, this does nothing and returns InvalidInput. Otherwise, it erases the event and returns Success.
  
  steward.erase_fiat_event(time, _) must not return InvalidTime if time >= steward.valid_from().
  steward.erase_fiat_event() may not change steward.valid_from().
  */
  fn erase_fiat_event(&mut self, time: B::Time, distinguisher: u64) -> FiatEventOperationResult;

  /**
  Returns a "snapshot" into the TimeSteward.
  
  The snapshot is guaranteed to be valid and unchanging for the full lifetime of the TimeSteward. It is specific to both the time argument, and the current collection of fiat events. Callers may freely call mutable methods of the same TimeSteward after taking a snapshot, without changing the contents of the snapshot.
  
  Each TimeSteward implementor determines exactly how to provide these guarantees. Implementors should provide individual guarantees about the processor-time bounds of snapshot operations.
  
  steward.snapshot_before(time) must return Some if time >= steward.valid_from().
  steward.snapshot_before(time) may not increase steward.valid_from() beyond time.
  */
  // note: we implement "before" and not "after" because we might be banning events that happen during max_time
  fn snapshot_before(&mut self, time: B::Time) -> Option<Self::S>;
}

pub trait FlatTimeSteward<'a, B: Basics>: TimeSteward<'a, B> {
  fn valid_strictly_between(&self) -> (B::Time, B::Time);
}


type Event<M> = Rc<for<'d> Fn(&'d mut M)>;
#[derive (Clone)]
enum Prediction<B: Basics, E> {
  Nothing,
  Immediately(E),
  At(B::Time, E),
}
type Predictor<B: Basics, M: Mutator<B>, PA: PredictorAccessor<B>> =
  Rc<for<'b, 'c> Fn(&'b mut PA, SiphashID) -> Prediction<B, Event<M>>>;


mod inefficient_flat_time_steward;
// mod simple_flat_time_steward;

#[test]
fn it_works() {}
