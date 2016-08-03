use std::collections::{HashMap, BinaryHeap};
use std::hash::{Hash, Hasher,SipHasher};
use std::any::Any;

#[derive (Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct SiphashID {
data: [u64; 2],
}
pub struct SiphashID_Generator {
data: [SipHasher; 2],
}
impl Hasher for SiphashID_Generator {
fn finish (& self)->u64 {panic!()}//Hack: this is actually for generating SiphashID, not 64-bit
fn write (&mut self, bytes: & [u8]) {self.data [0].write (bytes);self.data [1].write (bytes);}
//TODO: should we implement the others for efficiency?
}
impl SiphashID_Generator {
fn generate (& self)->SiphashID {SiphashID {data: [self.data [0].finish (), self.data [1].finish ()]}}
}

pub trait BaseTime: Ord {
  fn min_time ()->Self;
  fn max_time ()->Self;
}
impl BaseTime for i64 {
  fn min_time ()->i64 {i64::min_value ()}
  fn max_time ()->i64 {i64::max_value ()}
}

pub trait Field {
  type Data: Any;// = Self;
  fn unique_identifier ()->u64;
}

#[derive (Clone, PartialEq, Eq, PartialOrd, Ord)]
struct GenericExtendedTime <Base: BaseTime> {
  base: Base,
  iteration: u64,
  id: SiphashID,
}

pub trait Basics {
  type Time: BaseTime;
  type Constants;
}
type ExtendedTime <B: Basics> = GenericExtendedTime <B::Time>;

//Note: in the future, we expect we might use a custom hash table type that knows it can rely on SiphashID to already be random, so we don't need to hash it again.
impl Hash for SiphashID {
  fn hash <H: Hasher> (& self, state: &mut H) {self.data [0].hash (state);}
}
impl <Base: BaseTime> Hash for GenericExtendedTime <Base>{
  fn hash <H: Hasher> (& self, state: &mut H) {self.id.hash (state);}
}

pub trait Accessor {
  fn get <Type: Field> (&mut self, id: SiphashID)-> Option <& Type::Data>;
}

pub trait Mutator <B: Basics>: Accessor {
  fn get_mutable <Type: Field> (&mut self, id: SiphashID)->Option <&mut Type::Data>;
  fn set <Type: Field> (&mut self, id: SiphashID, data: Option <Type::Data>);
  fn random_bits (&mut self, bits:u32)->u64;
  fn random_id (&mut self)->SiphashID;
  fn now (& self)->B::Time;
  fn constants (& self)->& B::Constants;
}
pub trait PredictorAccessor <B: Basics>: Accessor {

}

pub type Event<B: Basics>  =Box <Fn (&mut Mutator <B>)>;
enum Prediction <B: Basics> {
  Nothing,
  Immediately (Event <B>),
  At (B::Time, Event <B>),
}

pub trait TimeSteward <B: Basics> {
  type A: Accessor;
  fn insert_fiat_event (&mut self, time: B::Time, distinguisher: u64, event: Event<B>)->bool;
  fn erase_fiat_event (&mut self, time: B::Time, distinguisher: u64)->bool;
  fn accessor_after (&mut self, time: B::Time)->A;
}

pub trait FlatTimeSteward <B: Basics>:TimeSteward <B> {
  fn valid_strictly_between (& self)->(B::Time, B::Time);
}

type StandardFlatTimeStewardPredictor <B: Basics> = Box <for <'a, 'b> Fn (& 'a mut StandardFlatTimeStewardPredictorAccessor <'b, B>)->Prediction <B>>;
pub struct StandardFlatTimeSteward <B: Basics> {
  entity_states: HashMap< SiphashID, Box <Any>>,
  last_change: B::Time,
  fiat_events: HashMap< ExtendedTime <B>, Event<B>>,
  predictions: HashMap<ExtendedTime <B>, Prediction<B>>,
  upcoming_event_times: BinaryHeap <ExtendedTime <B>>,
    
}
struct StandardFlatTimeStewardAccessor <'a, B: Basics + 'a> {
  steward: &'a StandardFlatTimeSteward <B>,
}
struct StandardFlatTimeStewardMutator <'a, B: Basics + 'a> {
  steward: & 'a mut StandardFlatTimeSteward <B>,
}
struct StandardFlatTimeStewardPredictorAccessor <'a, B: Basics + 'a> {
  steward: & 'a mut StandardFlatTimeSteward <B>,
}
impl <B: Basics>StandardFlatTimeSteward <B> {
  fn new (constants: B::Constants, predictors: Vec<StandardFlatTimeStewardPredictor <B> >)->Self {}
}
impl <'a, B: Basics> Accessor for StandardFlatTimeStewardAccessor <'a, B> {
  pub fn get <Type: Field> (&mut self, id: SiphashID)-> Option <& Type::Data> {
    self.steward.entity_states.get (& id).map (| something | something.downcast ::<Type::Data> ().unwrap ().borrow ())
  }
}
impl <'a, B: Basics> Accessor for StandardFlatTimeStewardMutator <'a, B> {
  pub fn get <Type: Field> (&mut self, id: SiphashID)-> Option <& Type::Data> {
    self.steward.entity_states.get (& id).map (| something | something.downcast ::<Type::Data> ().unwrap ().borrow ())
  }
}



impl <B: Basics> TimeSteward <B> for StandardFlatTimeSteward <B> {
  pub fn insert_fiat_event (&mut self, time: B::Time, distinguisher: u64, event: Event <B>)->bool {}
  pub fn erase_fiat_event (&mut self, time: B::Time, distinguisher: u64)->bool {}
  pub fn accessor_after (&mut self, time: B::Time)->Accessor {}
}



 #[test]
fn it_works() {
}
