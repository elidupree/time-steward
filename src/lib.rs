
use std::hash::{Hash, Hasher,SipHasher};
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

#[derive (Clone, PartialEq, Eq, Hash)]
struct FieldID {
  entity: SiphashID,
  field: u64,
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

//Note: in the future, we expect we might use a custom hash table type that knows it can rely on SiphashID to already be random, so we don't need to hash it again. This also applies to FieldID, although there may be some complications in that case.
impl Hash for SiphashID {
  fn hash <H: Hasher> (& self, state: &mut H) {self.data [0].hash (state);}
}
impl <Base: BaseTime> Hash for GenericExtendedTime <Base>{
  fn hash <H: Hasher> (& self, state: &mut H) {self.id.hash (state);}
}

pub trait Accessor <B: Basics> {
  fn get <F: Field> (&mut self, id: SiphashID)-> Option <& F::Data>;
  fn constants (& self)->& B::Constants;
}

pub trait Mutator <B: Basics>: Accessor <B> {
  fn get_mut <F: Field> (&mut self, id: SiphashID)->Option <&mut F::Data> where F::Data: Clone;
  fn set <F: Field> (&mut self, id: SiphashID, data: Option <F::Data>);
  fn random_bits (&mut self, num_bits:u32)->u64;
  fn random_id (&mut self)->SiphashID;
  fn now (& self)->B::Time;
}
pub trait PredictorAccessor <B: Basics>: Accessor <B> {

}

pub trait TimeSteward <'a, B: Basics> {
  type A: Accessor <B>;
  type M: Mutator <B>;
  type P: PredictorAccessor <B>;
  type Event;
  type Prediction;
  type Predictor;
  fn insert_fiat_event (&mut self, time: B::Time, distinguisher: u64, event: Self::Event)->bool;
  fn erase_fiat_event (&mut self, time: B::Time, distinguisher: u64)->bool;
  //note: "before" only because we might be banning events that happen during max_time
  fn snapshot_before (&mut self, time: B::Time)->Self::A;
}

pub trait FlatTimeSteward <'a, B: Basics>:TimeSteward <'a, B> {
  fn valid_strictly_between (& self)->(B::Time, B::Time);
}


type Event <B: Basics, M: Mutator <B>> =Rc <Fn (&mut M)>;
enum Prediction <B: Basics, M: Mutator <B>> {
  Nothing,
  Immediately (Event <B, M>),
  At (B::Time, Event <B, M>),
}
type Predictor <B: Basics, M: Mutator <B>, PA: PredictorAccessor <B>> =Rc <for <'b, 'c> Fn (& 'b mut PA)->Prediction <B, M>>;



mod StandardFlatTimeSteward {
use super::{SiphashID, FieldID, Field, ExtendedTime, Basics, TimeSteward};
use std::collections::{HashMap, BinaryHeap};
use std::any::Any;
use std::borrow::Borrow;
use std::rc::Rc;


pub struct Steward <'a, B: Basics> {
  entity_states: HashMap< FieldID, Box <Any>>,
  last_change: B::Time,
  fiat_events: HashMap< ExtendedTime <B>, Event <'a, B>>,
  predictions: HashMap<ExtendedTime <B>, Prediction <'a, B>>,
  upcoming_event_times: BinaryHeap <ExtendedTime <B>>,
    
}
pub struct Accessor <'a, B: Basics + 'a> {
  steward: &'a Steward <'a, B>,
}
pub struct Mutator <'a, B: Basics + 'a> {
  steward: & 'a mut Steward <'a, B>,
}
pub struct PredictorAccessor <'a, B: Basics + 'a> {
  steward: & 'a mut Steward <'a, B>,
}
type Event <'a, B: Basics + 'a> = super::Event <B, Mutator <'a, B>>;
type Prediction <'a, B: Basics + 'a> = super::Prediction <B, Mutator <'a, B>>;
type Predictor <'a, B: Basics + 'a> = super::Predictor <B, Mutator <'a, B>, PredictorAccessor <'a, B>>;


impl <'a, B: Basics>Steward <'a, B> {
  fn new (constants: B::Constants, predictors: Vec<Predictor <'a, B>>)->Self {}
}
impl <'a, B: Basics> super::Accessor <B> for Accessor <'a, B> {
  fn get <F: Field> (&mut self, id: SiphashID)-> Option <& F::Data> {
    self.steward.entity_states.get (& FieldID {entity: id, field: F::unique_identifier ()}).map (| something | something.downcast ::<F::Data> ().unwrap ().borrow ())
  }
}
impl <'a, B: Basics> super::Accessor <B> for Mutator <'a, B> {
  fn get <F: Field> (&mut self, id: SiphashID)-> Option <& F::Data> {
    self.steward.entity_states.get (& FieldID {entity: id, field: F::unique_identifier ()}).map (| something | something.downcast ::<F::Data> ().unwrap ().borrow ())
  }
}

impl <'a, B: Basics> super::TimeSteward <'a, B> for Steward <'a, B> {
  type A =Accessor <'a, B>;
  type M =Mutator <'a, B>;
  type P =PredictorAccessor <'a, B>;
    
  type Event = Event <'a, B>;
  type Prediction = super::Prediction <B, Self::Event>;
  type Predictor = super::Predictor <B, Self::P, Self::Prediction>;

  fn insert_fiat_event (&mut self, time: B::Time, distinguisher: u64, event: Self::Event)->bool {}
  fn erase_fiat_event (&mut self, time: B::Time, distinguisher: u64)->bool {}
  fn snapshot_before (&mut self, time: B::Time)->Self::A {}
}

}

 #[test]
fn it_works() {
}
