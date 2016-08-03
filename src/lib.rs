use std::collections::{HashMap, BinaryHeap};
use std::hash::{Hash, Hasher,SipHasher};
use std::any::Any;
use std::borrow::Borrow;
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

pub trait TimeSteward <'a, B: Basics> {
  type A: Accessor;
  type M: Mutator <B>;
  type P: PredictorAccessor <B>;
  type Event;
  type Prediction;
  type Predictor;
  fn insert_fiat_event (&mut self, time: B::Time, distinguisher: u64, event: Self::Event)->bool;
  fn erase_fiat_event (&mut self, time: B::Time, distinguisher: u64)->bool;
  fn accessor_after (&mut self, time: B::Time)->Self::A;
}

pub trait FlatTimeSteward <'a, B: Basics>:TimeSteward <'a, B> {
  fn valid_strictly_between (& self)->(B::Time, B::Time);
}


type GenericEvent <B: Basics, M: Mutator <B>> =Rc <Fn (&mut M)>;
enum GenericPrediction <B: Basics, Event> {
  Nothing,
  Immediately (Event),
  At (B::Time, Event),
}






pub struct StandardFlatTimeSteward <'a, B: Basics> {
  entity_states: HashMap< FieldID, Box <Any>>,
  last_change: B::Time,
  //fiat_events: HashMap< ExtendedTime <B>, Self::Event>,
  //predictions: HashMap<ExtendedTime <B>, Prediction<B>>,
  upcoming_event_times: BinaryHeap <ExtendedTime <B>>,
    
}
pub struct StandardFlatTimeStewardAccessor <'a, B: Basics + 'a> {
  steward: &'a StandardFlatTimeSteward <'a, B>,
}
pub struct StandardFlatTimeStewardMutator <'a, B: Basics + 'a> {
  steward: & 'a mut StandardFlatTimeSteward <'a, B>,
}
pub struct StandardFlatTimeStewardPredictorAccessor <'a, B: Basics + 'a> {
  steward: & 'a mut StandardFlatTimeSteward <'a, B>,
}
impl <'a, B: Basics>StandardFlatTimeSteward <'a, B> {
  fn new (constants: B::Constants, predictors: Vec<Self::Predictor>)->Self {}
}
impl <'a, B: Basics> Accessor for StandardFlatTimeStewardAccessor <'a, B> {
  fn get <Type: Field> (&mut self, id: SiphashID)-> Option <& Type::Data> {
    self.steward.entity_states.get (& FieldID {entity: id, field: Type::unique_identifier ()}).map (| something | something.downcast ::<Type::Data> ().unwrap ().borrow ())
  }
}
impl <'a, B: Basics> Accessor for StandardFlatTimeStewardMutator <'a, B> {
  fn get <Type: Field> (&mut self, id: SiphashID)-> Option <& Type::Data> {
    self.steward.entity_states.get (& FieldID {entity: id, field: Type::unique_identifier ()}).map (| something | something.downcast ::<Type::Data> ().unwrap ().borrow ())
  }
}

impl <'a, B: Basics> TimeSteward <'a, B> for StandardFlatTimeSteward <'a, B> {
  type A =StandardFlatTimeStewardAccessor <'a, B>;
  type M =StandardFlatTimeStewardMutator <'a, B>;
    
  type P =StandardFlatTimeStewardPredictorAccessor <'a, B>;
    
  type Event = GenericEvent <B, Self::M>;
  type Prediction = GenericPrediction <B, Self::Event>;
  type Predictor =Rc <for <'b, 'c> Fn (& 'b mut StandardFlatTimeStewardPredictorAccessor <'c, B>)->Self::Prediction>;

  fn insert_fiat_event (&mut self, time: B::Time, distinguisher: u64, event: Self::Event)->bool {}
  fn erase_fiat_event (&mut self, time: B::Time, distinguisher: u64)->bool {}
  fn accessor_after (&mut self, time: B::Time)->Self::A {}
}



 #[test]
fn it_works() {
}
