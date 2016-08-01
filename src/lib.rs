
pub trait BaseTime {
  pub const Self MIN;
  pub const Self MAX;
  pub const Self NEVER;
}
impl BaseTime for i64 {
  
}

pub trait Field {
  type Data = Self;
  const unique_identifier: u64;
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

pub trait Accessor {
  pub fn get <Type: Field> (&mut self, id: SiphashID)-> Option <& Field::Data>;
}

pub trait Mutator <B: Basics>: Accessor {
  pub fn get_mutable <Type: Field> (&mut self, id: SiphashID)->& mut Option <Field::Data>;
  pub fn set <Type: Field> (&mut self, id: SiphashID, Option <Field::Data> data);
  pub fn random_bits (&mut self, bits:u32)->u64;
  pub fn random_id (&mut self)->SiphashID;
  pub fn now (& self)->B::Time;
  pub fn constants (& self)->& B::Constants;
}
pub trait PredictorAccessor <B: Basics>: Accessor {

}

pub type Event =Box <Fn (&mut Mutator)>;
enum Prediction <Time: BaseTime> {
  Nothing,
  Immediately (Event),
  At (Time, Event),
}

pub trait TimeSteward <B: Basics> {
  pub type A: Accessor;
  pub type M: Mutator;
  pub type P: PredictorAccessor;
  pub type Predictor = Box <Fn(&mut P)->Prediction>;
  
  pub fn new (constants: B::Constants, predictors: Vec<Predictor>)->Self;
  pub fn insert_fiat_event (&mut self, time: B::Time, distinguisher: u64, Event)->bool;
  pub fn erase_fiat_event (&mut self, time: B::Time, distinguisher: u64)->bool;
  pub fn accessor_after (&mut self, time: B::Time)->Accessor;
}

pub trait FlatTimeSteward <B: Basics>:TimeSteward <B> {
  pub fn valid_strictly_between (& self)->(B::Time, B::Time);
}


pub struct StandardFlatTimeSteward <B: Basics> {
  entity_states: HashMap< SiphashID, Box <Any>>,
  last_change: B::Time,
  fiat_events: BTreeMap< ExtendedTime <B>, Event>,
  predictions: HashMap<ExtendedTime <B>,
  prediction_times:
}
struct StandardFlatTimeStewardAccessor <'a, B: Basics> {
  steward: &'a StandardFlatTimeSteward <B>,
}
impl <'a, B: Basics> Accessor for StandardFlatTimeStewardAccessor <'a, B: Basics> {
  pub fn get <Type: Field> (&mut self, id: SiphashID)-> Option <& Field::Data> {
    self.steward.entity_states.get (id).map (| box | box.downcast <Field::Data>.unwrap ().borrow ()
  }
}
struct StandardFlatTimeStewardMutator <'a, B: Basics> {
  steward: & mut 'a StandardFlatTimeSteward <B>,
}
impl <'a, B: Basics> Accessor for StandardFlatTimeStewardMutator <'a, B: Basics> {
  pub fn get <Type: Field> (&mut self, id: SiphashID)-> Option <& Field::Data> {
    self.steward.entity_states.get (id)
  }
}



impl <B: Basics> TimeSteward <B> for StandardFlatTimeSteward <B> {
  pub type A: StandardFlatTimeStewardAccessor;
  pub type M: StandardFlatTimeStewardMutator;
  pub type P: StandardFlatTimeStewardPredictorAccessor;
  pub type Predictor = Box <Fn(&mut P)->Prediction>;
  
  pub fn new (constants: B::Constants, predictors: Vec<Predictor>)->Self;
  pub fn insert_fiat_event (&mut self, time: B::Time, distinguisher: u64, Event)->bool;
  pub fn erase_fiat_event (&mut self, time: B::Time, distinguisher: u64)->bool;
  pub fn accessor_after (&mut self, time: B::Time)->Accessor;
}



 #[test]
fn it_works() {
}
