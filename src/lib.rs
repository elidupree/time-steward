
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

struct ExtendedTime <Base: BaseTime> {
  base: Base,
  iteration: u64,
  id: SiphashID,
}

pub trait Basics {
  type Time: BaseTime;
  type Constants;
}

pub trait Accessor {
  fn get <Type: Field> (&mut self, id: SiphashID)->& Option <Field::Data>;
}

pub trait Mutator <B: Basics>: Accessor {
  fn get_mutable <Type: Field> (&mut self, id: SiphashID)->& mut Option <Field::Data>;
  fn set <Type: Field> (&mut self, id: SiphashID, Option <Field::Data> data);
  fn random_bits (&mut self, bits:u32)->u64;
  fn random_id (&mut self)->SiphashID;
  fn now (& self)->B::Time;
  fn constants (& self)->& B::Constants;
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
  fn new (constants: B::Constants, predictors: Vec<Predictor>)->Self;
}

pub trait FlatTimeSteward <B: Basics>:TimeSteward <B> {
  
}



 #[test]
fn it_works() {
}
