
use super::{SiphashID, FieldID, Field, ExtendedTime, Basics, TimeSteward, FiatEventOperationResult};
use std::collections::{HashMap, BTreeMap};
use std::any::Any;
use std::borrow::{Borrow, Cow};
use std::rc::Rc;


struct ConstantPart<'a, B: Basics + 'a> {
  predictors: Vec<Predictor<'a, B>>,
  constants: B::Constants,
}
#[derive (Clone)]
struct BothParts<'a, B: Basics + 'a> {
  entity_states: HashMap<FieldID, Rc<Any>>,
  last_change: B::Time,
  fiat_events: BTreeMap<ExtendedTime<B>, Event<'a, B>>,
  predictions: BTreeMap<ExtendedTime<B>, Prediction<'a, B>>,
  prediction_dependencies: HashMap<FieldID, Vec<ExtendedTime<B>>>,
  constant_part: Rc<ConstantPart<'a, B>>,
}

pub struct Steward<'a, B: Basics + 'a> {
  data: Cow<'a, BothParts<'a, B>>,
}
pub struct Snapshot<'a, B: Basics + 'a> {
  now: B::Time,
  data: &'a BothParts<'a, B>,
}
pub struct Mutator<'a, B: Basics + 'a> {
  now: B::Time,
  data: &'a mut BothParts<'a, B>,
}
pub struct PredictorAccessor<'a, B: Basics + 'a> {
  data: &'a mut BothParts<'a, B>,
}
pub type Event<'a, B: Basics + 'a> = super::Event<Mutator<'a, B>>;
type Prediction<'a, B: Basics + 'a> = super::Prediction<B, Event<'a, B>>;
type Predictor<'a, B: Basics + 'a> = super::Predictor<B, Mutator<'a, B>, PredictorAccessor<'a, B>>;



impl<'a, B: Basics> super::Accessor<B> for Snapshot<'a, B> {
  fn get<F: Field>(&mut self, id: SiphashID) -> Option<&F::Data> {
    self.data.get::<F>(id)
  }
  fn constants(&self) -> &B::Constants {
    &self.data.constant_part.constants
  }
}
impl<'a, B: Basics> super::Accessor<B> for Mutator<'a, B> {
  fn get<F: Field>(&mut self, id: SiphashID) -> Option<&F::Data> {
    self.data.get::<F>(id)
  }
  fn constants(&self) -> &B::Constants {
    &self.data.constant_part.constants
  }
}
impl<'a, B: Basics> super::Accessor<B> for PredictorAccessor<'a, B> {
  fn get<F: Field>(&mut self, id: SiphashID) -> Option<&F::Data> {
    self.data.get::<F>(id)
  }
  fn constants(&self) -> &B::Constants {
    &self.data.constant_part.constants
  }
}

impl<'a, B: Basics> super::MomentaryAccessor<B> for Snapshot<'a, B> {
  fn now(&self) -> B::Time {
    self.now
  }
}
impl<'a, B: Basics> super::MomentaryAccessor<B> for Mutator<'a, B> {
  fn now(&self) -> B::Time {
    self.now
  }
}
impl<'a, B: Basics> super::PredictorAccessor<B> for PredictorAccessor<'a, B> {}
impl<'a, B: Basics> super::Snapshot<B> for Snapshot<'a, B> {}

impl<'a, B: Basics> super::Mutator<B> for Mutator<'a, B> {
  fn get_mut<F: Field>(&mut self, id: SiphashID) -> Option<&mut F::Data> where F::Data: Clone {}
  fn set<F: Field>(&mut self, id: SiphashID, data: Option<F::Data>) {}
  fn random_bits(&mut self, num_bits: u32) -> u64 {}
  fn random_id(&mut self) -> SiphashID {}
}






impl<'a, B: Basics> BothParts<'a, B> {
  fn get<F: Field>(&mut self, id: SiphashID) -> Option<&F::Data> {
    self.entity_states
      .get(&FieldID {
        entity: id,
        field: F::unique_identifier(),
      })
      .map(|something| something.downcast_ref::<F::Data>().unwrap().borrow())
  }
}


impl<'a, B: Basics> Steward<'a, B> {
  fn new(constants: B::Constants, predictors: Vec<Predictor<'a, B>>) -> Self {}
}
impl<'a, B: Basics> super::TimeSteward<'a, B> for Steward<'a, B> {
  type S = Snapshot<'a, B>;
  type Event = Event<'a, B>;

  fn valid_from(&self) -> B::Time {}
  fn insert_fiat_event(&mut self,
                       time: B::Time,
                       distinguisher: u64,
                       event: Self::Event)
                       -> FiatEventOperationResult {
  }
  fn erase_fiat_event(&mut self, time: B::Time, distinguisher: u64) -> FiatEventOperationResult {}
  fn snapshot_before(&mut self, time: B::Time) -> Option<Self::S> {}
}
