#![feature (plugin, custom_derive)]
#![plugin (serde_macros)]

#[macro_use]
extern crate time_steward as steward;
extern crate rand;
extern crate serde;

use steward::{RowId, DeterministicRandomId, ColumnId, PredictorId, Column, TimeStewardSettings};
use rand::{Rng, ChaChaRng};

#[derive (Clone, PartialEq, Eq, Serialize, Deserialize, Debug)]
struct Basics;
impl steward::Basics for Basics {
  type Time = DeterministicRandomId;
  type Constants = DeterministicRandomId;
}

// As in, "putting it through its paces"
fn paces <Steward: steward::TimeSteward <Basics>, G: Rng> (generator: &mut G) {
  struct ColumnHack;
  impl Column for ColumnHack {
    type FieldType = DeterministicRandomId;
    fn column_id()->ColumnId {ColumnId (0x8e07f5045b91d636)}
  }

  let mut settings = Steward::Settings::new();
  settings.insert_predictor (PredictorId (0x59c5a4cce2789300), ColumnHack::column_id(),
    time_steward_predictor! (Basics, struct Predictor {}, | &self, accessor, id | {
      for index in 0.. (id.data() [0] ^ accessor.constants().data() [0]).leading_zeros() {
        let data_0 = DeterministicRandomId::new (& (index, id));
        let data_1 = DeterministicRandomId::new (& (index, id, "something different"));
        accessor.predict_at_time (data_0, time_steward_event! (Basics, struct Event {data: DeterministicRandomId = data_1}, | &self, mutator | {
          let mut generator = mutator.gen::<ChaChaRng>();
          for _ in 0.. (mutator.gen::<u32> ().leading_zeros()) {
            match mutator.gen_range::<u8>(0, 16) {
              0 => mutator.set:: <ColumnHack> (RowId::new (& generator.gen::<u8>()), Some (RowId::new (& generator.gen::<u8>()))), 
              1 => mutator.set:: <ColumnHack> (RowId::new (& generator.gen::<u8>()), None),
              2 => if mutator.get:: <ColumnHack> (RowId::new (& generator.gen::<u8>())).is_some() { mutator.gen_id(); },
              _ =>(),
            }
          }
        }));
      }
    })
  );
  
  let mut stew: Steward = Steward::new_empty (RowId::new (& generator.gen::<u64>()), settings);
  
  
}
