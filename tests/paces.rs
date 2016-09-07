#![feature (plugin, custom_derive)]
#![plugin (serde_macros)]

#[macro_use]
extern crate time_steward as steward;
extern crate rand;
extern crate serde;

use steward::{RowId, DeterministicRandomId, ColumnId, PredictorId, Column, TimeStewardSettings};
use std::marker::PhantomData;
use rand::{Rng, SeedableRng, ChaChaRng};

/*Tried to hunt an ICE, but failed
trait Hack <B> {type Whatever;}
use std::iter::{self, Empty};
struct IntoIter; impl <'a> IntoIterator for & 'a IntoIter {type Item = i32; type IntoIter = Empty <i32>; fn into_iter(self)->Self::IntoIter {iter::empty()}}
struct Original <B> {field: PhantomData <B>}
impl <B> Hack <B> for Original <B> {type Whatever = IntoIter;}
struct Wrapper <B, Inner: Hack <B>> {field: PhantomData <(B, Inner)>,}
impl <B, Inner: Hack <B>> Hack <B> for Wrapper <B, Inner> {type Whatever = Inner::Whatever;}
fn thingy <B, H: Hack <B>>() where for <'a> & 'a H::Whatever: IntoIterator <Item = i32> {}
#[test]
fn chunk() {thingy::<i32, Wrapper <i32, Wrapper <i32, Original<i32>>>>();}
*/

#[derive (Clone, PartialEq, Eq, Serialize, Deserialize, Debug, Default)]
struct Basics;
impl steward::Basics for Basics {
  type Time = DeterministicRandomId;
  type Constants = DeterministicRandomId;
}
  struct ColumnHack;
  impl Column for ColumnHack {
    type FieldType = DeterministicRandomId;
    fn column_id()->ColumnId {ColumnId (0x8e07f5045b91d636)}
  }
// As in, "putting it through its paces"
#[allow (unused_must_use)]
fn paces <Steward: steward::TimeSteward <Basics>, G: Rng> (generator: &mut G)
where Steward::Settings: Clone,
for <'a> & 'a Steward::Snapshot: IntoIterator <Item = steward::SnapshotEntry <'a, Basics>>
{


  let mut settings = Steward::Settings::new();
  settings.insert_predictor (PredictorId (0x59c5a4cce2789300), ColumnHack::column_id(),
    time_steward_predictor! (Basics, struct Predictor {}, | &self, accessor, id | {
      for index in 0.. (id.data() [0] ^ accessor.constants().data() [0]).leading_zeros() {
        let data_0 = DeterministicRandomId::new (& (index, id));
        let data_1 = DeterministicRandomId::new (& (index, id, "something different"));
        accessor.predict_at_time (data_0, time_steward_event! (Basics, struct Event {data: DeterministicRandomId = data_1}, | &self, mutator | {
          let mut generator = mutator.gen::<ChaChaRng>();
          for _ in 0.. (mutator.gen::<u32> ().leading_zeros()*10) {
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
  
  let mut stew: Steward = Steward::new_empty (RowId::new (& generator.gen::<u64>()), settings.clone());
  let mut snapshots = Vec::new();
  
  for index in 0..1000 {
    let time = RowId::new (& generator.gen::<u64>());
    match generator.gen_range::<u64> (0, 16) {
      0 => { stew.insert_fiat_event (time, RowId::new (& index),
      
      time_steward_event! (Basics, struct Event {}, | &self, mutator | {
        let who =RowId::new (& mutator.gen::<u8>());
          mutator.get:: <ColumnHack> (who);
          let who =RowId::new (& mutator.gen::<u8>());
          let what = if mutator.gen() {None} else {Some (mutator.gen_id())};
          mutator.set:: <ColumnHack> (who, what);
                    
         })

      
      );},
      1 => if let Some (snapshot) = stew.snapshot_before (& time) {snapshots.push (snapshot);},
      2 => if !snapshots.is_empty() {let who = generator.gen_range (0, snapshots.len()); snapshots.remove (who);},
      3 => if let Some (snapshot) = generator.choose (snapshots.as_slice()) { stew = Steward::from_snapshot::<Steward::Snapshot> (snapshot, settings.clone());},
      _ =>(),
    }
  }
}

#[test]
//#[ignore]
fn main() {
  let mut generator = ChaChaRng::from_seed(& [1337]);
  use steward::stewards::{amortized, memoized_flat, flat_to_inefficient_full, crossverified};
  paces:: <crossverified::Steward <Basics, 
    PhantomData <ColumnHack>,
    amortized::Steward <Basics>,
    /*flat_to_inefficient_full::Steward <Basics, */memoized_flat::Steward <Basics>/*>*/
  >,_> (&mut generator);
}
