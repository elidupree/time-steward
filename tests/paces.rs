
#[macro_use]
extern crate time_steward as steward;
extern crate rand;
extern crate serde;
#[macro_use]
extern crate serde_derive;

use steward::{RowId, DeterministicRandomId, ColumnId, PredictorId, EventId, Column, ColumnType, PredictorType, EventType, TimeStewardFromConstants, TimeStewardFromSnapshot};
use rand::{Rng, SeedableRng, ChaChaRng};

time_steward_basics!(struct Basics {
  type Time = DeterministicRandomId;
  type Constants = DeterministicRandomId;
  type IncludedTypes = (ColumnType <ColumnHack>, PredictorType <Predictor>, EventType <Event>, EventType <FiatEvent>);
});
  struct ColumnHack;
  impl Column for ColumnHack {
    type FieldType = DeterministicRandomId;
    fn column_id()->ColumnId {ColumnId (0x8e07f5045b91d636)}
  }
  
time_steward_predictor! (struct Predictor, Basics, PredictorId (0x59c5a4cce2789300), watching ColumnHack, | accessor, id | {
      let whatever = accessor.get::<ColumnHack> (id).unwrap().clone();
      for index in 0.. (id.data() [0] ^ accessor.constants().data() [0]).leading_zeros() {
        let data_0 = DeterministicRandomId::new (& (index, whatever, id));
        let data_1 = DeterministicRandomId::new (& (index, whatever, id, "something different"));
        accessor.predict_at_time (data_0, Event::new (id, data_1));
      }
    });
    
  time_steward_event! (struct Event {id: RowId, data: DeterministicRandomId}, Basics, EventId (0xc4b046f65bda3ba2), | &self, mutator | {
          if mutator.extended_now().iteration >10 {
            println!("Help! Loop!");
            mutator.set:: <ColumnHack> (self.id, None);
            return;
          }
          let mut generator = mutator.gen::<ChaChaRng>();
          for _ in 0.. (mutator.gen::<u32> ().leading_zeros()*10) {
            match mutator.gen_range::<u8>(0, 16) {
              0 => mutator.set:: <ColumnHack> (RowId::new (& generator.gen::<u8>()), Some (RowId::new (& generator.gen::<u8>()))), 
              1 => mutator.set:: <ColumnHack> (RowId::new (& generator.gen::<u8>()), None),
              2 => if mutator.get:: <ColumnHack> (RowId::new (& generator.gen::<u8>())).is_some() { mutator.gen_id(); },
              _ =>(),
            }
          }
        });
        
 time_steward_event! (struct FiatEvent {}, Basics, EventId (0x87d2e8f07f402d57), | &self, mutator | {
        let who =RowId::new (& mutator.gen::<u8>());
          mutator.get:: <ColumnHack> (who);
          let who =RowId::new (& mutator.gen::<u8>());
          let what = if mutator.gen() {None} else {Some (mutator.gen_id())};
          mutator.set:: <ColumnHack> (who, what);
                    
         });

// As in, "putting it through its paces"
#[allow (unused_must_use)]
fn paces <Steward: steward::IncrementalTimeSteward + TimeStewardFromConstants + TimeStewardFromSnapshot <Basics = Basics>, G: Rng> (generator: &mut G)
where for <'a> & 'a Steward::Snapshot: IntoIterator <Item = steward::SnapshotEntry <'a, Basics>>
{  
  let mut stew: Steward = Steward::from_constants (RowId::new (& generator.gen::<u64>()));
  let mut snapshots: Vec<Steward::Snapshot> = Vec::new();
  let mut fiat_events: Vec<(DeterministicRandomId, DeterministicRandomId)> = Vec::new();
  
  fn display_snapshot <S: steward::Snapshot <Basics = Basics>> (snapshot: & S)->(usize, DeterministicRandomId) {(snapshot.num_fields(), snapshot.now().clone())}
  
  for index in 0..10000 {
    let time = RowId::new (& generator.gen::<u64>());
    let choice = generator.gen_range::<u64> (0, 16);
    //println!(" Index: {:?}\n Time: {:?}\n ValidSince: {:?}\n    Present: {:?}\n Choice: {:?}\n\n ", index, time, stew.valid_since(), stew.updated_until_before (), choice);
    /*for snapshot in snapshots.iter() {
      snapshot.into_iter().count();
      if let Some (snapshot2) = Steward::from_snapshot::<Steward::Snapshot> (snapshot).snapshot_before (& snapshot.now()) {
        snapshot2.into_iter().count();
      }
    }*/
    match choice {
      0 => {
        println!("inserting fiat event");
        stew.insert_fiat_event (time, DeterministicRandomId::new (& index), FiatEvent::new());
        fiat_events.push ((time, DeterministicRandomId::new (& index)));
      },
      5 => if !fiat_events.is_empty() {
        println!("removing fiat event");
        let choice = generator.gen_range (0, fiat_events.len());
        let (time, id) = fiat_events [choice];
        stew.remove_fiat_event (&time, id);
        if generator.gen::<bool>() {fiat_events.remove (choice);}
      },
      1 => if let Some (limit) = stew.updated_until_before () {if let Some (snapshot) = stew.snapshot_before (& limit) {println!("recording snapshot {:?}", display_snapshot::<Steward::Snapshot> (&snapshot)); 
      //(&snapshot).into_iter().count();
      snapshots.push (snapshot);}},
      //2 => if snapshots.len() >30 {println!("deleting snapshot"); let who = generator.gen_range (0, snapshots.len()); snapshots.remove (who);},
      3 => if let Some (snapshot) = generator.choose (snapshots.as_slice()) { println!("reloading from snapshot {:?}", display_snapshot::<Steward::Snapshot> (&snapshot)); stew = Steward::from_snapshot::<Steward::Snapshot> (snapshot);},
      _ => {
        println!("stepping"); 
        stew.step();
        //println!(" ValidSince: {:?}\n    Present: {:?} ", stew.valid_since(), stew.updated_until_before ());
        //use steward::MomentaryAccessor; if let Some (limit) = stew.updated_until_before () {if let Some (snapshot) = stew.snapshot_before (& limit) {println!("checking snapshot at {:?}", snapshot.now()); (&snapshot).into_iter().count();}}
        
      },
      //_ => ()
    }
  }
}

#[test]
fn memoized_flat_cross_inefficient_flat() {
  for index in 0..10 {
    let mut generator = ChaChaRng::from_seed(& [1337, index]);
    use steward::stewards::{memoized_flat, inefficient_flat, crossverified};
    paces:: <crossverified::Steward <Basics, 
      memoized_flat::Steward <Basics>,
      inefficient_flat::Steward <Basics>
    >,_> (&mut generator);
  }
}

#[test]
fn amortized_cross_memoized_flat_to_inefficient_full() {
  for index in 0..10 {
    let mut generator = ChaChaRng::from_seed(& [1337, index]);
    use steward::stewards::{amortized, memoized_flat, flat_to_inefficient_full, crossverified};
    paces:: <crossverified::Steward <Basics, 
      amortized::Steward <Basics>,
      flat_to_inefficient_full::Steward <Basics, memoized_flat::Steward <Basics>>
    >,_> (&mut generator);
  }
}
