use stewards::crossverified as s;
use {TimeSteward, TimeStewardSettings, DeterministicRandomId, Column, ColumnId, RowId, PredictorId, ColumnType};
// use serde_json;
use bincode::serde::{Serializer, Deserializer};
use bincode;
use stewards::amortized;
use stewards::memoized_flat;

type Time = i64;

const HOW_MANY_PHILOSOPHERS: i32 = 7;

#[derive (Clone, PartialEq, Eq, Serialize, Deserialize, Debug, Default)]
struct Basics;
impl ::Basics for Basics {
  type Time = Time;
  type Constants = ();
  type Columns = Columns;
}

#[derive (Clone, PartialEq, Eq, Serialize, Deserialize, Debug)]
struct Philosopher {
  // This is sometimes in the future because
  // they muse philosophically about handshakes
  // for a while, whenever one of them happens.
  time_when_next_initiates_handshake: Time,
}
impl Column for Philosopher {
  type FieldType = Self;
  fn column_id() -> ColumnId {
    ColumnId(0x4084d1501468b6dd)
  }
}

type Steward = s::Steward<Basics,
                          amortized::Steward<Basics>,
                          memoized_flat::Steward<Basics>>;

fn get_philosopher_id(index: i32) -> RowId {
  DeterministicRandomId::new(&(0x2302c38efb47e0d0u64, index))
}

type Columns = ColumnType<Philosopher>;

fn display_snapshot<S: ::Snapshot<Basics>>(snapshot: &S) {
  println!("snapshot for {}", snapshot.now());
  for index in 0..HOW_MANY_PHILOSOPHERS {
    println!("{}",
             snapshot.get::<Philosopher>(get_philosopher_id(index))
                     .expect("missing philosopher")
                     .time_when_next_initiates_handshake);
  }
}

pub fn testfunc() {
  let mut settings = <Steward as TimeSteward>::Settings::new();
  settings.insert_predictor (PredictorId(0x0e7f27c7643f8167), Philosopher::column_id(),
    time_steward_predictor! (Basics, struct Shaker {}, | &self, pa, whodunnit | {
      println!("Planning {}", whodunnit);
      
      let me = pa.get::<Philosopher>(whodunnit).unwrap().clone();
      pa.predict_at_time(me.time_when_next_initiates_handshake,
        time_steward_event! (Basics, struct Shake {whodunnit: RowId = whodunnit}, | &self, m | {
          let now = *m.now();
          let friend_id = get_philosopher_id(m.gen_range(0, HOW_MANY_PHILOSOPHERS));
          let awaken_time_1 = now + m.gen_range(-1, 4);
          let awaken_time_2 = now + m.gen_range(-1, 7);
          println!("SHAKE!!! @{}. {}={}; {}={}", now, self.whodunnit, awaken_time_2, friend_id, awaken_time_1);
          // IF YOU SHAKE YOUR OWN HAND YOU RECOVER
          // IN THE SECOND TIME APPARENTLY
          m.set::<Philosopher>(friend_id,
                                   Some(Philosopher {
                                     time_when_next_initiates_handshake: awaken_time_1,
                                   }));
          m.set::<Philosopher>(self.whodunnit,
                                   Some(Philosopher {
                                     time_when_next_initiates_handshake: awaken_time_2,
                                   }));
        })
      );
    })
  );

  let mut stew: Steward = ::TimeSteward::new_empty((), settings.clone());

  stew.insert_fiat_event(0,
                       DeterministicRandomId::new(&0x32e1570766e768a7u64),
                       time_steward_event! (Basics, struct Initialize {}, | &self, m | {
      println!("FIAT!!!!!");
      for i in 0..HOW_MANY_PHILOSOPHERS {
        m.set::<Philosopher>(get_philosopher_id(i),
          Some(Philosopher {
            time_when_next_initiates_handshake: (i + 1) as Time,
          })
        );
      }
    }))
    .unwrap();

  let mut snapshots = Vec::new();
  for increment in 1..21 {
    snapshots.push(stew.snapshot_before(&(increment * 100i64)));
    stew = ::TimeSteward::from_snapshot::<<Steward as ::TimeSteward<Basics>>::Snapshot> (snapshots.last().unwrap().as_ref().unwrap(), settings.clone());
  }
  for snapshot in snapshots.iter_mut()
    .map(|option| option.as_mut().expect("all these snapshots should have been valid")) {
    display_snapshot(snapshot);
    let mut writer: Vec<u8> = Vec::with_capacity(128);
    {
      let mut serializer = Serializer::new(&mut writer);
      ::serialize_snapshot:: <Basics, Columns, <Steward as TimeSteward <Basics>>::Snapshot,_> (snapshot, &mut serializer).unwrap();
    }
    // let serialized = String::from_utf8 (serializer.into_inner()).unwrap();
    println!("{:?}", writer);
    let deserialized = ::deserialize_snapshot:: <Basics, Columns,_> (&mut Deserializer::new (&mut writer.as_slice(), bincode::SizeLimit::Infinite/*serialized.as_bytes().iter().map (| bite | Ok (bite.clone()))*/)).unwrap();
    display_snapshot(&deserialized);
    use MomentaryAccessor;
    display_snapshot(&<Steward as ::TimeSteward<Basics>>::from_snapshot::<::FiatSnapshot<Basics>>(&deserialized, settings.clone()).snapshot_before(deserialized.now()).unwrap());
  }
  // panic!("anyway")
}

#[test]
fn actuallytest() {
  testfunc();
}
