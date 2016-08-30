use inefficient_flat_time_steward as s;
use {TimeSteward, DeterministicRandomId, Column, ColumnId, RowId, PredictorId, Mutator, StewardRc,
     TimeStewardStaticMethods, Accessor, MomentaryAccessor, PredictorAccessor, Snapshot};
use rand::Rng;
// use serde_json;
use bincode::serde::{Serializer, Deserializer};
use bincode;

use std::io::Write;
macro_rules! printlnerr(
    ($($arg:tt)*) => { {
        let r = writeln!(&mut ::std::io::stderr(), $($arg)*);
        r.expect("failed printing to stderr");
    } }
);

type Time = i64;

const HOW_MANY_PHILOSOPHERS: i32 = 7;

#[derive(Clone)]
struct Basics;
impl ::Basics for Basics {
  type Time = Time;
  type Constants = ();
}

#[derive(Clone, Serialize, Deserialize)]
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

type Steward = s::Steward<Basics>;

fn get_philosopher_id(index: i32) -> RowId {
  DeterministicRandomId::new(&(0x2302c38efb47e0d0u64, index))
}


macro_rules! for_all_columns {
  ($macro_name: ident {Column, $($macro_arguments:tt)*}) => {{
    for_these_columns! {$macro_name {Column, $($macro_arguments)*}, Philosopher}
  }};
}
make_snapshot_serde_functions! (serialize_snapshot, deserialize_snapshot);

fn display_snapshot<S: ::Snapshot<Basics>>(snapshot: &S) {
  printlnerr!("snapshot for {}", snapshot.now());
  for index in 0..HOW_MANY_PHILOSOPHERS {
    printlnerr!("{}",
                snapshot.get::<Philosopher>(get_philosopher_id(index))
                        .expect("missing philosopher")
                        .time_when_next_initiates_handshake);
  }
}

pub fn testfunc() {
  let mut stew: Steward =
    ::TimeStewardStaticMethods::new_empty((),
                                          Box::new([s::Predictor {
                                                      predictor_id: PredictorId(0x0e7f27c7643f8167),
                                                      column_id: Philosopher::column_id(),
                                                      function: StewardRc::new(|pa, whodunnit| {
                                                        // printlnerr!("Planning {}", whodunnit);
                                                        let me = pa.get::<Philosopher>(whodunnit)
                                                                   .unwrap()
                                                                   .clone();
                                                        pa.predict_at_time(me.time_when_next_initiates_handshake,
StewardRc::new(move |m| {
        let now = *m.now();
        let friend_id = get_philosopher_id(m.gen_range(0, HOW_MANY_PHILOSOPHERS));
        let awaken_time_1 = now + m.gen_range(-1, 4);
        let awaken_time_2 = now + m.gen_range(-1, 7);
        printlnerr!("SHAKE!!! @{}. {}={}; {}={}", now, whodunnit, awaken_time_2, friend_id, awaken_time_1);
        // IF YOU SHAKE YOUR OWN HAND YOU RECOVER
        // IN THE SECOND TIME APPARENTLY
        m.set::<Philosopher>(friend_id,
                             Some(Philosopher {
                               time_when_next_initiates_handshake: awaken_time_1,
                             }));
        m.set::<Philosopher>(whodunnit,
                             Some(Philosopher {
                               time_when_next_initiates_handshake: awaken_time_2,
                             }));
      }));
                                                      }),
                                                    }]));

  stew.insert_fiat_event(0,
                         DeterministicRandomId::new(&0x32e1570766e768a7u64),
                         StewardRc::new(|m| {
                           printlnerr!("FIAT!!!!!");
                           for i in 0..HOW_MANY_PHILOSOPHERS {
                             m.set::<Philosopher>(get_philosopher_id(i),
                           Some(Philosopher {
                             time_when_next_initiates_handshake: (i + 1) as Time,
                           }));
                           }
                         }))
      .unwrap();

  let mut snapshots = Vec::new();
  for increment in 1..21 {
    snapshots.push(stew.snapshot_before(&(increment * 100i64)));
  }
  for snapshot in snapshots.iter_mut().map(|option| {
    option.as_mut().expect("all these snapshots should have been valid")
  }) {
    display_snapshot(snapshot);
    let mut writer: Vec<u8> = Vec::with_capacity(128);
    {
      let mut serializer = Serializer::new(&mut writer);
      serialize_snapshot(snapshot, &mut serializer);
    }
    // let serialized = String::from_utf8 (serializer.into_inner()).unwrap();
    printlnerr!("{:?}", writer);
    let deserialized = deserialize_snapshot:: <Basics,_> (&mut Deserializer::new (&mut writer.as_slice(), bincode::SizeLimit::Infinite/*serialized.as_bytes().iter().map (| bite | Ok (bite.clone()))*/)).unwrap();
    display_snapshot(&deserialized);
  }
  // panic!("anyway")
}

#[test]
fn actuallytest() {
  testfunc();
}
