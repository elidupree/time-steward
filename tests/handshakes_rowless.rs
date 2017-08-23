#[macro_use]
extern crate time_steward;

extern crate rand;
extern crate bincode;

extern crate serde;
#[macro_use]
extern crate serde_derive;

use time_steward::{DeterministicRandomId};
use time_steward::rowless::api::{self, StewardData, QueryOffset, DataTimelineCellTrait, Basics as BasicsTrait};
use time_steward::rowless::stewards::{simple_flat as steward_module};
use steward_module::{TimeSteward, ConstructibleTimeSteward, Event, DataTimelineCell, EventAccessor, UndoEventAccessor, SnapshotAccessor, simple_timeline};
use simple_timeline::{SimpleTimeline, GetVarying, tracking_query, modify_simple_timeline, unmodify_simple_timeline};


type Time = i64;
type Steward = steward_module::Steward <Basics>;

const HOW_MANY_PHILOSOPHERS: usize = 7;

type PhilosopherCell = DataTimelineCell <SimpleTimeline <Philosopher, Steward>>;

#[derive (Copy, Clone, Hash, PartialEq, Eq, PartialOrd, Ord, Serialize, Deserialize, Debug, Default)]
struct Basics {}
impl BasicsTrait for Basics {
  type Time = Time;
  type Globals = Vec<PhilosopherCell>;
  //type IncludedTypes = TimeStewardTypes;
}

#[derive (Clone, PartialEq, Eq, Serialize, Deserialize, Debug)]
struct Philosopher {
  // This is sometimes in the future because
  // they muse philosophically about handshakes
  // for a while, whenever one of them happens.
  time_when_next_initiates_handshake: Time,
  next_handshake_prediction: Option <<Steward as TimeSteward>::EventHandle>,
}
impl StewardData for Philosopher{}
/*impl Column for Philosopher {
  type FieldType = Self;
  fn column_id() -> ColumnId {
    ColumnId(0x4084d1501468b6dd)
  }
}*/

impl Philosopher {
  fn new()->Self {Philosopher {
    time_when_next_initiates_handshake: -1,
    next_handshake_prediction: None,
  }}
}

fn change_next_handshake_time <Accessor: EventAccessor <Steward = Steward>> (accessor: &Accessor, index: usize, handle: & PhilosopherCell, time: Time) {
  let mut philosopher = tracking_query(accessor, handle, QueryOffset::After).expect ("philosophers should never not exist").1;
  if let Some(prediction) = philosopher.next_handshake_prediction.take() {
    accessor.destroy_prediction (&prediction);
  }
  philosopher.time_when_next_initiates_handshake = time;
  if time >= *accessor.now() {
    let time_id = accessor.extended_now().id;
    philosopher.next_handshake_prediction = Some(accessor.create_prediction (time, DeterministicRandomId::new (& (time_id, index)), Shake {whodunnit: index}));
  }
  modify_simple_timeline (accessor, handle, Some (philosopher));
}


fn unchange_next_handshake_time <Accessor: UndoEventAccessor <Steward = Steward>> (accessor: &Accessor, handle: & PhilosopherCell) {
  let mut philosopher = accessor.query (handle, &GetVarying, QueryOffset::After).expect ("philosophers should never not exist").1;
  if let Some(prediction) = philosopher.next_handshake_prediction.take() {
    accessor.destroy_prediction (&prediction);
  }
  let mut philosopher = accessor.query (handle, &GetVarying, QueryOffset::Before).expect ("philosophers should never not exist").1;
  if let Some(prediction) = philosopher.next_handshake_prediction.take() {
    accessor.undestroy_prediction (&prediction, None);
  }
  unmodify_simple_timeline (accessor, handle);
}

 
/*
type TimeStewardTypes = (ListedType<SimpleTimeline <Philosopher>>,
                         ListedType<Initialize>,
                         ListedType<Tweak>,
                         ListedType<TweakUnsafe>,
                         ListedType<Shake>);*/

fn display_snapshot<Accessor: SnapshotAccessor<Steward = Steward>>(accessor: & Accessor) {
  println!("snapshot for {}", accessor.now());
  for handle in accessor.globals() {
    println!("{}",
             accessor.query(handle, &GetVarying, QueryOffset::After)
               .expect("missing philosopher").1
               .time_when_next_initiates_handshake);
  }
}



/*time_steward_predictor! (
  struct Shaker, Basics, PredictorId(0x0e7f27c7643f8167), watching Philosopher,
  | pa, whodunnit | {
// println!("Planning {}", whodunnit);
  let me = pa.get::<Philosopher>(whodunnit).unwrap().clone();
  pa.predict_at_time(me.time_when_next_initiates_handshake, Shake::new (whodunnit));
});*/

#[derive (Clone, PartialEq, Eq, Serialize, Deserialize, Debug)]
struct Shake {whodunnit: usize} //, Basics, EventId (0x8987a0b8e7d3d624),
impl StewardData for Shake {}
impl Event for Shake {
  type Steward = Steward;
  type ExecutionData = ();
  fn execute <Accessor: EventAccessor <Steward = Self::Steward>> (&self, accessor: &mut Accessor) {
    let now = *accessor.now();
    let friend_id = accessor.gen_range(0, HOW_MANY_PHILOSOPHERS);
    let awaken_time_1 = now + accessor.gen_range(-1, 4);
    let awaken_time_2 = now + accessor.gen_range(-1, 7);
    let philosophers = accessor.globals();
// println!("SHAKE!!! @{}. {}={}; {}={}", now, self.whodunnit, awaken_time_2, friend_id, awaken_time_1);
// IF YOU SHAKE YOUR OWN HAND YOU RECOVER
// IN THE SECOND TIME APPARENTLY
    change_next_handshake_time (accessor, friend_id, & philosophers [friend_id], awaken_time_1);
    change_next_handshake_time (accessor, self.whodunnit, & philosophers [self.whodunnit], awaken_time_2);
  }
  fn undo <Accessor: UndoEventAccessor <Steward = Self::Steward>> (&self, accessor: &mut Accessor, _: ()) {
    let friend_id = accessor.gen_range(0, HOW_MANY_PHILOSOPHERS);
    let philosophers = accessor.globals();
    unchange_next_handshake_time (accessor, & philosophers [friend_id]);
    unchange_next_handshake_time (accessor, & philosophers [self.whodunnit]);
  }
}

#[derive (Clone, PartialEq, Eq, Serialize, Deserialize, Debug)]
struct Initialize {} //, Basics, EventId (0xd5e73d8ba6ec59a2),
impl StewardData for Initialize {}
impl Event for Initialize {
  type Steward = Steward;
  type ExecutionData = ();
  fn execute <Accessor: EventAccessor <Steward = Self::Steward>> (&self, accessor: &mut Accessor) {
    println!("FIAT!!!!!");
    let philosophers = accessor.globals();
    for i in 0..HOW_MANY_PHILOSOPHERS {
      modify_simple_timeline (accessor, & philosophers [i], Some (Philosopher::new()));
      change_next_handshake_time (accessor, i, & philosophers [i], (i + 1) as Time);
    }
  }
  fn undo <Accessor: UndoEventAccessor <Steward = Self::Steward>> (&self, accessor: &mut Accessor, _: ()) {
    unimplemented!()
  }
}

#[derive (Clone, PartialEq, Eq, Serialize, Deserialize, Debug)]
struct Tweak {} //, Basics, EventId (0xfe9ff3047f9a9552),
impl StewardData for Tweak {}
impl Event for Tweak {
  type Steward = Steward;
  type ExecutionData = ();
  fn execute <Accessor: EventAccessor <Steward = Self::Steward>> (&self, accessor: &mut Accessor) {
    println!(" Tweak !!!!!");
    let now = *accessor.now();
    let friend_id = accessor.gen_range(0, HOW_MANY_PHILOSOPHERS);
    let awaken_time = now + accessor.gen_range(-1, 7);
    let philosophers = accessor.globals();
    change_next_handshake_time (accessor, friend_id, & philosophers [friend_id], awaken_time);
  }
  fn undo <Accessor: UndoEventAccessor <Steward = Self::Steward>> (&self, accessor: &mut Accessor, _: ()) {
    unimplemented!()
  }
}

use rand::{Rng, SeedableRng, ChaChaRng};
thread_local! {static INCONSISTENT: u32 = rand::thread_rng().gen::<u32>();}

#[derive (Clone, PartialEq, Eq, Serialize, Deserialize, Debug)]
struct TweakUnsafe {} //, Basics, EventId (0xa1618440808703da),
impl StewardData for TweakUnsafe {}
impl Event for TweakUnsafe {
  type Steward = Steward;
  type ExecutionData = ();
  fn execute <Accessor: EventAccessor <Steward = Self::Steward>> (&self, accessor: &mut Accessor) {
    let inconsistent = INCONSISTENT.with (| value | {
      *value
    });
    let mut rng = ChaChaRng::from_seed (& [inconsistent, accessor.next_u32()]);
    
    let now = *accessor.now();
    let friend_id = accessor.gen_range(0, HOW_MANY_PHILOSOPHERS);
    let awaken_time = now + rng.gen_range(-1, 7);
    let philosophers = accessor.globals();
    change_next_handshake_time (accessor, friend_id, & philosophers [friend_id], awaken_time);
  }
  fn undo <Accessor: UndoEventAccessor <Steward = Self::Steward>> (&self, accessor: &mut Accessor, _: ()) {
    unimplemented!()
  }
}

#[test]
pub fn handshakes_simple() {
  //type Steward = crossverified::Steward<Basics, inefficient_flat::Steward<Basics>, memoized_flat::Steward<Basics>>;
  let mut stew: Steward = Steward::from_globals ({
    let mut philosophers = Vec::new();
    for _ in 0.. HOW_MANY_PHILOSOPHERS {
      philosophers.push (DataTimelineCell::new (SimpleTimeline::new ()));
    }
    philosophers
  });

  stew.insert_fiat_event(0,
                       DeterministicRandomId::new(&0x32e1570766e768a7u64),
                       Initialize{})
    .unwrap();
    
  for increment in 1..21 {
    let snapshot: <Steward as TimeSteward>::SnapshotAccessor = stew.snapshot_before(&(increment * 100i64)).unwrap();
    display_snapshot(&snapshot);
  }
}

/*

#[test]
pub fn handshakes_reloading() {
  type Steward = crossverified::Steward<Basics, amortized::Steward<Basics>, memoized_flat::Steward<Basics>>;
  let mut stew: Steward = Steward::from_global_timeline (Basics::GlobalTimeline::new(Vec::new()));

  stew.insert_fiat_event(0,
                       DeterministicRandomId::new(&0x32e1570766e768a7u64),
                       Initialize::new())
    .unwrap();

  let mut snapshots = Vec::new();
  for increment in 1..21 {
    snapshots.push(stew.snapshot_before(&(increment * 100i64)));
    stew = Steward::from_snapshot::<<Steward as TimeSteward>::Snapshot> (snapshots.last().unwrap().as_ref().unwrap());
  }
  for snapshot in snapshots.iter_mut()
    .map(|option| option.as_mut().expect("all these snapshots should have been valid")) {
    display_snapshot(snapshot);
    let mut writer: Vec<u8> = Vec::with_capacity(128);
    time_steward::serialize_snapshot:: <Basics, <Steward as TimeSteward>::Snapshot,_,_> (snapshot, &mut writer, bincode::Infinite).unwrap();
    // let serialized = String::from_utf8 (serializer.into_inner()).unwrap();
    println!("{:?}", writer);
    use std::io::Cursor;
    let mut reader = Cursor::new(writer);
    let deserialized = time_steward::deserialize_snapshot:: <Basics, _,_> (&mut reader, bincode::Infinite/*serialized.as_bytes().iter().map (| bite | Ok (bite.clone()))*/).unwrap();
    println!("{:?}", deserialized);
    display_snapshot(&deserialized);
    use time_steward::MomentaryAccessor;
    display_snapshot(&Steward::from_snapshot::<time_steward::FiatSnapshot<Basics>>(&deserialized).snapshot_before(deserialized.now()).unwrap());
  }
  // panic!("anyway")
}

#[test]
fn handshakes_retroactive() {
  type Steward = crossverified::Steward<Basics, amortized::Steward<Basics>, flat_to_inefficient_full::Steward<Basics, memoized_flat::Steward <Basics> >>;
  let mut stew: Steward = Steward::from_constants(());

  stew.insert_fiat_event(0,
                       DeterministicRandomId::new(&0x32e1570766e768a7u64),
                       Initialize::new())
    .unwrap();

  stew.snapshot_before(&(2000i64));
  for increment in 1..21 {
    stew.insert_fiat_event(increment * 100i64, DeterministicRandomId::new(&increment), Tweak::new()).unwrap();
    let snapshot: <Steward as TimeSteward>::Snapshot = stew.snapshot_before(&(2000i64)).unwrap();
    display_snapshot(&snapshot);
  }

}

#[test]
fn local_synchronization_test() {
  use time_steward::stewards::simply_synchronized;
  use std::net::{TcpListener, TcpStream};
  use std::io::{BufReader, BufWriter};
  let listener = TcpListener::bind(("127.0.0.1", 0)).unwrap();
  let port = listener.local_addr().unwrap().port();
  ::std::thread::spawn(move || {
    let end_0 = listener.accept().unwrap().0;
    let mut stew_0: simply_synchronized::Steward<Basics, amortized::Steward<Basics>> =
      simply_synchronized::Steward::new(DeterministicRandomId::new(&0u32),
                                        0,
                                        4,
                                        (),
                                        BufReader::new(end_0.try_clone().unwrap()),
                                        BufWriter::new(end_0));
    stew_0.insert_fiat_event(0,
                         DeterministicRandomId::new(&0x32e1570766e768a7u64),
                         Initialize::new())
      .unwrap();

    for increment in 1..21 {
      let time = increment * 100i64;
      if increment % 3 == 0 {
        stew_0.insert_fiat_event(time, DeterministicRandomId::new(&increment), Tweak::new())
          .unwrap();
      }
      stew_0.snapshot_before(&time);
      stew_0.settle_before(time);
    }
    stew_0.finish();
  });
  let end_1 = TcpStream::connect(("127.0.0.1", port)).unwrap();
  let mut stew_1: simply_synchronized::Steward<Basics, amortized::Steward<Basics>> =
    simply_synchronized::Steward::new(DeterministicRandomId::new(&1u32),
                                      0,
                                      4,
                                      (),
                                      BufReader::new(end_1.try_clone().unwrap()),
                                      BufWriter::new(end_1));

  for increment in 1..21 {
    let time = increment * 100i64;
    if increment % 4 == 0 {
      stew_1.insert_fiat_event(time, DeterministicRandomId::new(&increment), Tweak::new()).unwrap();
    }
    stew_1.snapshot_before(&time);
    stew_1.settle_before(time);
  }
  stew_1.finish();
}

#[test]
#[should_panic (expected = "event occurred this way locally")]
fn local_synchronization_failure() {
  use time_steward::stewards::simply_synchronized;
  use std::net::{TcpListener, TcpStream};
  use std::io::{BufReader, BufWriter};
  let listener = TcpListener::bind(("127.0.0.1", 0)).unwrap();
  let port = listener.local_addr().unwrap().port();
  ::std::thread::spawn(move || {
    let end_0 = listener.accept().unwrap().0;
    let mut stew_0: simply_synchronized::Steward<Basics, amortized::Steward<Basics>> =
      simply_synchronized::Steward::new(DeterministicRandomId::new(&0u32),
                                        0,
                                        4,
                                        (),
                                        BufReader::new(end_0.try_clone().unwrap()),
                                        BufWriter::new(end_0));
    stew_0.insert_fiat_event(0,
                         DeterministicRandomId::new(&0x32e1570766e768a7u64),
                         Initialize::new())
      .unwrap();

    for increment in 1..21 {
      let time = increment * 100i64;
      if increment % 3 == 0 {
        stew_0.insert_fiat_event(time,
                             DeterministicRandomId::new(&increment),
                             TweakUnsafe::new())
          .unwrap();
      }
      stew_0.snapshot_before(&time);
      stew_0.settle_before(time);
    }
    stew_0.finish();
  });
  let end_1 = TcpStream::connect(("127.0.0.1", port)).unwrap();
  let mut stew_1: simply_synchronized::Steward<Basics, amortized::Steward<Basics>> =
    simply_synchronized::Steward::new(DeterministicRandomId::new(&1u32),
                                      0,
                                      4,
                                      (),
                                      BufReader::new(end_1.try_clone().unwrap()),
                                      BufWriter::new(end_1));

  for increment in 1..21 {
    let time = increment * 100i64;
    if increment % 4 == 0 {
      stew_1.insert_fiat_event(time,
                           DeterministicRandomId::new(&increment),
                           TweakUnsafe::new())
        .unwrap();
    }
    stew_1.snapshot_before(&time);
    stew_1.settle_before(time);
  }
  stew_1.finish();
}
*/
