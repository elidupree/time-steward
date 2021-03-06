extern crate time_steward;

extern crate rand;

extern crate serde;
#[macro_use]
extern crate serde_derive;

use crate::simple_timeline::{query, set, tracking_query, unset, SimpleTimeline};
use crate::steward_module::{
  simple_timeline, ConstructibleTimeSteward, DataTimelineCell, Event, EventAccessor,
  FutureCleanupAccessor, SnapshotAccessor, TimeSteward,
};
use time_steward::stewards::simple_full as steward_module;
use time_steward::DeterministicRandomId;
use time_steward::{
  Basics as BasicsTrait, DataTimelineCellTrait
};
use time_steward::type_utils::{PersistentTypeId, PersistentlyIdentifiedType};
use time_steward::type_utils::list_of_types::{ListedType};

type Time = i64;
type Steward = steward_module::Steward<Basics>;

const HOW_MANY_PHILOSOPHERS: usize = 7;

type PhilosopherCell = DataTimelineCell<SimpleTimeline<Philosopher, Steward>>;

#[derive(
  Copy, Clone, Hash, PartialEq, Eq, PartialOrd, Ord, Serialize, Deserialize, Debug, Default,
)]
struct Basics {}
impl BasicsTrait for Basics {
  type Time = Time;
  type Globals = Vec<PhilosopherCell>;
  type Types = TimeStewardTypes;
}

#[derive(Clone, PartialEq, Eq, Serialize, Deserialize, Debug)]
struct Philosopher {
  // This is sometimes in the future because
  // they muse philosophically about handshakes
  // for a while, whenever one of them happens.
  time_when_next_initiates_handshake: Time,
  next_handshake_prediction: Option<<Steward as TimeSteward>::EventHandle>,
}

impl Philosopher {
  fn new() -> Self {
    Philosopher {
      time_when_next_initiates_handshake: -1,
      next_handshake_prediction: None,
    }
  }
}

fn change_next_handshake_time<Accessor: EventAccessor<Steward = Steward>>(
  accessor: &Accessor,
  index: usize,
  handle: &PhilosopherCell,
  time: Time,
) {
  let mut philosopher = tracking_query(accessor, handle);
  philosopher.time_when_next_initiates_handshake = time;
  if time >= *accessor.now() {
    let time_id = accessor.extended_now().id;
    philosopher.next_handshake_prediction = Some(accessor.create_prediction(
      time,
      DeterministicRandomId::new(&(time_id, index)),
      Shake { whodunnit: index },
    ));
  } else {
    philosopher.next_handshake_prediction = None;
  }
  set(accessor, handle, philosopher);
}

fn unchange_next_handshake_time<Accessor: FutureCleanupAccessor<Steward = Steward>>(
  accessor: &Accessor,
  handle: &PhilosopherCell,
) {
  unset(accessor, handle);
}

type TimeStewardTypes = (
  ListedType<Initialize>,
  ListedType<Tweak>,
  ListedType<TweakUnsafe>,
  ListedType<Shake>,
);

fn display_snapshot<Accessor: SnapshotAccessor<Steward = Steward>>(accessor: &Accessor) {
  println!("snapshot for {}", accessor.now());
  for handle in accessor.globals() {
    println!(
      "{}",
      query(accessor, handle).time_when_next_initiates_handshake
    );
  }
}
fn dump_snapshot<Accessor: SnapshotAccessor<Steward = Steward>>(accessor: &Accessor) -> Vec<Time> {
  let mut result = Vec::new();
  for handle in accessor.globals() {
    result.push(query(accessor, handle).time_when_next_initiates_handshake);
  }
  result
}

#[derive(Clone, PartialEq, Eq, Serialize, Deserialize, Debug)]
struct Shake {
  whodunnit: usize,
}
impl PersistentlyIdentifiedType for Shake {
  const ID: PersistentTypeId = PersistentTypeId(0x8987a0b8e7d3d624);
}
impl Event for Shake {
  type Steward = Steward;
  type ExecutionData = ();
  fn execute<Accessor: EventAccessor<Steward = Self::Steward>>(&self, accessor: &mut Accessor) {
    let now = *accessor.now();
    let mut rng = accessor.id().to_rng();
    let friend_id = rng.gen_range(0, HOW_MANY_PHILOSOPHERS);
    let awaken_time_1 = now + rng.gen_range(-1, 4);
    let awaken_time_2 = now + rng.gen_range(-1, 7);
    let philosophers = accessor.globals();
    //println!("SHAKE!!! @{:?}. {}={}; {}={}", accessor.extended_now(), self.whodunnit, awaken_time_2, friend_id, awaken_time_1);
    // IF YOU SHAKE YOUR OWN HAND YOU RECOVER
    // IN THE SECOND TIME APPARENTLY
    if friend_id != self.whodunnit {
      change_next_handshake_time(accessor, friend_id, &philosophers[friend_id], awaken_time_1);
    }
    change_next_handshake_time(
      accessor,
      self.whodunnit,
      &philosophers[self.whodunnit],
      awaken_time_2,
    );
  }
  fn undo<Accessor: FutureCleanupAccessor<Steward = Self::Steward>>(
    &self,
    accessor: &mut Accessor,
    _: (),
  ) {
    let mut rng = accessor.id().to_rng();
    let friend_id = rng.gen_range(0, HOW_MANY_PHILOSOPHERS);
    let philosophers = accessor.globals();
    //println!("UNSHAKE!!! @{:?}. {} {}", accessor.extended_now(), self.whodunnit, friend_id);
    if friend_id != self.whodunnit {
      unchange_next_handshake_time(accessor, &philosophers[friend_id]);
    }
    unchange_next_handshake_time(accessor, &philosophers[self.whodunnit]);
  }
}

#[derive(Clone, PartialEq, Eq, Serialize, Deserialize, Debug)]
struct Initialize {}
impl PersistentlyIdentifiedType for Initialize {
  const ID: PersistentTypeId = PersistentTypeId(0xd5e73d8ba6ec59a2);
}
impl Event for Initialize {
  type Steward = Steward;
  type ExecutionData = ();
  fn execute<Accessor: EventAccessor<Steward = Self::Steward>>(&self, accessor: &mut Accessor) {
    println!("FIAT!!!!!");
    let philosophers = accessor.globals();
    for i in 0..HOW_MANY_PHILOSOPHERS {
      set(accessor, &philosophers[i], Philosopher::new());
      change_next_handshake_time(accessor, i, &philosophers[i], (i + 1) as Time);
    }
  }
  fn undo<Accessor: FutureCleanupAccessor<Steward = Self::Steward>>(
    &self,
    accessor: &mut Accessor,
    _: (),
  ) {
    let philosophers = accessor.globals();
    for i in 0..HOW_MANY_PHILOSOPHERS {
      unchange_next_handshake_time(accessor, &philosophers[i]);
      set(accessor, &philosophers[i], Philosopher::new());
    }
  }
}

#[derive(Clone, PartialEq, Eq, Serialize, Deserialize, Debug)]
struct Tweak {}
impl PersistentlyIdentifiedType for Tweak {
  const ID: PersistentTypeId = PersistentTypeId(0xfe9ff3047f9a9552);
}
impl Event for Tweak {
  type Steward = Steward;
  type ExecutionData = ();
  fn execute<Accessor: EventAccessor<Steward = Self::Steward>>(&self, accessor: &mut Accessor) {
    let now = *accessor.now();
    let mut rng = accessor.id().to_rng();
    let friend_id = rng.gen_range(0, HOW_MANY_PHILOSOPHERS);
    let awaken_time = now + rng.gen_range(-1, 7);
    let philosophers = accessor.globals();
    println!(
      " Tweak !!!!! @{:?}. {}={}",
      accessor.extended_now(),
      friend_id,
      awaken_time
    );
    change_next_handshake_time(accessor, friend_id, &philosophers[friend_id], awaken_time);
  }
  fn undo<Accessor: FutureCleanupAccessor<Steward = Self::Steward>>(
    &self,
    accessor: &mut Accessor,
    _: (),
  ) {
    let mut rng = accessor.id().to_rng();
    let friend_id = rng.gen_range(0, HOW_MANY_PHILOSOPHERS);
    let philosophers = accessor.globals();
    println!(
      " UnTweak !!!!! @{:?}. {}",
      accessor.extended_now(),
      friend_id
    );
    unchange_next_handshake_time(accessor, &philosophers[friend_id]);
  }
}

use rand::{ChaChaRng, Rng, SeedableRng};
thread_local! {static INCONSISTENT: u32 = rand::thread_rng().gen::<u32>();}

#[derive(Clone, PartialEq, Eq, Serialize, Deserialize, Debug)]
struct TweakUnsafe {}
impl PersistentlyIdentifiedType for TweakUnsafe {
  const ID: PersistentTypeId = PersistentTypeId(0xa1618440808703da);
}
impl Event for TweakUnsafe {
  type Steward = Steward;
  type ExecutionData = ();
  fn execute<Accessor: EventAccessor<Steward = Self::Steward>>(&self, accessor: &mut Accessor) {
    let inconsistent = INCONSISTENT.with(|value| *value);
    let mut rng =
      ChaChaRng::from_seed([inconsistent.wrapping_add(accessor.id().data()[1] as u32) as u8; 32]);

    let now = *accessor.now();
    let friend_id = rng.gen_range(0, HOW_MANY_PHILOSOPHERS);
    let awaken_time = now + rng.gen_range(-1, 7);
    let philosophers = accessor.globals();
    change_next_handshake_time(accessor, friend_id, &philosophers[friend_id], awaken_time);
  }
  fn undo<Accessor: FutureCleanupAccessor<Steward = Self::Steward>>(
    &self,
    accessor: &mut Accessor,
    _: (),
  ) {
    let inconsistent = INCONSISTENT.with(|value| *value);
    let mut rng =
      ChaChaRng::from_seed([inconsistent.wrapping_add(accessor.id().data()[1] as u32) as u8; 32]);

    let friend_id = rng.gen_range(0, HOW_MANY_PHILOSOPHERS);
    let philosophers = accessor.globals();
    unchange_next_handshake_time(accessor, &philosophers[friend_id]);
  }
}

fn make_globals() -> <Basics as BasicsTrait>::Globals {
  let mut philosophers = Vec::new();
  for _ in 0..HOW_MANY_PHILOSOPHERS {
    philosophers.push(DataTimelineCell::new(SimpleTimeline::new()));
  }
  philosophers
}

#[test]
pub fn handshakes_simple() {
  //type Steward = crossverified::Steward<Basics, inefficient_flat::Steward<Basics>, memoized_flat::Steward<Basics>>;
  let mut stew: Steward = Steward::from_globals(make_globals());

  stew
    .insert_fiat_event(
      0,
      DeterministicRandomId::new(&0x32e1570766e768a7u64),
      Initialize {},
    )
    .unwrap();

  for increment in 1..21 {
    let snapshot: <Steward as TimeSteward>::SnapshotAccessor =
      stew.snapshot_before(&(increment * 100i64)).unwrap();
    display_snapshot(&snapshot);
  }
}

#[test]
fn handshakes_retroactive() {
  let mut stew: Steward = Steward::from_globals(make_globals());

  stew
    .insert_fiat_event(
      0,
      DeterministicRandomId::new(&0x32e1570766e768a7u64),
      Initialize {},
    )
    .unwrap();

  let first_dump;
  {
    let snapshot = stew.snapshot_before(&(2000i64)).unwrap();
    first_dump = dump_snapshot(&snapshot);
    display_snapshot(&snapshot);
  }
  for increment in 1..21 {
    stew
      .insert_fiat_event(
        increment * 100i64,
        DeterministicRandomId::new(&increment),
        Tweak {},
      )
      .unwrap();
    let snapshot: <Steward as TimeSteward>::SnapshotAccessor =
      stew.snapshot_before(&(2000i64)).unwrap();
    display_snapshot(&snapshot);
  }
  for increment in 1..21 {
    stew
      .remove_fiat_event(
        &(increment * 100i64),
        DeterministicRandomId::new(&increment),
      )
      .unwrap();
    let snapshot: <Steward as TimeSteward>::SnapshotAccessor =
      stew.snapshot_before(&(2000i64)).unwrap();
    display_snapshot(&snapshot);
  }
  let last_dump;
  {
    let snapshot = stew.snapshot_before(&(2000i64)).unwrap();
    last_dump = dump_snapshot(&snapshot);
  }
  assert_eq!(first_dump, last_dump);
}

#[test]
fn handshakes_reloading() {
  let mut stew: Steward = Steward::from_globals(make_globals());

  stew
    .insert_fiat_event(
      0,
      DeterministicRandomId::new(&0x32e1570766e768a7u64),
      Initialize {},
    )
    .unwrap();

  let first_dump;
  {
    let snapshot = stew.snapshot_before(&(2000i64)).unwrap();
    first_dump = dump_snapshot(&snapshot);
    display_snapshot(&snapshot);
  }

  for increment in 1..21 {
    stew
      .insert_fiat_event(
        increment * 100i64,
        DeterministicRandomId::new(&increment),
        Tweak {},
      )
      .unwrap();
    let earlier_snapshot: <Steward as TimeSteward>::SnapshotAccessor =
      stew.snapshot_before(&(increment * 100i64)).unwrap();
    let mut serialized = Vec::new();
    earlier_snapshot.serialize_into(&mut serialized).unwrap();
    use std::io::Cursor;
    let mut reader = Cursor::new(serialized);
    stew = Steward::deserialize_from(&mut reader).unwrap();
    let ending_snapshot: <Steward as TimeSteward>::SnapshotAccessor =
      stew.snapshot_before(&(2000i64)).unwrap();
    let dump = dump_snapshot(&ending_snapshot);
    display_snapshot(&earlier_snapshot);
    assert_eq!(first_dump, dump);
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
