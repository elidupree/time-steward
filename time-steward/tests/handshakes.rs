#![feature(specialization)]

use derivative::Derivative;
use serde::{Deserialize, Serialize};

use time_steward::stewards::simple_flat;
use time_steward::type_utils::list_of_types::ListedType;
use time_steward::type_utils::{PersistentTypeId, PersistentlyIdentifiedType};
use time_steward::DeterministicRandomId;
use time_steward::{
  ConstructibleTimeSteward, EntityHandle, Event, EventAccessor, EventHandleTrait, ReplaceWith,
  SimulationSpec as SimulationSpecTrait, SimulationSpecGATs, SnapshotAccessor, TimeSteward,
};

type Time = i64;

const HOW_MANY_PHILOSOPHERS: usize = 7;

type PhilosopherHandle<Steward> = EntityHandle<Steward, (), Philosopher<Steward>>;
type EventHandle<Steward> = <Steward as TimeSteward>::EventHandle;

#[derive(
  Copy, Clone, Hash, PartialEq, Eq, PartialOrd, Ord, Serialize, Deserialize, Debug, Default,
)]
struct SimulationSpec;
//delegate!(PartialEq, Eq, PartialOrd, Ord, [this => &()], [Steward: TimeSteward], [SimulationSpec]);
impl SimulationSpecTrait for SimulationSpec {
  type Time = Time;
}
impl<Steward: TimeSteward> SimulationSpecGATs<Steward> for SimulationSpec {
  type Globals = Vec<PhilosopherHandle<Steward>>;
  type Types = TimeStewardTypes<Steward>;
}

#[derive(Serialize, Deserialize, Derivative)]
#[derivative(
  Clone(bound = ""),
  PartialEq(bound = ""),
  Eq(bound = ""),
  Hash(bound = ""),
  Debug(bound = "")
)]
#[serde(bound = "")]
struct Philosopher<Steward: TimeSteward> {
  // This is sometimes in the future because
  // they muse philosophically about handshakes
  // for a while, whenever one of them happens.
  time_when_next_initiates_handshake: Time,
  next_handshake_prediction: Option<EventHandle<Steward>>,
}

impl<Steward: TimeSteward> PersistentlyIdentifiedType for Philosopher<Steward> {
  const ID: PersistentTypeId = PersistentTypeId(0x28c7f7c2007af71f);
}

impl<Steward: TimeSteward> Philosopher<Steward> {
  fn new() -> Self {
    Philosopher {
      time_when_next_initiates_handshake: -1,
      next_handshake_prediction: None,
    }
  }
}

fn change_next_handshake_time<
  Steward: TimeSteward<SimulationSpec = SimulationSpec>,
  Accessor: EventAccessor<Steward = Steward>,
>(
  accessor: &Accessor,
  index: usize,
  handle: &PhilosopherHandle<Steward>,
  time: Time,
) where
  Steward::EventHandle: EventHandleTrait<Steward::SimulationSpec>,
{
  let philosopher = accessor.query(handle);
  if let Some(removed) = &philosopher.next_handshake_prediction {
    accessor.destroy_prediction(removed);
  }
  let new_philosopher = Philosopher {
    time_when_next_initiates_handshake: time,
    next_handshake_prediction: if time >= *accessor.now() {
      Some(accessor.create_prediction(time, Shake { whodunnit: index }))
    } else {
      None
    },
  };
  ::std::mem::drop(philosopher);
  accessor.modify::<(), Philosopher<Steward>, ReplaceWith<Philosopher<Steward>>>(
    handle,
    ReplaceWith(new_philosopher),
  );
}

type TimeStewardTypes<Steward> = (
  ListedType<Initialize>,
  ListedType<Tweak>,
  ListedType<TweakUnsafe>,
  ListedType<Shake>,
  ListedType<ReplaceWith<Philosopher<Steward>>>,
);

fn display_snapshot<
  Steward: TimeSteward<SimulationSpec = SimulationSpec>,
  Accessor: SnapshotAccessor<Steward = Steward>,
>(
  accessor: &Accessor,
) where
  Steward::EventHandle: EventHandleTrait<Steward::SimulationSpec>,
{
  println!("snapshot for {}", accessor.now());
  for handle in accessor.globals() {
    println!(
      "{}",
      accessor.query(handle).time_when_next_initiates_handshake
    );
  }
}
fn dump_snapshot<
  Steward: TimeSteward<SimulationSpec = SimulationSpec>,
  Accessor: SnapshotAccessor<Steward = Steward>,
>(
  accessor: &Accessor,
) -> Vec<Time>
where
  Steward::EventHandle: EventHandleTrait<Steward::SimulationSpec>,
{
  let mut result = Vec::new();
  for handle in accessor.globals() {
    result.push(accessor.query(handle).time_when_next_initiates_handshake);
  }
  result
}

#[derive(Clone, PartialEq, Eq, Hash, Serialize, Deserialize, Debug)]
struct Shake {
  whodunnit: usize,
}
impl PersistentlyIdentifiedType for Shake {
  const ID: PersistentTypeId = PersistentTypeId(0x8987a0b8e7d3d624);
}
impl<Steward: TimeSteward<SimulationSpec = SimulationSpec>> Event<Steward> for Shake
where
  Steward::EventHandle: EventHandleTrait<Steward::SimulationSpec>,
{
  fn execute<Accessor: EventAccessor<Steward = Steward>>(&self, accessor: &mut Accessor) {
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
}

#[derive(Clone, PartialEq, Eq, Hash, Serialize, Deserialize, Debug)]
struct Initialize {}
impl PersistentlyIdentifiedType for Initialize {
  const ID: PersistentTypeId = PersistentTypeId(0xd5e73d8ba6ec59a2);
}
impl<Steward: TimeSteward<SimulationSpec = SimulationSpec>> Event<Steward> for Initialize
where
  Steward::EventHandle: EventHandleTrait<Steward::SimulationSpec>,
{
  fn execute<Accessor: EventAccessor<Steward = Steward>>(&self, accessor: &mut Accessor) {
    println!("FIAT!!!!!");
    let philosophers = accessor.globals();
    for i in 0..HOW_MANY_PHILOSOPHERS {
      change_next_handshake_time(accessor, i, &philosophers[i], (i + 1) as Time);
    }
  }
}

#[derive(Clone, PartialEq, Eq, Hash, Serialize, Deserialize, Debug)]
struct Tweak {}
impl PersistentlyIdentifiedType for Tweak {
  const ID: PersistentTypeId = PersistentTypeId(0xfe9ff3047f9a9552);
}
impl<Steward: TimeSteward<SimulationSpec = SimulationSpec>> Event<Steward> for Tweak
where
  Steward::EventHandle: EventHandleTrait<Steward::SimulationSpec>,
{
  fn execute<Accessor: EventAccessor<Steward = Steward>>(&self, accessor: &mut Accessor) {
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
}

use rand::{ChaChaRng, Rng, SeedableRng};
thread_local! {static INCONSISTENT: u32 = rand::thread_rng().gen::<u32>();}

#[derive(Clone, PartialEq, Eq, Hash, Serialize, Deserialize, Debug)]
struct TweakUnsafe {}
impl PersistentlyIdentifiedType for TweakUnsafe {
  const ID: PersistentTypeId = PersistentTypeId(0xa1618440808703da);
}
impl<Steward: TimeSteward<SimulationSpec = SimulationSpec>> Event<Steward> for TweakUnsafe
where
  Steward::EventHandle: EventHandleTrait<Steward::SimulationSpec>,
{
  fn execute<Accessor: EventAccessor<Steward = Steward>>(&self, accessor: &mut Accessor) {
    let inconsistent = INCONSISTENT.with(|value| *value);
    let mut rng =
      ChaChaRng::from_seed([inconsistent.wrapping_add(accessor.id().data()[1] as u32) as u8; 32]);

    let now = *accessor.now();
    let friend_id = rng.gen_range(0, HOW_MANY_PHILOSOPHERS);
    let awaken_time = now + rng.gen_range(-1, 7);
    let philosophers = accessor.globals();
    change_next_handshake_time(accessor, friend_id, &philosophers[friend_id], awaken_time);
  }
}

fn make_globals<Steward: TimeSteward<SimulationSpec = SimulationSpec>>(
) -> <SimulationSpec as SimulationSpecGATs<Steward>>::Globals
where
  Steward::EventHandle: EventHandleTrait<Steward::SimulationSpec>,
{
  let mut philosophers = Vec::new();
  for _ in 0..HOW_MANY_PHILOSOPHERS {
    philosophers.push(Steward::new_entity_handle_nonreplicable(
      (),
      Philosopher::new(),
    ));
  }
  philosophers
}

fn make_steward<Steward: ConstructibleTimeSteward<SimulationSpec = SimulationSpec>>() -> Steward
where
  Steward::EventHandle: EventHandleTrait<Steward::SimulationSpec>,
{
  let mut steward: Steward = Steward::from_globals(make_globals::<Steward>());

  steward
    .insert_fiat_event(
      0,
      DeterministicRandomId::new(&0x32e1570766e768a7u64),
      Initialize {},
    )
    .unwrap();

  steward
}

type FlatSteward = self::simple_flat::Steward<SimulationSpec>;

fn handshakes_simple_generic<Steward: ConstructibleTimeSteward<SimulationSpec = SimulationSpec>>()
where
  Steward::EventHandle: EventHandleTrait<Steward::SimulationSpec>,
{
  //type Steward = crossverified::Steward<SimulationSpec, inefficient_flat::Steward<SimulationSpec>, memoized_flat::Steward<SimulationSpec>>;
  let mut steward = make_steward::<Steward>();

  for increment in 1..21 {
    let snapshot: <Steward as TimeSteward>::SnapshotAccessor =
      steward.snapshot_before(&(increment * 100i64)).unwrap();
    display_snapshot(&snapshot);
  }
}

#[test]
fn handshakes_simple() {
  handshakes_simple_generic::<FlatSteward>();
}

fn handshakes_snapshot_consistency_generic<
  Steward: ConstructibleTimeSteward<SimulationSpec = SimulationSpec>,
>()
where
  Steward::EventHandle: EventHandleTrait<Steward::SimulationSpec>,
{
  let mut steward_1 = make_steward::<Steward>();
  let mut steward_2 = make_steward::<Steward>();
  let mut steward_3 = make_steward::<Steward>();
  let mut steward_4 = make_steward::<Steward>();

  let mut dumps_1 = Vec::new();
  let mut dumps_2 = Vec::new();
  let mut snapshots_3 = Vec::new();
  let mut snapshots_4 = Vec::new();

  steward_2.snapshot_before(&2000i64).unwrap();

  for increment in 1..21 {
    let time = increment * 100i64;
    dumps_1.push(dump_snapshot(&steward_1.snapshot_before(&time).unwrap()));
    dumps_2.push(dump_snapshot(&steward_2.snapshot_before(&time).unwrap()));
    snapshots_3.push(steward_3.snapshot_before(&time).unwrap());
    let snapshot = steward_4.snapshot_before(&time).unwrap();
    dump_snapshot(&snapshot);
    snapshots_4.push(snapshot);
  }

  let dumps_3: Vec<_> = snapshots_3.iter().map(dump_snapshot).collect();
  let dumps_4: Vec<_> = snapshots_4.iter().map(dump_snapshot).collect();

  assert_eq!(dumps_1, dumps_2);
  assert_eq!(dumps_1, dumps_3);
  assert_eq!(dumps_1, dumps_4);
}

#[test]
fn handshakes_snapshot_consistency() {
  handshakes_snapshot_consistency_generic::<FlatSteward>();
}

fn handshakes_retroactive_generic<
  Steward: ConstructibleTimeSteward<SimulationSpec = SimulationSpec>,
>()
where
  Steward::EventHandle: EventHandleTrait<Steward::SimulationSpec>,
{
  let mut steward = make_steward::<Steward>();

  let first_dump;
  {
    let snapshot = steward.snapshot_before(&(2000i64)).unwrap();
    first_dump = dump_snapshot(&snapshot);
    display_snapshot(&snapshot);
  }
  for increment in 1..21 {
    steward
      .insert_fiat_event(
        increment * 100i64,
        DeterministicRandomId::new(&increment),
        Tweak {},
      )
      .unwrap();
    let snapshot: <Steward as TimeSteward>::SnapshotAccessor =
      steward.snapshot_before(&(2000i64)).unwrap();
    display_snapshot(&snapshot);
  }
  for increment in 1..21 {
    steward
      .remove_fiat_event(
        &(increment * 100i64),
        DeterministicRandomId::new(&increment),
      )
      .unwrap();
    let snapshot: <Steward as TimeSteward>::SnapshotAccessor =
      steward.snapshot_before(&(2000i64)).unwrap();
    display_snapshot(&snapshot);
  }
  let last_dump;
  {
    let snapshot = steward.snapshot_before(&(2000i64)).unwrap();
    last_dump = dump_snapshot(&snapshot);
  }
  assert_eq!(first_dump, last_dump);
}

#[test]
fn handshakes_retroactive() {
  handshakes_retroactive_generic::<FlatSteward>();
}

fn handshakes_reloading_generic<
  Steward: ConstructibleTimeSteward<SimulationSpec = SimulationSpec>,
>()
where
  Steward::EventHandle: EventHandleTrait<Steward::SimulationSpec>,
{
  let mut steward = make_steward::<Steward>();

  let first_dump;
  {
    let snapshot = steward.snapshot_before(&(2000i64)).unwrap();
    first_dump = dump_snapshot(&snapshot);
    display_snapshot(&snapshot);
  }

  for increment in 1..21 {
    steward
      .insert_fiat_event(
        increment * 100i64,
        DeterministicRandomId::new(&increment),
        Tweak {},
      )
      .unwrap();
    let earlier_snapshot: <Steward as TimeSteward>::SnapshotAccessor =
      steward.snapshot_before(&(increment * 100i64)).unwrap();
    let mut serialized = Vec::new();
    earlier_snapshot.serialize_into(&mut serialized).unwrap();
    use std::io::Cursor;
    let mut reader = Cursor::new(serialized);
    steward = Steward::deserialize_from(&mut reader).unwrap();
    let ending_snapshot: <Steward as TimeSteward>::SnapshotAccessor =
      steward.snapshot_before(&(2000i64)).unwrap();
    let dump = dump_snapshot(&ending_snapshot);
    display_snapshot(&earlier_snapshot);
    assert_eq!(first_dump, dump);
  }
}

#[test]
fn handshakes_reloading() {
  handshakes_reloading_generic::<FlatSteward>();
}

/*

#[test]
pub fn handshakes_reloading() {
  type Steward = crossverified::Steward<SimulationSpec, amortized::Steward<SimulationSpec>, memoized_flat::Steward<SimulationSpec>>;
  let mut steward: Steward = Steward::from_global_timeline (SimulationSpec::GlobalTimeline::new(Vec::new()));

  steward.insert_fiat_event(0,
                       DeterministicRandomId::new(&0x32e1570766e768a7u64),
                       Initialize::new())
    .unwrap();

  let mut snapshots = Vec::new();
  for increment in 1..21 {
    snapshots.push(steward.snapshot_before(&(increment * 100i64)));
    steward = Steward::from_snapshot::<<Steward as TimeSteward>::Snapshot> (snapshots.last().unwrap().as_ref().unwrap());
  }
  for snapshot in snapshots.iter_mut()
    .map(|option| option.as_mut().expect("all these snapshots should have been valid")) {
    display_snapshot(snapshot);
    let mut writer: Vec<u8> = Vec::with_capacity(128);
    time_steward::serialize_snapshot:: <SimulationSpec, <Steward as TimeSteward>::Snapshot,_,_> (snapshot, &mut writer, bincode::Infinite).unwrap();
    // let serialized = String::from_utf8 (serializer.into_inner()).unwrap();
    println!("{:?}", writer);
    use std::io::Cursor;
    let mut reader = Cursor::new(writer);
    let deserialized = time_steward::deserialize_snapshot:: <SimulationSpec, _,_> (&mut reader, bincode::Infinite/*serialized.as_bytes().iter().map (| bite | Ok (bite.clone()))*/).unwrap();
    println!("{:?}", deserialized);
    display_snapshot(&deserialized);
    use time_steward::MomentaryAccessor;
    display_snapshot(&Steward::from_snapshot::<time_steward::FiatSnapshot<SimulationSpec>>(&deserialized).snapshot_before(deserialized.now()).unwrap());
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
    let mut steward_0: simply_synchronized::Steward<SimulationSpec, amortized::Steward<SimulationSpec>> =
      simply_synchronized::Steward::new(DeterministicRandomId::new(&0u32),
                                        0,
                                        4,
                                        (),
                                        BufReader::new(end_0.try_clone().unwrap()),
                                        BufWriter::new(end_0));
    steward_0.insert_fiat_event(0,
                         DeterministicRandomId::new(&0x32e1570766e768a7u64),
                         Initialize::new())
      .unwrap();

    for increment in 1..21 {
      let time = increment * 100i64;
      if increment % 3 == 0 {
        steward_0.insert_fiat_event(time, DeterministicRandomId::new(&increment), Tweak::new())
          .unwrap();
      }
      steward_0.snapshot_before(&time);
      steward_0.settle_before(time);
    }
    steward_0.finish();
  });
  let end_1 = TcpStream::connect(("127.0.0.1", port)).unwrap();
  let mut steward_1: simply_synchronized::Steward<SimulationSpec, amortized::Steward<SimulationSpec>> =
    simply_synchronized::Steward::new(DeterministicRandomId::new(&1u32),
                                      0,
                                      4,
                                      (),
                                      BufReader::new(end_1.try_clone().unwrap()),
                                      BufWriter::new(end_1));

  for increment in 1..21 {
    let time = increment * 100i64;
    if increment % 4 == 0 {
      steward_1.insert_fiat_event(time, DeterministicRandomId::new(&increment), Tweak::new()).unwrap();
    }
    steward_1.snapshot_before(&time);
    steward_1.settle_before(time);
  }
  steward_1.finish();
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
    let mut steward_0: simply_synchronized::Steward<SimulationSpec, amortized::Steward<SimulationSpec>> =
      simply_synchronized::Steward::new(DeterministicRandomId::new(&0u32),
                                        0,
                                        4,
                                        (),
                                        BufReader::new(end_0.try_clone().unwrap()),
                                        BufWriter::new(end_0));
    steward_0.insert_fiat_event(0,
                         DeterministicRandomId::new(&0x32e1570766e768a7u64),
                         Initialize::new())
      .unwrap();

    for increment in 1..21 {
      let time = increment * 100i64;
      if increment % 3 == 0 {
        steward_0.insert_fiat_event(time,
                             DeterministicRandomId::new(&increment),
                             TweakUnsafe::new())
          .unwrap();
      }
      steward_0.snapshot_before(&time);
      steward_0.settle_before(time);
    }
    steward_0.finish();
  });
  let end_1 = TcpStream::connect(("127.0.0.1", port)).unwrap();
  let mut steward_1: simply_synchronized::Steward<SimulationSpec, amortized::Steward<SimulationSpec>> =
    simply_synchronized::Steward::new(DeterministicRandomId::new(&1u32),
                                      0,
                                      4,
                                      (),
                                      BufReader::new(end_1.try_clone().unwrap()),
                                      BufWriter::new(end_1));

  for increment in 1..21 {
    let time = increment * 100i64;
    if increment % 4 == 0 {
      steward_1.insert_fiat_event(time,
                           DeterministicRandomId::new(&increment),
                           TweakUnsafe::new())
        .unwrap();
    }
    steward_1.snapshot_before(&time);
    steward_1.settle_before(time);
  }
  steward_1.finish();
}
*/
