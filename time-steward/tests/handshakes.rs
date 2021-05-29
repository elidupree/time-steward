#![feature(specialization, generic_associated_types)]
#![allow(incomplete_features)]

use rand::{Rng, SeedableRng};
use rand_pcg::Pcg64Mcg;
use serde::{Deserialize, Serialize};
use std::cell::Cell;

use time_steward::type_utils::list_of_types::ListedType;
use time_steward::type_utils::{PersistentTypeId, PersistentlyIdentifiedType};
use time_steward::{
  ConstructGlobals, ConstructibleTimeSteward, EntityHandle, EntityHandleKind, EntityId, EntityKind,
  EventAccessor, Globals, GlobalsConstructionAccessor, OwnedTypedEntityHandle, SimulationSpec,
  SnapshotAccessor, TimeSteward, TypedHandle, TypedHandleRef, Wake, WriteAccess,
};
use time_steward_simple_flat as simple_flat;

type Time = i64;

const HOW_MANY_PHILOSOPHERS: usize = 7;

#[derive(
  Copy, Clone, Hash, PartialEq, Eq, PartialOrd, Ord, Serialize, Deserialize, Debug, Default,
)]
struct PhilosophersSpec;
//delegate!(PartialEq, Eq, PartialOrd, Ord, [this => &()], [Steward: TimeSteward], [SimulationSpec]);
impl SimulationSpec for PhilosophersSpec {
  type Time = Time;
  type Globals<H: EntityHandleKind> = Vec<TypedHandle<Philosopher, H>>;
  type Types = TimeStewardTypes;
}

struct Philosopher;

impl EntityKind for Philosopher {
  type ImmutableData<H: EntityHandleKind> = ();
  type MutableData<H: EntityHandleKind> = Pcg64Mcg;
}

impl PersistentlyIdentifiedType for Philosopher {
  const ID: PersistentTypeId = PersistentTypeId(0x28c7f7c2007af71f);
}

fn change_next_handshake_time<A: EventAccessor<SimulationSpec = PhilosophersSpec>>(
  accessor: &mut A,
  handle: TypedHandleRef<Philosopher, A::EntityHandleKind>,
  time: Time,
) {
  accessor.set_schedule(
    handle,
    if time >= *accessor.now() {
      Some(time)
    } else {
      None
    },
  );
}

type TimeStewardTypes = (
  ListedType<Initialize>,
  ListedType<Tweak>,
  ListedType<TweakUnsafe>,
);

fn display_snapshot<A: SnapshotAccessor<SimulationSpec = PhilosophersSpec>>(accessor: &A) {
  println!("snapshot for {}", accessor.now());
  for handle in accessor.globals() {
    println!("{:?}", accessor.raw_read_schedule(handle.borrow()));
  }
}

fn dump_snapshot<A: SnapshotAccessor<SimulationSpec = PhilosophersSpec>>(
  accessor: &A,
) -> Vec<Option<Time>> {
  let mut result = Vec::new();
  for handle in accessor.globals() {
    result.push(accessor.raw_read_schedule(handle.borrow()));
  }
  result
}

impl Wake<PhilosophersSpec> for Philosopher {
  fn wake<A: EventAccessor<SimulationSpec = PhilosophersSpec>>(
    accessor: &mut A,
    this: TypedHandleRef<Self, A::EntityHandleKind>,
  ) {
    let now = *accessor.now();
    let mut rng = this.write(accessor);
    let friend_id = rng.gen_range(0..HOW_MANY_PHILOSOPHERS);
    let awaken_time_1 = now + rng.gen_range(-1..4);
    let awaken_time_2 = now + rng.gen_range(0..7);
    std::mem::drop(rng);
    //println!("SHAKE!!! @{:?}. {}={}; {}={}", accessor.extended_now(), self.whodunnit, awaken_time_2, friend_id, awaken_time_1);
    // IF YOU SHAKE YOUR OWN HAND YOU RECOVER
    // IN THE SECOND TIME APPARENTLY
    let friend = accessor.globals()[friend_id].clone();
    if friend.id() != this.id() {
      change_next_handshake_time(accessor, friend.borrow(), awaken_time_1);
    }
    change_next_handshake_time(accessor, this, awaken_time_2);
  }
}

#[derive(Clone, PartialEq, Eq, Hash, Serialize, Deserialize, Debug)]
struct Initialize;
impl EntityKind for Initialize {
  type ImmutableData<H: EntityHandleKind> = ();
  type MutableData<H: EntityHandleKind> = ();
}
impl PersistentlyIdentifiedType for Initialize {
  const ID: PersistentTypeId = PersistentTypeId(0xd5e73d8ba6ec59a2);
}
impl Wake<PhilosophersSpec> for Initialize {
  fn wake<A: EventAccessor<SimulationSpec = PhilosophersSpec>>(
    accessor: &mut A,
    _this: TypedHandleRef<Self, A::EntityHandleKind>,
  ) {
    println!("FIAT!!!!!");
    for i in 0..HOW_MANY_PHILOSOPHERS {
      let philosopher = accessor.globals()[i].clone();
      change_next_handshake_time(accessor, philosopher.borrow(), (i + 1) as Time);
    }
  }
}

#[derive(Clone, PartialEq, Eq, Hash, Serialize, Deserialize, Debug)]
struct Tweak;
impl EntityKind for Tweak {
  type ImmutableData<H: EntityHandleKind> = ();
  type MutableData<H: EntityHandleKind> = ();
}
impl PersistentlyIdentifiedType for Tweak {
  const ID: PersistentTypeId = PersistentTypeId(0xfe9ff3047f9a9552);
}
impl Wake<PhilosophersSpec> for Tweak {
  fn wake<A: EventAccessor<SimulationSpec = PhilosophersSpec>>(
    accessor: &mut A,
    _this: TypedHandleRef<Self, A::EntityHandleKind>,
  ) {
    let now = *accessor.now();
    let first_philosopher = accessor.globals()[0].clone();
    let mut rng = first_philosopher.borrow().write(accessor);
    let friend_id = rng.gen_range(0..HOW_MANY_PHILOSOPHERS);
    let awaken_time = now + rng.gen_range(0..7);
    std::mem::drop(rng);
    println!(
      " Tweak !!!!! @{:?}. {}={}",
      accessor.now(),
      friend_id,
      awaken_time
    );
    let friend = accessor.globals()[friend_id].clone();
    change_next_handshake_time(accessor, friend.borrow(), awaken_time);
  }
}

#[derive(Clone, PartialEq, Eq, Hash, Serialize, Deserialize, Debug)]
struct TweakUnsafe;
impl EntityKind for TweakUnsafe {
  type ImmutableData<H: EntityHandleKind> = ();
  type MutableData<H: EntityHandleKind> = ();
}
impl PersistentlyIdentifiedType for TweakUnsafe {
  const ID: PersistentTypeId = PersistentTypeId(0xa1618440808703da);
}
impl Wake<PhilosophersSpec> for TweakUnsafe {
  fn wake<A: EventAccessor<SimulationSpec = PhilosophersSpec>>(
    accessor: &mut A,
    _this: TypedHandleRef<Self, A::EntityHandleKind>,
  ) {
    thread_local! {static INCONSISTENT: Cell<u32> = Cell::new(0);}
    let inconsistent = INCONSISTENT.with(|value| {
      value.set(value.get() + 1);
      value.get()
    });
    let mut rng = Pcg64Mcg::from_seed([inconsistent as u8; 16]);

    let now = *accessor.now();
    let friend_id = rng.gen_range(0..HOW_MANY_PHILOSOPHERS);
    let awaken_time = now + rng.gen_range(0..7);
    let friend = accessor.globals()[friend_id].clone();
    change_next_handshake_time(accessor, friend.borrow(), awaken_time);
  }
}

impl ConstructGlobals<PhilosophersSpec> for PhilosophersSpec {
  fn construct_globals<A: GlobalsConstructionAccessor<SimulationSpec = PhilosophersSpec>>(
    self,
    accessor: &mut A,
  ) -> Globals<PhilosophersSpec, A::EntityHandleKind> {
    (0..HOW_MANY_PHILOSOPHERS)
      .map(|_| accessor.create_entity::<Philosopher>((), Pcg64Mcg::new(0xbad)))
      .collect()
  }
}

fn make_steward<Steward: ConstructibleTimeSteward<SimulationSpec = PhilosophersSpec>>() -> Steward {
  let mut steward: Steward = Steward::from_construct_globals((), PhilosophersSpec);

  steward
    .insert_fiat_event::<Initialize>(0, EntityId::hash_of(&0x32e1570766e768a7u64), (), ())
    .unwrap();

  steward
}

type FlatSteward = self::simple_flat::Steward<PhilosophersSpec>;

fn handshakes_simple_generic<
  Steward: ConstructibleTimeSteward<SimulationSpec = PhilosophersSpec>,
>() {
  //type Steward = crossverified::Steward<SimulationSpec, inefficient_flat::Steward<SimulationSpec>, memoized_flat::Steward<SimulationSpec>>;
  let mut steward = make_steward::<Steward>();

  for increment in 1..21 {
    let snapshot: <Steward as TimeSteward>::SnapshotAccessor =
      steward.snapshot_before(increment * 100i64).unwrap();
    display_snapshot(&snapshot);
  }
}

#[test]
fn handshakes_simple() {
  handshakes_simple_generic::<FlatSteward>();
}

fn handshakes_snapshot_consistency_generic<
  Steward: ConstructibleTimeSteward<SimulationSpec = PhilosophersSpec>,
>() {
  let mut steward_1 = make_steward::<Steward>();
  let mut steward_2 = make_steward::<Steward>();
  let mut steward_3 = make_steward::<Steward>();
  let mut steward_4 = make_steward::<Steward>();

  let mut dumps_1 = Vec::new();
  let mut dumps_2 = Vec::new();
  let mut snapshots_3 = Vec::new();
  let mut snapshots_4 = Vec::new();

  steward_2.snapshot_before(2000i64).unwrap();

  for increment in 1..21 {
    let time = increment * 100i64;
    dumps_1.push(dump_snapshot(&mut steward_1.snapshot_before(time).unwrap()));
    dumps_2.push(dump_snapshot(&mut steward_2.snapshot_before(time).unwrap()));
    snapshots_3.push(steward_3.snapshot_before(time).unwrap());
    let snapshot = steward_4.snapshot_before(time).unwrap();
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
  Steward: ConstructibleTimeSteward<SimulationSpec = PhilosophersSpec>,
>() {
  let mut steward = make_steward::<Steward>();

  let first_dump;
  {
    let snapshot = steward.snapshot_before(2000i64).unwrap();
    first_dump = dump_snapshot(&snapshot);
    display_snapshot(&snapshot);
  }
  for increment in 1..21 {
    steward
      .insert_fiat_event::<Tweak>(increment * 100i64, EntityId::hash_of(&increment), (), ())
      .unwrap();
    let snapshot: <Steward as TimeSteward>::SnapshotAccessor =
      steward.snapshot_before(2000i64).unwrap();
    display_snapshot(&snapshot);
  }
  for increment in 1..21 {
    steward
      .remove_fiat_event(&(increment * 100i64), EntityId::hash_of(&increment))
      .unwrap();
    let snapshot: <Steward as TimeSteward>::SnapshotAccessor =
      steward.snapshot_before(2000i64).unwrap();
    display_snapshot(&snapshot);
  }
  let last_dump;
  {
    let snapshot = steward.snapshot_before(2000i64).unwrap();
    last_dump = dump_snapshot(&snapshot);
  }
  assert_eq!(first_dump, last_dump);
}

#[test]
fn handshakes_retroactive() {
  handshakes_retroactive_generic::<FlatSteward>();
}

fn handshakes_reloading_generic<
  Steward: ConstructibleTimeSteward<SimulationSpec = PhilosophersSpec>,
>() {
  let mut steward = make_steward::<Steward>();

  let first_dump;
  {
    let snapshot = steward.snapshot_before(2000i64).unwrap();
    first_dump = dump_snapshot(&snapshot);
    display_snapshot(&snapshot);
  }

  for increment in 1..21 {
    steward
      .insert_fiat_event::<Tweak>(increment * 100i64, EntityId::hash_of(&increment), (), ())
      .unwrap();
    let earlier_snapshot: <Steward as TimeSteward>::SnapshotAccessor =
      steward.snapshot_before(increment * 100i64).unwrap();
    let /*mut*/ serialized = Vec::new();
    //earlier_snapshot.serialize_into(&mut serialized).unwrap();
    use std::io::Cursor;
    let mut reader = Cursor::new(serialized);
    steward = Steward::from_serialized((), &mut reader).unwrap();
    let ending_snapshot: <Steward as TimeSteward>::SnapshotAccessor =
      steward.snapshot_before(2000i64).unwrap();
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
                       EntityId::hash_of(&0x32e1570766e768a7u64),
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
      simply_synchronized::Steward::new(EntityId::hash_of(&0u32),
                                        0,
                                        4,
                                        (),
                                        BufReader::new(end_0.try_clone().unwrap()),
                                        BufWriter::new(end_0));
    steward_0.insert_fiat_event(0,
                         EntityId::hash_of(&0x32e1570766e768a7u64),
                         Initialize::new())
      .unwrap();

    for increment in 1..21 {
      let time = increment * 100i64;
      if increment % 3 == 0 {
        steward_0.insert_fiat_event(time, EntityId::hash_of(&increment), Tweak::new())
          .unwrap();
      }
      steward_0.snapshot_before(&time);
      steward_0.settle_before(time);
    }
    steward_0.finish();
  });
  let end_1 = TcpStream::connect(("127.0.0.1", port)).unwrap();
  let mut steward_1: simply_synchronized::Steward<SimulationSpec, amortized::Steward<SimulationSpec>> =
    simply_synchronized::Steward::new(EntityId::hash_of(&1u32),
                                      0,
                                      4,
                                      (),
                                      BufReader::new(end_1.try_clone().unwrap()),
                                      BufWriter::new(end_1));

  for increment in 1..21 {
    let time = increment * 100i64;
    if increment % 4 == 0 {
      steward_1.insert_fiat_event(time, EntityId::hash_of(&increment), Tweak::new()).unwrap();
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
      simply_synchronized::Steward::new(EntityId::hash_of(&0u32),
                                        0,
                                        4,
                                        (),
                                        BufReader::new(end_0.try_clone().unwrap()),
                                        BufWriter::new(end_0));
    steward_0.insert_fiat_event(0,
                         EntityId::hash_of(&0x32e1570766e768a7u64),
                         Initialize::new())
      .unwrap();

    for increment in 1..21 {
      let time = increment * 100i64;
      if increment % 3 == 0 {
        steward_0.insert_fiat_event(time,
                             EntityId::hash_of(&increment),
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
    simply_synchronized::Steward::new(EntityId::hash_of(&1u32),
                                      0,
                                      4,
                                      (),
                                      BufReader::new(end_1.try_clone().unwrap()),
                                      BufWriter::new(end_1));

  for increment in 1..21 {
    let time = increment * 100i64;
    if increment % 4 == 0 {
      steward_1.insert_fiat_event(time,
                           EntityId::hash_of(&increment),
                           TweakUnsafe::new())
        .unwrap();
    }
    steward_1.snapshot_before(&time);
    steward_1.settle_before(time);
  }
  steward_1.finish();
}
*/
