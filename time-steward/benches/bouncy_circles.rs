#![feature (test)]
#![feature (macro_vis_matcher)]

extern crate test;
#[macro_use]
extern crate time_steward;

#[macro_use]
extern crate glium;

extern crate nalgebra;
extern crate rand;
extern crate boolinator;
extern crate docopt;

extern crate serde;
#[macro_use]
extern crate serde_derive;


use test::Bencher;

use time_steward::{EntityId};
use time_steward::{PersistentTypeId, ListedType, PersistentlyIdentifiedType, EntityCellTrait, SimulationSpec as SimulationSpecTrait};
//use time_steward::stewards::{simple_full as steward_module};
use steward_module::{TimeSteward, ConstructibleTimeSteward, Event, EntityCell, EventAccessor, FutureCleanupAccessor, SnapshotAccessor, simple_timeline};
use simple_timeline::{SimpleTimeline, GetVarying};

#[path = "../dev-shared/bouncy_circles.rs"] mod bouncy_circles;
use bouncy_circles::*;

#[bench]
fn bouncy_circles_straightforward(bencher: &mut Bencher) {
  bencher.iter(|| {
    let mut steward: Steward = Steward::from_globals (make_globals());
    steward.insert_fiat_event(0, EntityId::hash_of(&0), Initialize {}).unwrap();
    for index in 0..1000 {
      let time = 10*SECOND*index/1000;
      steward.snapshot_before(& time).expect("steward failed to provide snapshot");
      steward.forget_before(& time);
    }
  })
}


#[bench]
fn bouncy_circles_disturbed (bencher: &mut Bencher) {
  bencher.iter(|| {
    let mut steward: Steward = Steward::from_globals (make_globals());
    steward.insert_fiat_event(0, EntityId::hash_of(&0), Initialize {}).unwrap();
    for index in 1..10 {
      steward.insert_fiat_event (index*SECOND, EntityId::hash_of (& index), Disturb{ coordinates: [ARENA_SIZE/3,ARENA_SIZE/3]}).unwrap();
    }
    for index in 0..1000 {
      let time = 10*SECOND*index/1000;
      steward.snapshot_before(& time).expect("steward failed to provide snapshot");
      steward.forget_before(& time);
    }
  })
}
/*
#[bench]
fn bouncy_circles_disturbed_retroactive (bencher: &mut Bencher) {
  bencher.iter(|| {
    let mut steward: amortized::Steward<SimulationSpec> = amortized::Steward::from_constants(());
    steward.insert_fiat_event(0, EntityId::hash_of(&0), Initialize::new()).unwrap();
    steward.snapshot_before(& (10*SECOND)).expect("steward failed to provide snapshot");
    for index in 1..10 {
      steward.insert_fiat_event (index*SECOND, EntityId::hash_of (& index), Disturb::new ([ARENA_SIZE/3,ARENA_SIZE/3])).unwrap();
      steward.snapshot_before(& (10*SECOND)).expect("steward failed to provide snapshot");
    }
  })
}
*/
