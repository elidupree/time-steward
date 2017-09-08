#![feature (test)]

extern crate test;
#[macro_use]
extern crate time_steward;

#[macro_use]
extern crate glium;

extern crate nalgebra;
extern crate rand;
extern crate docopt;

extern crate serde;
#[macro_use]
extern crate serde_derive;


use test::Bencher;

use time_steward::{DeterministicRandomId};
use time_steward::rowless::api::{PersistentTypeId, ListedType, PersistentlyIdentifiedType, DataTimelineCellTrait, Basics as BasicsTrait};
use time_steward::rowless::stewards::{simple_full as steward_module};
use steward_module::{TimeSteward, ConstructibleTimeSteward, Event, DataTimelineCell, EventAccessor, FutureCleanupAccessor, SnapshotAccessor, simple_timeline};
use simple_timeline::{SimpleTimeline, GetVarying};

#[path = "../dev-shared/bouncy_circles_rowless.rs"] mod bouncy_circles;
use bouncy_circles::*;

#[bench]
fn bouncy_circles_straightforward(bencher: &mut Bencher) {
  bencher.iter(|| {
    let mut steward: Steward = Steward::from_globals (make_globals());
    steward.insert_fiat_event(0, DeterministicRandomId::new(&0), Initialize {}).unwrap();
    // make sure to check for inefficiencies in the forgetting code
    steward.forget_before(& 0);
    steward.snapshot_before(& SECOND).expect("steward failed to provide snapshot");
  })
}


#[bench]
fn bouncy_circles_disturbed (bencher: &mut Bencher) {
  bencher.iter(|| {
    let mut steward: Steward = Steward::from_globals (make_globals());
    steward.insert_fiat_event(0, DeterministicRandomId::new(&0), Initialize {}).unwrap();
    for index in 1..10 {
      steward.insert_fiat_event (index*SECOND/10, DeterministicRandomId::new (& index), Disturb{ coordinates: [ARENA_SIZE/3,ARENA_SIZE/3]}).unwrap();
    }
    steward.forget_before(& 0);
    steward.snapshot_before(& SECOND).expect("steward failed to provide snapshot");
  })
}
/*
#[bench]
fn bouncy_circles_disturbed_retroactive (bencher: &mut Bencher) {
  bencher.iter(|| {
    let mut steward: amortized::Steward<Basics> = amortized::Steward::from_constants(());
    steward.insert_fiat_event(0, DeterministicRandomId::new(&0), Initialize::new()).unwrap();
    steward.snapshot_before(& SECOND).expect("steward failed to provide snapshot");
    for index in 1..10 {
      steward.insert_fiat_event (index*SECOND/10, DeterministicRandomId::new (& index), Disturb::new ([ARENA_SIZE/3,ARENA_SIZE/3])).unwrap();
      steward.snapshot_before(& SECOND).expect("steward failed to provide snapshot");
    }
  })
}
*/
