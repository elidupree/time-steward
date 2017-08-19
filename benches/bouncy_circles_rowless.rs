#![feature (test)]

extern crate test;
#[macro_use]
extern crate time_steward;

#[macro_use]
extern crate glium;

extern crate nalgebra;
extern crate docopt;

extern crate serde;
#[macro_use]
extern crate serde_derive;


use test::Bencher;

use time_steward::DeterministicRandomId;
use time_steward::rowless::api::{QueryOffset, TypedDataTimelineHandleTrait, ExtendedTime, Basics as BasicsTrait};
use time_steward::rowless::stewards::simple_flat as steward_module;
use steward_module::{TimeSteward, IncrementalTimeSteward, ConstructibleTimeSteward, Accessor, MomentaryAccessor, SnapshotAccessor, DataTimelineHandle, simple_timeline};
use simple_timeline::{SimpleTimeline, ConstantTimeline, GetValue, query_constant_timeline, query_simple_timeline, modify_simple_timeline, unmodify_simple_timeline};

#[path = "../dev-shared/bouncy_circles_rowless.rs"] mod bouncy_circles;
use bouncy_circles::*;

#[bench]
fn bouncy_circles_straightforward(bencher: &mut Bencher) {
  bencher.iter(|| {
  
    let global_timeline = <Basics as BasicsTrait>::GlobalTimeline::new ({
      let mut circles = Vec::new();
      for index in 0..HOW_MANY_CIRCLES {
        circles.push (DataTimelineHandle::new (SimpleTimeline::new ()));
      }
      circles
    });

    let mut steward: Steward = Steward::from_global_timeline (global_timeline);
    steward.insert_fiat_event(0, DeterministicRandomId::new(&0), Initialize {}).unwrap();
    // make sure to check for inefficiencies in the forgetting code
    steward.forget_before(& 0);
    steward.snapshot_before(& SECOND).expect("steward failed to provide snapshot");
  })
}


#[bench]
fn bouncy_circles_disturbed (bencher: &mut Bencher) {
  bencher.iter(|| {
    let global_timeline = <Basics as BasicsTrait>::GlobalTimeline::new ({
      let mut circles = Vec::new();
      for index in 0..HOW_MANY_CIRCLES {
        circles.push (DataTimelineHandle::new (SimpleTimeline::new ()));
      }
      circles
    });

    let mut steward: Steward = Steward::from_global_timeline (global_timeline);
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
