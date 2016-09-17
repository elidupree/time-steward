#![feature (test)]
#![feature (plugin, custom_derive)]
#![plugin (serde_macros)]

extern crate test;

#[macro_use]
extern crate time_steward;

#[macro_use]
extern crate glium;

extern crate nalgebra;
extern crate rustc_serialize;
extern crate docopt;

use test::Bencher;

use time_steward::{TimeSteward, TimeStewardFromConstants, DeterministicRandomId};

use time_steward::stewards::amortized;

#[path = "../dev-shared/bouncy_circles.rs"] mod bouncy_circles;
use bouncy_circles::*;

#[bench]
fn bouncy_circles_straightforward(bencher: &mut Bencher) {
  bencher.iter(|| {
    let mut steward: amortized::Steward<Basics> = amortized::Steward::from_constants(());
    steward.insert_fiat_event(0, DeterministicRandomId::new(&0), Initialize::new()).unwrap();
    steward.snapshot_before(& SECOND).expect("steward failed to provide snapshot");
  })
}

#[bench]
fn bouncy_circles_disturbed (bencher: &mut Bencher) {
  bencher.iter(|| {
    let mut steward: amortized::Steward<Basics> = amortized::Steward::from_constants(());
    steward.insert_fiat_event(0, DeterministicRandomId::new(&0), Initialize::new()).unwrap();
    for index in 1..10 {
      steward.insert_fiat_event (index*SECOND/10, DeterministicRandomId::new (& index), Disturb::new ([ARENA_SIZE/3,ARENA_SIZE/3])).unwrap();
    }
    steward.snapshot_before(& SECOND).expect("steward failed to provide snapshot");
  })
}

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
