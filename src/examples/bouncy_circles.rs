extern crate nalgebra;

use memoized_flat_time_steward as s;
use ::{TimeSteward, DeterministicRandomId, Column, ColumnId, RowId, PredictorId, Mutator,
       TimeStewardLifetimedMethods, TimeStewardStaticMethods, Accessor, MomentaryAccessor, PredictorAccessor};
use ::collision_detection::inefficient as collisions;
use ::time_functions::Trajectory;
use std::rc::Rc;
use rand::Rng;
use nalgebra::{Vector3};

use std::io::Write;
macro_rules! printlnerr(
    ($($arg:tt)*) => { {
        let r = writeln!(&mut ::std::io::stderr(), $($arg)*);
        r.expect("failed printing to stderr");
    } }
);

type Time = i64;
type SpaceCoordinate = i64;
type Trajectory = Trajectory3 <Vector2 < SpaceCoordinate>>;


const HOW_MANY_CIRCLES: i32 = 20;
const ARENA_SIZE: SpaceCoordinate = 10000000;
const SECOND: Time = 10000;

#[derive(Clone)]
struct Basics;
impl ::Basics for Basics {
  type Time = Time;
  type Constants = ();
}

#[derive(Clone)]
struct Circle {
position: Trajectory,
radius: SpaceCoordinate,
}
impl Column for Circle {
  type FieldType = Self;
  fn column_id() -> ColumnId {
    ColumnId(0x6422505ce8c8ce8e)
  }
}

type Steward = s::Steward<Basics>;

fn get_circle_id(index: i32) -> RowId {
  DeterministicRandomId::new(&(0x86ccbeb2c140cc51, index))
}

fn collision_predictor <PA: PredictorAccessor> (accessor: PA, id: RowId) {
  printlnerr!("Planning {}", id);
let ids = pa.get::<collisions::Nearness>(id).unwrap().ids.clone();
  {
  
let us = [pa.get::<collisions::Nearness>(ids [0]).unwrap(),pa.get::<collisions::Nearness>(ids [0]).unwrap()];
  }
  accessor.predict_at_time (Rc::new (move | mutator | {
    
  }));
}

pub fn testfunc() {

  let mut stew: Steward = ::TimeStewardStaticMethods::new_empty((),
                                                   vec![s::Predictor {
                                                          predictor_id: PredictorId(0x5375592f4da8682c),
                                                          column_id: collisions::Nearness::column_id(),
                                                          function: Rc::new(| accessor, id | collision_predictor (accessor, id)),}]); /*{
      printlnerr!("Planning {}", id);
      let me = pa.get::<Circle>(id).unwrap().clone();
      pa.predict_at_time(&me.time_when_next_initiates_handshake,
                         Rc::new(move |m| {
        let now = *m.now();
        let friend_id = get_philosopher_id(m.rng().gen_range(0, HOW_MANY_PHILOSOPHERS));
        let awaken_time_1 = now + m.rng().gen_range(-1, 4);
        let awaken_time_2 = now + m.rng().gen_range(-1, 7);
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
                                                        }]);*/

  stew.insert_fiat_event(0,
                         DeterministicRandomId::new(& 0),
                         Rc::new(| mutator | {
    for i in 0..HOW_MANY_CIRCLES {
let position = Vector::new (m.rng().gen_range(0, ARENA_SIZE),m.rng().gen_range(0, ARENA_SIZE));
let thingy =ARENA_SIZE/SECOND/20;
let velocity = Vector::new (m.rng().gen_range(- thingy, thingy),m.rng().gen_range(- thingy, thingy));
mass radius =m.rng().gen_range(ARENA_SIZE/30, ARENA_SIZE/15);
      m.set::<Circle>(get_circle_id(i),
                           Some(Circle {
position: position, velocity: velocity, radius: radius
                           }));
    }
  }));

  let mut snapshots = Vec:: new();
  for increment in 1..21 {
    snapshots.push (stew.snapshot_before(& (increment*100i64)));
  }
  for snapshot in snapshots.iter_mut().map (| option | option.as_mut().expect ("all these snapshots should have been valid")) {
    printlnerr!("snapshot for {}", snapshot.now());
    for index in 0..HOW_MANY_CIRCLES {
      printlnerr!("{}", snapshot.get::<Circle> (get_circle_id (index)).expect("missing circle").position);
    }
  }
  // panic!("anyway")
}

#[test]
fn actuallytest() {
  testfunc();
}
