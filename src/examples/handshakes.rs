
use inefficient_flat_time_steward as s;
use ::{TimeSteward, DeterministicRandomId, Column, ColumnId,
  RowId, Mutator, TimeStewardLifetimedMethods, Accessor,
  MomentaryAccessor, PredictorAccessor, Snapshot};
use std::rc::Rc;
use rand::Rng;

type Time = i64;

const HOW_MANY_PHILOSOPHERS: i32 = 7;

#[derive(Clone)]
struct Basics;
impl ::Basics for Basics {
  type Time = Time;
  type Constants = ();
}

#[derive(Clone)]
struct Philosopher {
  // This is sometimes in the future because
  // they muse philosophically about handshakes
  // for a while, whenever one of them happens.
  time_when_next_initiates_handshake: Time,
}
impl Column for Philosopher {
  type FieldType = Self;
  fn column_id() -> ColumnId { ColumnId(0x4084d1501468b6dd) }
}

type Steward = s::Steward<Basics>;

fn get_philosopher_id(index: i32) -> RowId {
  DeterministicRandomId::new(&(0x2302c38efb47e0d0u64, index))
}

#[test]
fn testfunc() {

  let mut stew: Steward = ::TimeSteward::new_empty((), vec![
    s::Predictor {
      predictor_id: 0x0e7f27c7643f8167,
      column_id: Philosopher::column_id(),
      function: Rc::new(|pa, whodunnit| {
        let me = pa.get::<Philosopher>(whodunnit).unwrap().clone();
        pa.predict_at_time(
          &me.time_when_next_initiates_handshake,
          Rc::new(move|m| {
            // BAD let mut rng = m.rng().clone();
            let now = *m.now();
            let friend_id = get_philosopher_id(m.rng().gen_range(0, HOW_MANY_PHILOSOPHERS));
            // TODO allow 0+
            let awaken_time_1 = now + m.rng().gen_range(-1, 4);
            let awaken_time_2 = now + m.rng().gen_range(-1, 7);
            // IF YOU SHAKE YOUR OWN HAND YOU RECOVER
            // IN THE SECOND TIME APPARENTLY
            m.set::<Philosopher>(friend_id, Some(Philosopher{
              time_when_next_initiates_handshake: awaken_time_1,
              }));
            m.set::<Philosopher>(whodunnit, Some(Philosopher{
              time_when_next_initiates_handshake: awaken_time_2,
              }));
          })
        );
      })
    }
  ]);
  
  stew.insert_fiat_event(
    0,
    DeterministicRandomId::new(&0x32e1570766e768a7u64),
    Rc::new(|m| {
      for i in 0..HOW_MANY_PHILOSOPHERS {
        m.set::<Philosopher>(get_philosopher_id(i),
          Some(Philosopher{
            time_when_next_initiates_handshake: (i+1) as Time
          }));
      }
    }));
    
  stew.snapshot_before(&50);
  
  //panic!("anyway")
}
