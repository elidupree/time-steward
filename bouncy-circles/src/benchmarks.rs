use crate::physics::*;
use time_steward::{ConstructibleTimeSteward, EntityId};

pub fn bouncy_circles_straightforward<
  Steward: ConstructibleTimeSteward<(), SimulationSpec = BouncyCirclesSpec>,
>() {
  let mut steward = Steward::from_construct_globals((), BouncyCirclesSpec);
  steward
    .insert_fiat_event::<Initialize>(0, EntityId::hash_of(&0), (), ())
    .unwrap();
  for index in 0..1000 {
    let time = 10 * SECOND * index / 1000;
    steward
      .snapshot_before(time)
      .expect("steward failed to provide snapshot");
    steward.forget_before(time);
  }
}

pub fn bouncy_circles_disturbed<
  Steward: ConstructibleTimeSteward<(), SimulationSpec = BouncyCirclesSpec>,
>() {
  let mut steward = Steward::from_construct_globals((), BouncyCirclesSpec);
  steward
    .insert_fiat_event::<Initialize>(0, EntityId::hash_of(&0), (), ())
    .unwrap();
  for index in 1..10 {
    steward
      .insert_fiat_event::<Disturb>(
        index * SECOND,
        EntityId::hash_of(&index),
        Disturb {
          coordinates: [ARENA_SIZE / 3, ARENA_SIZE / 3],
        },
        (),
      )
      .unwrap();
  }
  for index in 0..1000 {
    let time = 10 * SECOND * index / 1000;
    steward
      .snapshot_before(time)
      .expect("steward failed to provide snapshot");
    steward.forget_before(time);
  }
}
