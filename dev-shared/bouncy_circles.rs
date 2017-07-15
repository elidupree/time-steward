//use time_steward::stewards::crossverified as s;
use time_steward::{DeterministicRandomId, Column, ColumnId, RowId, PredictorId, EventId, Accessor,
     MomentaryAccessor, PredictorAccessor, ColumnType, EventType, PredictorType};
use time_steward::support::collision_detection::simple_grid as collisions;

use time_steward::support;
use time_steward::support::time_functions::QuadraticTrajectory;
use nalgebra::Vector2;
use time_steward::support::rounding_error_tolerant_math::right_shift_round_up;


pub type Time = i64;
pub type SpaceCoordinate = i64;


pub const HOW_MANY_CIRCLES: i32 = 20;
pub const ARENA_SIZE_SHIFT: u32 = 20;
pub const ARENA_SIZE: SpaceCoordinate = 1 << 20;
pub const GRID_SIZE_SHIFT: u32 = ARENA_SIZE_SHIFT - 3;
// pub const GRID_SIZE: SpaceCoordinate = 1 << GRID_SIZE_SHIFT;
pub const MAX_DISTANCE_TRAVELED_AT_ONCE: SpaceCoordinate = ARENA_SIZE << 4;
pub const TIME_SHIFT: u32 = 20;
pub const SECOND: Time = 1 << TIME_SHIFT;

time_steward_basics!(pub struct Basics {
  type Time = Time;
  type Constants = ();
  type IncludedTypes = TimeStewardTypes;
});
#[derive (Clone, PartialEq, Eq, Serialize, Deserialize, Debug)]
pub struct CollisionBasics;
impl support::collision_detection::Basics for CollisionBasics {
  type StewardBasics = Basics;
  type DetectorId = ();
  fn nearness_column_id() -> ColumnId {
    ColumnId(0x89ebc3ba9a24c286)
  }
}
impl collisions::Basics for CollisionBasics {
  fn get_bounds<A: MomentaryAccessor<Basics = Basics>>(accessor: &A,
                                                       who: RowId,
                                                       _: ())
                                                       -> collisions::Bounds {
    let (circle, time) = accessor.data_and_last_change::<Circle>(who)
      .expect("no circle despite is being collision detected as a circle");
    let center = circle.position.updated_by(accessor.now() - time).unwrap().evaluate();
    collisions::Bounds {
      min: [(center[0] - circle.radius) >> GRID_SIZE_SHIFT,
            (center[1] - circle.radius) >> GRID_SIZE_SHIFT],
      max: [right_shift_round_up(center[0] + circle.radius, GRID_SIZE_SHIFT),
            right_shift_round_up(center[1] + circle.radius, GRID_SIZE_SHIFT)],
    }
  }
  fn when_escapes<A: Accessor<Basics = Basics>>(accessor: &A,
                                                who: RowId,
                                                bounds: collisions::Bounds,
                                                _: ())
                                                -> Option<Time> {
    let (circle, time) = accessor.data_and_last_change::<Circle>(who)
      .expect("no circle despite is being collision detected as a circle");
    circle.position.approximately_when_escapes(time.clone(),
                                               accessor.unsafe_now().clone(),
                                               [[(bounds.min[0] << GRID_SIZE_SHIFT) +
                                                 circle.radius,
                                                 (bounds.max[0] << GRID_SIZE_SHIFT) -
                                                 circle.radius],
                                                [(bounds.min[1] << GRID_SIZE_SHIFT) +
                                                 circle.radius,
                                                 (bounds.max[1] << GRID_SIZE_SHIFT) -
                                                 circle.radius]])
  }
}

pub type Nearness = support::collision_detection::Nearness<CollisionBasics>;

#[derive (Clone, PartialEq, Eq, Serialize, Deserialize, Debug)]
pub struct Circle {
  pub position: QuadraticTrajectory,
  pub radius: SpaceCoordinate,
}
impl Column for Circle {
  type FieldType = Self;
  fn column_id() -> ColumnId {
    ColumnId(0x6422505ce8c8ce8e)
  }
}
#[derive (Clone, PartialEq, Eq, Serialize, Deserialize, Debug)]
pub struct Intersection {
  pub induced_acceleration: Vector2<SpaceCoordinate>,
}
impl Column for Intersection {
  type FieldType = Self;
  fn column_id() -> ColumnId {
    ColumnId(0x9357f021339198a1)
  }
}


pub type TimeStewardTypes = (ColumnType<Circle>,
                         ColumnType<Intersection>,
                         PredictorType<CollisionPredictor>,
                         PredictorType<BoundaryPredictor>,
                         EventType<Initialize>,
                         EventType<Collision>,
                         EventType<BoundaryCollision>,
                         EventType<Disturb>,
                         collisions::TimeStewardTypes<CollisionBasics>);

pub fn get_circle_id(index: i32) -> RowId {
  DeterministicRandomId::new(&(0x86ccbeb2c140cc51u64, index))
}

pub fn collision_predictor<PA: PredictorAccessor<Basics = Basics>>(accessor: &mut PA, id: RowId) {
  let ids = Nearness::get_ids(accessor, id).0;
  let time;
  {

    let us = (accessor.data_and_last_change::<Circle>(ids[0])
      .expect("a nearness exists for a circle that doesn't"),
              accessor.data_and_last_change::<Circle>(ids[1])
      .expect("a nearness exists for a circle that doesn't"));

    let relationship = accessor.get::<Intersection>(id);
    time = QuadraticTrajectory::approximately_when_distance_passes((us.0).0.radius +
                                                                   (us.1).0.radius,
                                                                   if relationship.is_none() {
                                                                     -1
                                                                   } else {
                                                                     1
                                                                   },
                                                                   ((us.0).1.clone(),
                                                                    &(us.0).0.position),
                                                                   ((us.1).1.clone(),
                                                                    &(us.1).0.position));
    // println!("Planning for {} At {}, {}", id, (us.0).1, (us.1).1);
    if time.is_none() && relationship.is_some() {
      panic!(" fail {:?} {:?} {:?} {:?}", id, ids, us, relationship)
    }
  }
  if let Some(yes) = time {
    // println!(" planned for {}", &yes);
    accessor.predict_at_time(yes, Collision::new(id));
  }
}

time_steward_event! (
  pub struct Collision {id: RowId}, Basics, EventId (0x2312e29e341a2495),
  | &self, mutator | {
    let new_relationship;
    let mut new;
    let ids = Nearness::get_ids(mutator, self.id).0;
    {
      let relationship = mutator.get::<Intersection>(self.id).clone();
      let us = (mutator.data_and_last_change::<Circle>(ids[0])
                    .expect("a nearness exists for a circle that \
                             doesn't (event)"),
             mutator.data_and_last_change::<Circle>(ids[1])
                    .expect("a nearness exists for a circle that \
                             doesn't (event)"));
      new = ((us.0).0.clone(), (us.1).0.clone());
      new.0.position.update_by(mutator.now() - (us.0).1);
      new.1.position.update_by(mutator.now() - (us.1).1);
      if let Some(intersection) = relationship {
        new.0
          .position
          .add_acceleration(-intersection.induced_acceleration);
        new.1
          .position
          .add_acceleration(intersection.induced_acceleration);
        new_relationship = None;
        //println!("Parted {} At {}", self.id, mutator.now());
      } else {
        let acceleration = (new.0.position.evaluate() -
                           new.1.position.evaluate()) *
                          (ARENA_SIZE * 4 /
                           (new.0.radius + new.1.radius));
        new.0.position.add_acceleration(acceleration);
        new.1.position.add_acceleration(-acceleration);
        new_relationship = Some(Intersection {
         induced_acceleration: acceleration,
        });

        //println!("Joined {} At {}", self.id, mutator.now());
      }
    }
    mutator.set::<Intersection>(self.id, new_relationship);
    mutator.set::<Circle>(ids[0], Some(new.0));
    mutator.set::<Circle>(ids[1], Some(new.1));
  }
);

pub fn boundary_predictor<PA: PredictorAccessor<Basics = Basics>>(accessor: &mut PA, id: RowId) {
  let time;
  {
    let arena_center = QuadraticTrajectory::new(TIME_SHIFT,
                                                MAX_DISTANCE_TRAVELED_AT_ONCE,
                                                [ARENA_SIZE / 2, ARENA_SIZE / 2, 0, 0, 0, 0]);
    let me = accessor.data_and_last_change::<Circle>(id)
      .expect("a prediction was recorded for a circle that doesn't exist");

    let relationship = accessor.get::<Intersection>(id);
    time = QuadraticTrajectory::approximately_when_distance_passes(ARENA_SIZE - me.0.radius,
                                                                   if relationship.is_some() {
                                                                     -1
                                                                   } else {
                                                                     1
                                                                   },
                                                                   (me.1.clone(),
                                                                    &(me.0.position)),
                                                                   (0, &(arena_center)));
  }
  if let Some(yes) = time {
    // println!(" planned for {}", &yes);
    accessor.predict_at_time(yes, BoundaryCollision::new(id));
  }
}

time_steward_event! (
  pub struct BoundaryCollision {id: RowId}, Basics, EventId (0x59732d675b2329ad),
  | &self, mutator | {
    let new_relationship;
    let mut new;
    {
      let relationship = mutator.get::<Intersection>(self.id).clone();
      let me = mutator.data_and_last_change::<Circle>(self.id)
                   .expect("a an event was recorded for a circle \
                            that doesn't exist)");
      new = me.0.clone();
      new.position.update_by(mutator.now() - me.1);
      if let Some(intersection) = relationship {
        new.position
          .add_acceleration(-intersection.induced_acceleration);
        new_relationship = None;
      } else {
      let acceleration = -(new.position.evaluate() -
                            Vector2::new(ARENA_SIZE / 2,
                                         ARENA_SIZE / 2)) *
                          (ARENA_SIZE * 400 / (ARENA_SIZE - me.0.radius));
        new.position.add_acceleration(acceleration);
        new_relationship = Some(Intersection {
         induced_acceleration: acceleration,
      });

      }
    }
    mutator.set::<Intersection>(self.id, new_relationship);
    mutator.set::<Circle>(self.id, Some(new));
  }
);

time_steward_predictor! (pub struct CollisionPredictor, Basics, PredictorId(0x5375592f4da8682c), watching Nearness, fn collision_predictor);
time_steward_predictor! (pub struct BoundaryPredictor, Basics, PredictorId(0x87d8a4a095350d30), watching Circle, fn boundary_predictor);

time_steward_event! (
  pub struct Initialize {}, Basics, EventId (0xa2a17317b84f96e5),
  | &self, mutator | {
    for i in 0..HOW_MANY_CIRCLES {
      let thingy = ARENA_SIZE / 20;
      let radius = mutator.gen_range(ARENA_SIZE / 30, ARENA_SIZE / 15);
      let id = get_circle_id(i);

      let position =
      QuadraticTrajectory::new(TIME_SHIFT,
                              MAX_DISTANCE_TRAVELED_AT_ONCE,
                              [mutator.gen_range(0, ARENA_SIZE),
                               mutator.gen_range(0, ARENA_SIZE),
                               mutator.gen_range(-thingy, thingy),
                               mutator.gen_range(-thingy, thingy),
                               0,
                               0]);
      mutator.set::<Circle>(id,
                         Some(Circle {
                           position: position,
                           radius: radius,
                         }));
      collisions::insert::<CollisionBasics, _>(mutator, id, ());
    }
  }
);

time_steward_event! (
  pub struct Disturb {coordinates: [SpaceCoordinate; 2]}, Basics, EventId(0x058cb70d89116605),
  | &self, mutator | {
    let mut best_id = RowId::new (& 0u8);
    let mut best_distance_squared = i64::max_value();
    for i in 0..HOW_MANY_CIRCLES {
      let id = get_circle_id(i);
      let (circle, time) = mutator.data_and_last_change::<Circle>(id)
          .expect("missing circle")
          .clone();
      let position = circle.position.updated_by(mutator.now() - time).unwrap().evaluate();
      let distance_squared = (self.coordinates [0] - position [0]) * (self.coordinates [0] - position [0]) + (self.coordinates [1] - position [1]) * (self.coordinates [1] - position [1]);
      if distance_squared <best_distance_squared {
        best_distance_squared = distance_squared;
        best_id = id;
      }
    }    let mut new;{
    let (a, time) = mutator.data_and_last_change::<Circle>(best_id)
              .expect("missing circle")
              .clone();
    new =a.clone();
    new.position.update_by(mutator.now() - time);
    let impulse = -(new.position.evaluate() -
                            Vector2::new(ARENA_SIZE / 2,
                                         ARENA_SIZE / 2)) *
                          (ARENA_SIZE * 4 / (ARENA_SIZE ));
    new.position.add_velocity(impulse);}
    mutator.set::<Circle>(best_id, Some(new));
  }
);
