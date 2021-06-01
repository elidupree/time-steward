use nalgebra::Vector2;
use rand::Rng;
use rand_pcg::Pcg64Mcg;
use serde::{Deserialize, Serialize};

use time_steward::support::trajectories;
use time_steward::type_utils::list_of_types::ListedType;
use time_steward::type_utils::{PersistentTypeId, PersistentlyIdentifiedType};
use time_steward::{
  ConstructGlobals, EntityHandleKind, EntityKind, EventAccessor, Globals,
  GlobalsConstructionAccessor, ReadAccess, TypedHandleRef, Wake, WriteAccess,
};
use time_steward::{SimulationSpec, TypedHandle};

pub type Time = i64;
pub type SpaceCoordinate = i32;
pub type QuadraticTrajectory = trajectories::QuadraticTrajectory<Vector2<SpaceCoordinate>>;

pub const HOW_MANY_CIRCLES: usize = 20;
pub const ARENA_SIZE_SHIFT: u32 = 20;
pub const ARENA_SIZE: SpaceCoordinate = 1 << ARENA_SIZE_SHIFT;
// pub const GRID_SIZE_SHIFT: u32 = ARENA_SIZE_SHIFT - 3;
// pub const GRID_SIZE: SpaceCoordinate = 1 << GRID_SIZE_SHIFT;
pub const TIME_SHIFT: u32 = 20;
pub const STATIC_TIME_SHIFT: u32 = TIME_SHIFT;
pub const SECOND: Time = 1 << TIME_SHIFT;

pub struct BouncyCirclesSpec;
impl SimulationSpec for BouncyCirclesSpec {
  type Time = Time;
  type Globals<H: EntityHandleKind> = BouncyCirclesGlobals<H>;
  type Types = (
    ListedType<Circle>,
    ListedType<Relationship>,
    ListedType<Initialize>,
    ListedType<Disturb>,
    //collisions::simple_grid::Types<Space>,
  );
}

#[derive(Clone, PartialEq, Eq, Serialize, Deserialize, Debug)]
#[serde(bound = "")]
pub struct BouncyCirclesGlobals<H: EntityHandleKind> {
  pub circles: Vec<TypedHandle<Circle, H>>,
  //pub detector: EntityCell<SimpleTimeline<DataHandle<SimpleGridDetector<Space>>, Steward>>,
}

#[derive(Clone, PartialEq, Eq, Serialize, Deserialize, Debug)]
pub struct CircleImmutable {
  pub index: usize,
  pub radius: SpaceCoordinate,
}
#[derive(Clone, PartialEq, Eq, Serialize, Deserialize, Debug)]
#[serde(bound = "")]
pub struct CircleMutable<H: EntityHandleKind> {
  pub position: QuadraticTrajectory,
  pub relationships: Vec<TypedHandle<Relationship, H>>,
  pub boundary_induced_acceleration: Option<Vector2<SpaceCoordinate>>,
  //pub collision_data: Option<collisions::simple_grid::DetectorDataPerObject<Space>>,
}
pub struct Circle;
impl PersistentlyIdentifiedType for Circle {
  const ID: PersistentTypeId = PersistentTypeId(0xd711cc7240c71607);
}
impl EntityKind for Circle {
  type ImmutableData<H: EntityHandleKind> = CircleImmutable;
  type MutableData<H: EntityHandleKind> = CircleMutable<H>;
}

#[derive(Clone, PartialEq, Eq, Serialize, Deserialize, Debug)]
#[serde(bound = "")]
pub struct RelationshipImmutable<H: EntityHandleKind> {
  pub circles: [TypedHandle<Circle, H>; 2],
}
#[derive(Clone, PartialEq, Eq, Serialize, Deserialize, Debug)]
pub struct RelationshipMutable {
  pub induced_acceleration: Option<Vector2<SpaceCoordinate>>,
}
pub struct Relationship;
impl PersistentlyIdentifiedType for Relationship {
  const ID: PersistentTypeId = PersistentTypeId(0xa1010b5e80c3465a);
}
impl EntityKind for Relationship {
  type ImmutableData<H: EntityHandleKind> = RelationshipImmutable<H>;
  type MutableData<H: EntityHandleKind> = RelationshipMutable;
}

impl Wake<BouncyCirclesSpec> for Circle {
  fn wake<A: EventAccessor<SimulationSpec = BouncyCirclesSpec>>(
    accessor: &mut A,
    this: TypedHandleRef<Self, A::EntityHandleKind>,
  ) {
    let now = *accessor.now();
    let mut new = this.write(accessor);
    if let Some(induced_acceleration) = new.boundary_induced_acceleration {
      new
        .position
        .add_acceleration(now, STATIC_TIME_SHIFT, -induced_acceleration)
        .unwrap();
      new.boundary_induced_acceleration = None;
    } else {
      let acceleration = -(new.position.value(now, STATIC_TIME_SHIFT).unwrap()
        - Vector2::new(ARENA_SIZE / 2, ARENA_SIZE / 2))
        * (ARENA_SIZE * 1600 / (ARENA_SIZE - this.radius));
      new
        .position
        .add_acceleration(now, STATIC_TIME_SHIFT, acceleration)
        .unwrap();
      new.boundary_induced_acceleration = Some(acceleration);
    }
    drop(new);
    trajectory_changed(accessor, this);
  }
}

impl Wake<BouncyCirclesSpec> for Relationship {
  fn wake<A: EventAccessor<SimulationSpec = BouncyCirclesSpec>>(
    accessor: &mut A,
    this: TypedHandleRef<Self, A::EntityHandleKind>,
  ) {
    let circles = &this.circles;
    let now = *accessor.now();
    let mut new = circles
      .each_ref()
      .map(|c| c.read(accessor).position.clone());
    // let new_difference =
    //   new[0].value(now, STATIC_TIME_SHIFT).unwrap() - new[0].value(now, STATIC_TIME_SHIFT).unwrap();
    // println!(
    //   "event with error {:?}",
    //   (new_difference.dot(&new_difference) as f64).sqrt()
    //     - (circles[0].radius + circles[1].radius) as f64
    // );
    let mut this_mutable = this.write(accessor);
    if let Some(induced_acceleration) = this_mutable.induced_acceleration {
      new[0]
        .add_acceleration(now, STATIC_TIME_SHIFT, -induced_acceleration)
        .unwrap();
      new[1]
        .add_acceleration(now, STATIC_TIME_SHIFT, induced_acceleration)
        .unwrap();
      this_mutable.induced_acceleration = None;
      //println!("Parted {} At {}", self.id, mutator.now());
    } else {
      let acceleration = (new[0].value(now, STATIC_TIME_SHIFT).unwrap()
        - new[1].value(now, STATIC_TIME_SHIFT).unwrap())
        * (ARENA_SIZE * 16 / (circles[0].radius + circles[1].radius));
      new[0]
        .add_acceleration(now, STATIC_TIME_SHIFT, acceleration)
        .unwrap();
      new[1]
        .add_acceleration(now, STATIC_TIME_SHIFT, -acceleration)
        .unwrap();
      this_mutable.induced_acceleration = Some(acceleration);
      //println!("Joined {} At {}", self.id, mutator.now());
    }

    drop(this_mutable);
    for (circle, new_position) in circles.iter().zip(new) {
      circle.write(accessor).position = new_position;
    }
    for circle in circles {
      trajectory_changed(accessor, circle.borrow());
    }
  }
}

fn trajectory_changed<A: EventAccessor<SimulationSpec = BouncyCirclesSpec>>(
  accessor: &mut A,
  circle: TypedHandleRef<Circle, A::EntityHandleKind>,
) {
  let relationships = circle.read(accessor).relationships.clone();
  for relationship in relationships {
    update_relationship_change_schedule(accessor, relationship.borrow());
  }
  update_boundary_change_schedule(accessor, circle);
  // SimpleGridDetector::changed_course(
  //   accessor,
  //   &query(accessor, &accessor.globals().detector),
  //   circle,
  // );
}

fn update_relationship_change_schedule<A: EventAccessor<SimulationSpec = BouncyCirclesSpec>>(
  accessor: &mut A,
  relationship: TypedHandleRef<Relationship, A::EntityHandleKind>,
) {
  let circles = &relationship.circles;
  let us = circles.each_ref().map(|circle| circle.read(accessor));

  let difference = &us[1].position - &us[0].position;
  let radius = circles[0].radius + circles[1].radius;
  let relationship_mutable = relationship.read(accessor);
  let change_time = if relationship_mutable.induced_acceleration.is_none() {
    difference.next_time_magnitude_significantly_lt(
      [*accessor.now(), Time::MAX],
      STATIC_TIME_SHIFT,
      radius - 2,
    )
  } else {
    difference.next_time_magnitude_significantly_gt(
      [*accessor.now(), Time::MAX],
      STATIC_TIME_SHIFT,
      radius + 2,
    )
  };

  if change_time.is_none() && relationship_mutable.induced_acceleration.is_some() {
    panic!(
      " fail {:?} {:?} {:?}",
      relationship,
      *relationship_mutable,
      us.each_ref().map(|c| &**c)
    )
  }

  drop((us, relationship_mutable));
  accessor.set_schedule(relationship, change_time);
}

fn update_boundary_change_schedule<A: EventAccessor<SimulationSpec = BouncyCirclesSpec>>(
  accessor: &mut A,
  circle: TypedHandleRef<Circle, A::EntityHandleKind>,
) {
  let arena_center = QuadraticTrajectory::constant(Vector2::new(ARENA_SIZE / 2, ARENA_SIZE / 2));
  let circle_mutable = circle.read(accessor);

  let difference = &circle_mutable.position - arena_center;
  let radius = ARENA_SIZE - circle.radius;
  let change_time = if circle_mutable.boundary_induced_acceleration.is_some() {
    difference.next_time_magnitude_significantly_lt(
      [*accessor.now(), Time::max_value()],
      STATIC_TIME_SHIFT,
      radius - 2,
    )
  } else {
    difference.next_time_magnitude_significantly_gt(
      [*accessor.now(), Time::max_value()],
      STATIC_TIME_SHIFT,
      radius + 2,
    )
  };

  drop(circle_mutable);
  accessor.set_schedule(circle, change_time);
}

impl ConstructGlobals<BouncyCirclesSpec> for BouncyCirclesSpec {
  fn construct_globals<A: GlobalsConstructionAccessor<SimulationSpec = BouncyCirclesSpec>>(
    self,
    accessor: &mut A,
  ) -> Globals<BouncyCirclesSpec, A::EntityHandleKind> {
    let mut generator = Pcg64Mcg::new(0x0);
    let thingy = ARENA_SIZE / 20;

    let circles = (0..HOW_MANY_CIRCLES)
      .map(|index| {
        let radius = generator.gen_range(ARENA_SIZE / 30..ARENA_SIZE / 15);
        let mut position = QuadraticTrajectory::constant(Vector2::new(
          generator.gen_range(0..ARENA_SIZE),
          generator.gen_range(0..ARENA_SIZE),
        ));
        position
          .set_velocity(
            0,
            STATIC_TIME_SHIFT,
            Vector2::new(
              generator.gen_range(-thingy..=thingy),
              generator.gen_range(-thingy..=thingy),
            ),
          )
          .unwrap();

        accessor.create_entity::<Circle>(
          CircleImmutable { index, radius },
          CircleMutable {
            position,
            relationships: Vec::new(),
            boundary_induced_acceleration: None,
          },
        )
      })
      .collect();

    BouncyCirclesGlobals { circles }
  }
}

#[derive(Clone, PartialEq, Eq, Serialize, Deserialize, Debug)]
pub struct Initialize;
impl EntityKind for Initialize {
  type ImmutableData<H: EntityHandleKind> = ();
  type MutableData<H: EntityHandleKind> = ();
}
impl PersistentlyIdentifiedType for Initialize {
  const ID: PersistentTypeId = PersistentTypeId(0xbf7ba1ff2ab76640);
}

impl Wake<BouncyCirclesSpec> for Initialize {
  fn wake<A: EventAccessor<SimulationSpec = BouncyCirclesSpec>>(
    accessor: &mut A,
    _this: TypedHandleRef<Self, A::EntityHandleKind>,
  ) {
    // set(
    //   accessor,
    //   &accessor.globals().detector,
    //   SimpleGridDetector::new(accessor, Space, (ARENA_SIZE >> 2) as collisions::Coordinate),
    // );
    let circles = accessor.globals().circles.clone();

    for first in &circles {
      update_boundary_change_schedule(accessor, first.borrow());
      for second in &circles {
        if first < second {
          let relationship = accessor.create_entity::<Relationship>(
            RelationshipImmutable {
              circles: [first.clone(), second.clone()],
            },
            RelationshipMutable {
              induced_acceleration: None::<Vector2<SpaceCoordinate>>,
            },
          );
          first
            .write(accessor)
            .relationships
            .push(relationship.clone());
          second
            .write(accessor)
            .relationships
            .push(relationship.clone());
          update_relationship_change_schedule(accessor, relationship.borrow());
        }
      }
    }
    // for index in 0..HOW_MANY_CIRCLES {
    //   SimpleGridDetector::insert(
    //     accessor,
    //     &query(accessor, &accessor.globals().detector),
    //     &circles[index],
    //     None,
    //   );
    // }
  }
}

#[derive(Clone, PartialEq, Eq, Serialize, Deserialize, Debug)]
pub struct Disturb {
  pub coordinates: [SpaceCoordinate; 2],
}
impl EntityKind for Disturb {
  type ImmutableData<H: EntityHandleKind> = Disturb;
  type MutableData<H: EntityHandleKind> = ();
}
impl PersistentlyIdentifiedType for Disturb {
  const ID: PersistentTypeId = PersistentTypeId(0xb8bbf65eaaf08d0e);
}

impl Wake<BouncyCirclesSpec> for Disturb {
  fn wake<A: EventAccessor<SimulationSpec = BouncyCirclesSpec>>(
    accessor: &mut A,
    this: TypedHandleRef<Self, A::EntityHandleKind>,
  ) {
    //println!("Disturb {:?}", this.coordinates);
    let now = *accessor.now();
    let circles = accessor.globals().circles.clone();
    let best_circle = circles
      .into_iter()
      .min_by_key(|circle| {
        let mutable = circle.read(accessor);
        let position = mutable.position.value(now, STATIC_TIME_SHIFT).unwrap();
        let [dx, dy] = [
          (this.coordinates[0] - position[0]) as i64,
          (this.coordinates[1] - position[1]) as i64,
        ];
        dx * dx + dy * dy
      })
      .unwrap();

    let impulse = -(best_circle
      .read(accessor)
      .position
      .value(now, STATIC_TIME_SHIFT)
      .unwrap()
      - Vector2::new(ARENA_SIZE / 2, ARENA_SIZE / 2))
      * (ARENA_SIZE * 4 / (ARENA_SIZE));

    best_circle
      .write(accessor)
      .position
      .add_velocity(now, TIME_SHIFT, impulse)
      .unwrap();
    trajectory_changed(accessor, best_circle.borrow());
  }
}

// fn to_collision_space(coordinate: SpaceCoordinate) -> collisions::Coordinate {
//   (coordinate as collisions::Coordinate).wrapping_sub(1u64 << 63)
// }
// fn from_collision_space(coordinate: collisions::Coordinate) -> SpaceCoordinate {
//   (coordinate.wrapping_add(1u64 << 63)) as SpaceCoordinate
// }
//
// #[derive(Clone, PartialEq, Eq, Serialize, Deserialize, Debug)]
// pub struct Space;
// impl PersistentlyIdentifiedType for Space {
//   const ID: PersistentTypeId = PersistentTypeId(0x879511343e48addd);
// }
// impl collisions::Space for Space {
//   type SimulationSpec = BouncyCirclesSpec;
//   type Object = Circle;
//   type DetectorDataPerObject = collisions::simple_grid::DetectorDataPerObject<Self>;
//   type UniqueId = usize;
//
//   const DIMENSIONS: NumDimensions = 2;
//
//   // An Object generally has to store some opaque data for the collision detector.
//   // It would normally include a DataHandle to a tree node.
//   // These are getter and setter methods for that data.
//   fn get_detector_data<A: Accessor<Steward = Self::Steward>>(
//     &self,
//     accessor: &A,
//     object: &DataHandle<Self::Object>,
//   ) -> Option<Self::DetectorDataPerObject> {
//     query(accessor, &object.varying).collision_data
//   }
//   fn set_detector_data<A: EventAccessor<Steward = Self::Steward>>(
//     &self,
//     accessor: &A,
//     object: &DataHandle<Self::Object>,
//     data: Option<Self::DetectorDataPerObject>,
//   ) {
//     modify(accessor, &object.varying, |varying| {
//       varying.collision_data = data
//     });
//   }
//   fn unique_id<A: EventAccessor<Steward = Self::Steward>>(
//     &self,
//     _accessor: &A,
//     object: &DataHandle<Self::Object>,
//   ) -> Self::UniqueId {
//     object.index
//   }
//
//   fn current_bounding_box<A: EventAccessor<Steward = Self::Steward>>(
//     &self,
//     accessor: &A,
//     object: &DataHandle<Self::Object>,
//   ) -> BoundingBox<Self> {
//     let varying = tracking_query(accessor, &object.varying);
//     let center = varying
//       .position
//       .value(*accessor.now(), STATIC_TIME_SHIFT)
//       .unwrap();
//     let effective_radius = object.radius + 16; // just correcting for leeway in next_time_possibly_outside_bounds
//     BoundingBox {
//       bounds: [
//         [
//           to_collision_space(center[0] - effective_radius),
//           to_collision_space(center[0] + effective_radius),
//         ],
//         [
//           to_collision_space(center[1] - effective_radius),
//           to_collision_space(center[1] + effective_radius),
//         ],
//       ],
//       _marker: PhantomData,
//     }
//   }
//   fn when_escapes<A: EventAccessor<Steward = Self::Steward>>(
//     &self,
//     accessor: &A,
//     object: &DataHandle<Self::Object>,
//     bounds: BoundingBox<Self>,
//   ) -> Option<<<Self::Steward as TimeSteward>::SimulationSpec as SimulationSpecTrait>::Time> {
//     let varying = tracking_query(accessor, &object.varying);
//     varying.position.next_time_possibly_outside_bounds(
//       [*accessor.now(), Time::max_value()],
//       STATIC_TIME_SHIFT,
//       [
//         Vector2::new(
//           from_collision_space(bounds.bounds[0][0]) + object.radius,
//           from_collision_space(bounds.bounds[1][0]) + object.radius,
//         ),
//         Vector2::new(
//           from_collision_space(bounds.bounds[0][1]) - object.radius,
//           from_collision_space(bounds.bounds[1][1]) - object.radius,
//         ),
//       ],
//     )
//   }
//
//   fn become_neighbors<A: EventAccessor<Steward = Self::Steward>>(
//     &self,
//     accessor: &A,
//     objects: [&DataHandle<Self::Object>; 2],
//   ) {
//     //println!("become {:?}", (objects [0].index, objects [1].index));
//     let relationship = accessor.new_handle(Relationship {
//       circles: (objects[0].clone(), objects[1].clone()),
//       varying: EntityCell::new(SimpleTimeline::new()),
//     });
//     set(
//       accessor,
//       &relationship.varying,
//       RelationshipVarying {
//         induced_acceleration: None,
//         next_change: None,
//       },
//     );
//     for object in objects.iter() {
//       modify(accessor, &object.varying, |varying| {
//         varying.relationships.push(relationship.clone())
//       });
//     }
//     update_relationship_change_prediction(accessor, &relationship);
//   }
//   fn stop_being_neighbors<A: EventAccessor<Steward = Self::Steward>>(
//     &self,
//     accessor: &A,
//     objects: [&DataHandle<Self::Object>; 2],
//   ) {
//     //println!("stop {:?}", (objects [0].index, objects [1].index));
//     let varying = tracking_query(accessor, &objects[0].varying);
//     let relationship = varying
//       .relationships
//       .iter()
//       .find(|relationship| {
//         ([&relationship.circles.0, &relationship.circles.1] == objects
//           || [&relationship.circles.1, &relationship.circles.0] == objects)
//       })
//       .unwrap()
//       .clone();
//     destroy(accessor, &relationship.varying);
//     for object in objects.iter() {
//       modify(accessor, &object.varying, |varying| {
//         varying.relationships.retain(|relationship| {
//           !([&relationship.circles.0, &relationship.circles.1] == objects
//             || [&relationship.circles.1, &relationship.circles.0] == objects)
//         })
//       });
//     }
//   }
// }
