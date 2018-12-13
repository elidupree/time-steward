//use time_steward::support;
use time_steward::support::trajectories;
use nalgebra::Vector2;
//use time_steward::support::rounding_error_tolerant_math::right_shift_round_up;
use time_steward::support::integer_math::polynomial::RootSearchResult;
use std::marker::PhantomData;

use time_steward::{DeterministicRandomId};
use time_steward::{PersistentTypeId, ListedType, PersistentlyIdentifiedType, DataHandleTrait, DataTimelineCellTrait, QueryResult, Basics as BasicsTrait};
pub use time_steward::stewards::{simple_full as steward_module};
use steward_module::{TimeSteward, Event, DataHandle, DataTimelineCell, Accessor, EventAccessor, FutureCleanupAccessor, bbox_collision_detection_2d as collisions};
use simple_timeline::{SimpleTimeline, query, tracking_query, tracking_query_ref, set, destroy};
use self::collisions::{BoundingBox, NumDimensions, Detector};
use self::collisions::simple_grid::{SimpleGridDetector};

use rand::Rng;
use boolinator::Boolinator;

pub type Time = i64;
pub type SpaceCoordinate = i64;
pub type QuadraticTrajectory = trajectories::QuadraticTrajectory <Vector2 <SpaceCoordinate>>;


pub const HOW_MANY_CIRCLES: usize = 20;
pub const ARENA_SIZE_SHIFT: u32 = 20;
pub const ARENA_SIZE: SpaceCoordinate = 1 << ARENA_SIZE_SHIFT;
// pub const GRID_SIZE_SHIFT: u32 = ARENA_SIZE_SHIFT - 3;
// pub const GRID_SIZE: SpaceCoordinate = 1 << GRID_SIZE_SHIFT;
pub const TIME_SHIFT: u32 = 20;
pub const STATIC_TIME_SHIFT: u32 = TIME_SHIFT;
pub const SECOND: Time = 1 << TIME_SHIFT;


macro_rules! define_event {
  (
    $visibility: vis struct $Struct: ident {$($contents:tt)*},
    PersistentTypeId($id: expr),
    fn execute $($execute:tt)*
  ) => {
#[derive (Clone, PartialEq, Eq, Serialize, Deserialize, Debug)]
$visibility struct $Struct {$($contents)*}
impl PersistentlyIdentifiedType for $Struct {
  const ID: PersistentTypeId = PersistentTypeId($id);
}
impl Event for $Struct {
  type Steward = Steward;
  type ExecutionData = ();
  fn execute <Accessor: EventAccessor <Steward = Self::Steward>> $($execute)*
  fn undo <Accessor: FutureCleanupAccessor <Steward = Self::Steward>> (&self, _accessor: &mut Accessor, _: ()) {
    unimplemented!()
  }
}
  }
}

fn modify<A: EventAccessor <Steward = Steward>, T: QueryResult, F: FnOnce(&mut T)>(accessor: &A, cell: &DataTimelineCell <SimpleTimeline <T, Steward>>, f: F) {
  let mut data = query (accessor, cell);
  (f)(&mut data);
  set (accessor, cell, data);
}
fn modify_trajectory<A: EventAccessor <Steward = Steward>, F: FnOnce(&mut CircleVarying)>(accessor: &A, circle: &CircleHandle, f: F) {
  modify (accessor, &circle.varying, |varying| {
    (f)(varying)
  });
  trajectory_changed (accessor, circle);
}
fn modify_trajectories<A: EventAccessor <Steward = Steward>, F: FnOnce(&mut RelationshipVarying, (&mut CircleVarying, &mut CircleVarying))>(accessor: &A, relationship: &RelationshipHandle, f: F) {
  modify (accessor, & relationship.varying, | relationship_varying | {
    modify (accessor, & relationship.circles.0.varying, | circle_0 | {
      modify (accessor, & relationship.circles.1.varying, | circle_1 | {
        (f)(relationship_varying, (circle_0, circle_1));
      });
    });
  });
  trajectory_changed (accessor, & relationship.circles.0);
  trajectory_changed (accessor, & relationship.circles.1);
}
fn trajectory_changed <A: EventAccessor <Steward = Steward>>(accessor: &A, circle: &CircleHandle) {
  for relationship in query (accessor, & circle.varying).relationships.iter() {
    update_relationship_change_prediction (accessor, relationship);
  }
  update_boundary_change_prediction (accessor, circle);
  SimpleGridDetector::changed_course(accessor, &query(accessor, &accessor.globals().detector), circle);
}



#[derive (Copy, Clone, Hash, PartialEq, Eq, PartialOrd, Ord, Serialize, Deserialize, Debug, Default)]
pub struct Basics {}
impl BasicsTrait for Basics {
  type Time = Time;
  type Globals = Globals;
  type Types = (ListedType <RelationshipChange>, ListedType <BoundaryChange>, ListedType <Initialize>, ListedType <Disturb>, collisions::simple_grid::Types <Space>);
}

pub type Steward = steward_module::Steward <Basics>;


#[derive (Clone, PartialEq, Eq, Serialize, Deserialize, Debug)]
pub struct Globals {
  pub circles: Vec<CircleHandle>,
  pub detector: DataTimelineCell <SimpleTimeline <DataHandle <SimpleGridDetector<Space>>, Steward>>,
}

#[derive (Clone, PartialEq, Eq, Serialize, Deserialize, Debug)]
pub struct Circle {
  pub index: usize,
  pub radius: SpaceCoordinate,
  pub varying: DataTimelineCell <SimpleTimeline <CircleVarying, Steward>>,
}
#[derive (Clone, PartialEq, Eq, Serialize, Deserialize, Debug)]
pub struct CircleVarying {
  pub position: QuadraticTrajectory,
  pub relationships: Vec<RelationshipHandle>,
  pub boundary_induced_acceleration: Option <Vector2<SpaceCoordinate>>,
  pub next_boundary_change: Option <<Steward as TimeSteward>::EventHandle>,
  pub collision_data: Option<collisions::simple_grid::DetectorDataPerObject<Space>>,
}
impl PersistentlyIdentifiedType for Circle {
  const ID: PersistentTypeId = PersistentTypeId(0xd711cc7240c71607);
}
type CircleHandle = DataHandle <Circle>;

#[derive (Clone, PartialEq, Eq, Serialize, Deserialize, Debug)]
pub struct Relationship {
  pub circles: (CircleHandle, CircleHandle),
  pub varying: DataTimelineCell <SimpleTimeline <RelationshipVarying, Steward>>,
}
#[derive (Clone, PartialEq, Eq, Serialize, Deserialize, Debug)]
pub struct RelationshipVarying {
  pub induced_acceleration: Option <Vector2<SpaceCoordinate>>,
  pub next_change: Option <<Steward as TimeSteward>::EventHandle>,
}
impl PersistentlyIdentifiedType for Relationship {
  const ID: PersistentTypeId = PersistentTypeId(0xa1010b5e80c3465a);
}
type RelationshipHandle = DataHandle <Relationship>;


fn to_collision_space (coordinate: SpaceCoordinate)->collisions::Coordinate {
  (coordinate as collisions::Coordinate).wrapping_sub(1u64 << 63)
}
fn from_collision_space (coordinate: collisions::Coordinate)->SpaceCoordinate {
  (coordinate.wrapping_add(1u64 << 63)) as SpaceCoordinate
}

#[derive (Clone, PartialEq, Eq, Serialize, Deserialize, Debug)]
pub struct Space;
impl PersistentlyIdentifiedType for Space {
  const ID: PersistentTypeId = PersistentTypeId(0x879511343e48addd);
}
impl collisions::Space for Space {
  type Steward = Steward;
  type Object = Circle;
  type DetectorDataPerObject = collisions::simple_grid::DetectorDataPerObject<Self>;
  type UniqueId = usize;
  
  const DIMENSIONS: NumDimensions = 2;

  // An Object generally has to store some opaque data for the collision detector.
  // It would normally include a DataHandle to a tree node.
  // These are getter and setter methods for that data.
  fn get_detector_data<A: Accessor <Steward = Self::Steward>>(&self, accessor: &A, object: &DataHandle<Self::Object>)->Option<Self::DetectorDataPerObject> {
    query (accessor, &object.varying).collision_data
  }
  fn set_detector_data<A: EventAccessor <Steward = Self::Steward>>(&self, accessor: &A, object: &DataHandle<Self::Object>, data: Option<Self::DetectorDataPerObject>) {
    modify (accessor, &object.varying, | varying | varying.collision_data = data);
  }
  fn unique_id<A: EventAccessor <Steward = Self::Steward>>(&self, _accessor: &A, object: &DataHandle<Self::Object>)->Self::UniqueId {
    object.index
  }

  fn current_bounding_box<A: EventAccessor <Steward = Self::Steward>>(&self, accessor: &A, object: &DataHandle<Self::Object>)->BoundingBox <Self> {
    let varying = tracking_query (accessor, & object.varying);
    let center = varying.position.value(*accessor.now(), STATIC_TIME_SHIFT).unwrap();
    BoundingBox {
      bounds: [
        [to_collision_space (center [0] - object.radius), to_collision_space (center [0] + object.radius)],
        [to_collision_space (center [1] - object.radius), to_collision_space (center [1] + object.radius)],
      ],
      _marker: PhantomData
    }
  }
  fn when_escapes<A: EventAccessor <Steward = Self::Steward>>(&self, accessor: &A, object: &DataHandle<Self::Object>, bounds: BoundingBox <Self>)->Option<<<Self::Steward as TimeSteward>::Basics as BasicsTrait>::Time> {
    let varying = tracking_query (accessor, & object.varying);
    match varying.position.next_time_possibly_outside_bounds ([*accessor.now(), Time::max_value()], STATIC_TIME_SHIFT, [
      Vector2::new (from_collision_space (bounds.bounds [0] [0]) + object.radius, from_collision_space (bounds.bounds [1] [0]) + object.radius),
      Vector2::new (from_collision_space (bounds.bounds [0] [1]) - object.radius, from_collision_space (bounds.bounds [1] [1]) - object.radius),
    ]) {
      RootSearchResult::Root (value)=>Some (value),
      _=> None,
    }
  }
  
  fn become_neighbors<A: EventAccessor <Steward = Self::Steward>>(&self, accessor: &A, objects: [&DataHandle<Self::Object>; 2]) {
    //println!("become {:?}", (objects [0].index, objects [1].index));
    let relationship = accessor.new_handle (Relationship {
      circles: (objects [0].clone(), objects [1].clone()),
      varying: DataTimelineCell::new(SimpleTimeline::new ()),
    });
    set (accessor, & relationship.varying, RelationshipVarying {
      induced_acceleration: None,
      next_change: None,
    });
    for object in objects.iter() {
      modify (accessor, &object.varying, | varying | varying.relationships.push (relationship.clone()));
    }
    update_relationship_change_prediction (accessor, &relationship) ;
  }
  fn stop_being_neighbors<A: EventAccessor <Steward = Self::Steward>>(&self, accessor: &A, objects: [&DataHandle<Self::Object>; 2]) {
    //println!("stop {:?}", (objects [0].index, objects [1].index));
    let varying = tracking_query (accessor, & objects[0].varying);
    let relationship = varying.relationships.iter().find (| relationship | (
      [&relationship.circles.0, &relationship.circles.1] == objects
      || [&relationship.circles.1, &relationship.circles.0] == objects
    )).unwrap().clone();
    destroy (accessor, & relationship.varying);
    for object in objects.iter() {
      modify (accessor, &object.varying, | varying | varying.relationships.retain (| relationship | !(
        [&relationship.circles.0, &relationship.circles.1] == objects
        || [&relationship.circles.1, &relationship.circles.0] == objects
      )));
    }
  }
}





pub fn update_relationship_change_prediction <Accessor: EventAccessor <Steward = Steward>>(accessor: &Accessor, relationship_handle: &RelationshipHandle) {
  let circles = &relationship_handle.circles;
  let us = (
    tracking_query_ref (accessor, & circles.0.varying),
    tracking_query_ref (accessor, & circles.1.varying));

  modify (accessor, & relationship_handle.varying, | relationship_varying | {
    let difference = &(us.1).position - &(us.0).position;
    let radius = circles.0.radius + circles.1.radius;
    let result = if relationship_varying.induced_acceleration.is_none() {
      difference.next_time_magnitude_significantly_lt([*accessor.now(), Time::max_value()], STATIC_TIME_SHIFT, radius)
    } else {
      difference.next_time_magnitude_significantly_gt([*accessor.now(), Time::max_value()], STATIC_TIME_SHIFT, radius)
    };
    let time = match result {
      Ok (RootSearchResult::Root (time)) => Some(time),
      _=> None,
    };
    
    // println!("Planning for {} At {}, {}", id, (us.0).1, (us.1).1);
    if time.is_none() && relationship_varying.induced_acceleration.is_some() {
      panic!(" fail {:?} {:?} {:?}", relationship_handle, relationship_varying, us)
    }
  
    relationship_varying.next_change = time.and_then (| time | (time >= *accessor.now()).as_some_from(||
      // println!(" planned for {}", &yes);
      accessor.create_prediction (
        time,
        DeterministicRandomId::new (&(accessor.extended_now().id, circles.0.index, circles.1.index.wrapping_add (0x6515c48170b61837))),
        RelationshipChange {relationship_handle: relationship_handle.clone()}
      )
    ));
  });
}

define_event!{
  pub struct RelationshipChange {pub relationship_handle: RelationshipHandle},
  PersistentTypeId(0x08c4b60ad5d0ed08),
  fn execute (&self, accessor: &mut Accessor) {
    let circles = &self.relationship_handle.circles;
    modify_trajectories(accessor, &self.relationship_handle, | relationship_varying, new | {
      //let new_difference = new.0.position.value(*accessor.now(), STATIC_TIME_SHIFT).unwrap()-new.1.position.value(*accessor.now(), STATIC_TIME_SHIFT).unwrap();
      //println!("event with error {:?}", (new_difference.dot(&new_difference) as f64).sqrt() - (circles.0.radius+circles.1.radius)  as f64);
      if let Some(induced_acceleration) = relationship_varying.induced_acceleration {
        new.0.position.add_acceleration(*accessor.now(), STATIC_TIME_SHIFT,-induced_acceleration).unwrap();
        new.1.position.add_acceleration(*accessor.now(), STATIC_TIME_SHIFT, induced_acceleration).unwrap();
        relationship_varying.induced_acceleration = None;
        //println!("Parted {} At {}", self.id, mutator.now());
      } else {
        let acceleration = (new.0.position.value(*accessor.now(), STATIC_TIME_SHIFT).unwrap() -
                            new.1.position.value(*accessor.now(), STATIC_TIME_SHIFT).unwrap()) *
                            (ARENA_SIZE * 16 /
                             (circles.0.radius + circles.1.radius));
        new.0.position.add_acceleration(*accessor.now(), STATIC_TIME_SHIFT, acceleration).unwrap();
        new.1.position.add_acceleration(*accessor.now(), STATIC_TIME_SHIFT,-acceleration).unwrap();
        relationship_varying.induced_acceleration = Some(acceleration);
        //println!("Joined {} At {}", self.id, mutator.now());
      }
    });
  }
}

pub fn update_boundary_change_prediction <Accessor: EventAccessor <Steward = Steward>>(accessor: &Accessor, circle_handle: &CircleHandle) {
  let arena_center = QuadraticTrajectory::constant(Vector2::new (ARENA_SIZE / 2, ARENA_SIZE / 2));
  
  modify (accessor, & circle_handle.varying, | varying | {
    let difference = &varying.position - arena_center;
    let radius = ARENA_SIZE - circle_handle.radius;
    let result = if varying.boundary_induced_acceleration.is_some() {
      difference.next_time_magnitude_significantly_lt([*accessor.now(), Time::max_value()], STATIC_TIME_SHIFT, radius)
    } else {
      difference.next_time_magnitude_significantly_gt([*accessor.now(), Time::max_value()], STATIC_TIME_SHIFT, radius)
    };
    let time = match result {
      Ok (RootSearchResult::Root (time)) => Some(time),
      _=> None,
    };
    
    varying.next_boundary_change = time.and_then (| time | (time >= *accessor.now()).as_some_from(||
      // println!(" planned for {}", &yes);
      accessor.create_prediction (
        time,
        DeterministicRandomId::new (&(accessor.extended_now().id, circle_handle.index)),
        BoundaryChange {circle_handle: circle_handle.clone()}
      )
    ));
  });
}


define_event!{
  pub struct BoundaryChange {pub circle_handle: CircleHandle},
  PersistentTypeId(0x6fc5127ff6aeb50d),
  fn execute (&self, accessor: &mut Accessor) {
    modify_trajectory (accessor, & self.circle_handle, | new | {
      if let Some(induced_acceleration) = new.boundary_induced_acceleration {
        new.position.add_acceleration(*accessor.now(), STATIC_TIME_SHIFT,-induced_acceleration).unwrap();
        new.boundary_induced_acceleration = None;
      } else {
        let acceleration = -(new.position.value(*accessor.now(), STATIC_TIME_SHIFT).unwrap() -
                              Vector2::new(ARENA_SIZE / 2,
                                           ARENA_SIZE / 2)) *
                            (ARENA_SIZE * 1600 / (ARENA_SIZE - self.circle_handle.radius));
        new.position.add_acceleration(*accessor.now(), STATIC_TIME_SHIFT, acceleration).unwrap();
        new.boundary_induced_acceleration = Some(acceleration);
      }
    });
  }
}

define_event!{
  pub struct Initialize {},
  PersistentTypeId(0xbf7ba1ff2ab76640),
  fn execute (&self, accessor: &mut Accessor) {
    set (accessor, &accessor.globals().detector, SimpleGridDetector::new (accessor, Space, (ARENA_SIZE >> 2) as collisions::Coordinate));
    let circles = &accessor.globals().circles;
    let mut varying = Vec::new();
    let mut generator = DeterministicRandomId::new (&2u8).to_rng();
    let thingy = ARENA_SIZE / 20;
    for index in 0..HOW_MANY_CIRCLES {
      let mut position = QuadraticTrajectory::constant (Vector2::new (
        generator.gen_range(0, ARENA_SIZE),
        generator.gen_range(0, ARENA_SIZE),
      ));
      position.set_velocity (*accessor.now(), STATIC_TIME_SHIFT, Vector2::new (
        generator.gen_range(-thingy, thingy),
        generator.gen_range(-thingy, thingy),
      )).unwrap();
      varying.push (CircleVarying {
        position: position,
        relationships: Vec::new(),
        boundary_induced_acceleration: None,
        next_boundary_change: None,
        collision_data: None,
      });
      set (accessor, & circles [index].varying, varying [index].clone());
    }
    for index in 0..HOW_MANY_CIRCLES {
      SimpleGridDetector::insert (accessor, &query(accessor, &accessor.globals().detector), & circles [index], None);
    }
  }
}

define_event!{
  pub struct Disturb {pub coordinates: [SpaceCoordinate; 2]},
  PersistentTypeId(0xb8bbf65eaaf08d0e),
  fn execute (&self, accessor: &mut Accessor) {
    let circles = &accessor.globals().circles;
    let mut best_handle = None;
    {
      let mut best_distance_squared = i64::max_value();
      for circle in circles.iter() {
        let varying = tracking_query_ref (accessor, &circle.varying);
        let position = varying.position.value (*accessor.now(), STATIC_TIME_SHIFT).unwrap();
        let distance_squared = (self.coordinates [0] - position [0]) * (self.coordinates [0] - position [0]) + (self.coordinates [1] - position [1]) * (self.coordinates [1] - position [1]);
        if distance_squared <best_distance_squared {
          best_distance_squared = distance_squared;
          best_handle = Some (circle.clone());
        }
      }
    }
    
    let best_handle = best_handle.unwrap() ;
    modify_trajectory (accessor, & best_handle, | new | {
      let impulse = -(new.position.value(*accessor.now(), STATIC_TIME_SHIFT).unwrap() -
                            Vector2::new(ARENA_SIZE / 2,
                                         ARENA_SIZE / 2)) *
                          (ARENA_SIZE * 4 / (ARENA_SIZE ));
      new.position.add_velocity(*accessor.now(), TIME_SHIFT, impulse).unwrap();
    });
  }
}

pub fn make_globals()-> <Basics as BasicsTrait>::Globals {
  let mut circles = Vec::new();
  let mut generator = DeterministicRandomId::new (&0u8).to_rng();
  
  for index in 0..HOW_MANY_CIRCLES {
    let radius = generator.gen_range(ARENA_SIZE / 30, ARENA_SIZE / 15);

    circles.push (DataHandle::new_for_globals (Circle {
      index: index,
      radius: radius,
      varying: DataTimelineCell::new(SimpleTimeline::new ())
    }));
  }
  Globals {
    circles: circles,
    detector: DataTimelineCell::new(SimpleTimeline::new ()),
  }
}

