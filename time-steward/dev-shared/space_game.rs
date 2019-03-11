//use time_steward::support;
use time_steward::support::time_functions::QuadraticTrajectory;
use nalgebra::Vector2;
//use time_steward::support::rounding_error_tolerant_math::right_shift_round_up;
use std::marker::PhantomData;

use time_steward::{DeterministicRandomId};
use time_steward::{DataHandleTrait, DataTimelineCellTrait, QueryResult, Basics as BasicsTrait};
use time_steward::type_utils::{PersistentTypeId, PersistentlyIdentifiedType};
use time_steward::type_utils::list_of_types::{ListedType};
pub use time_steward::stewards::{simple_full as steward_module};
use steward_module::{TimeSteward, Event, DataHandle, DataTimelineCell, Accessor, EventAccessor, FutureCleanupAccessor, bbox_collision_detection_2d as collisions};
use simple_timeline::{SimpleTimeline, query, tracking_query, tracking_query_ref, set, destroy};
use self::collisions::{BoundingBox, NumDimensions, Detector};
use self::collisions::simple_grid::{SimpleGridDetector};

use rand::Rng;
use boolinator::Boolinator;

pub type Time = i64;
pub type SpaceCoordinate = i64;


pub const HOW_MANY_CIRCLES: usize = 20;
pub const ARENA_SIZE_SHIFT: u32 = 20;
pub const ARENA_SIZE: SpaceCoordinate = 1 << ARENA_SIZE_SHIFT;
// pub const GRID_SIZE_SHIFT: u32 = ARENA_SIZE_SHIFT - 3;
// pub const GRID_SIZE: SpaceCoordinate = 1 << GRID_SIZE_SHIFT;
pub const MAX_DISTANCE_TRAVELED_AT_ONCE: SpaceCoordinate = ARENA_SIZE << 4;
pub const TIME_SHIFT: u32 = 20;
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
    varying.position.update_by(accessor.now() - varying.last_change);
    varying.last_change = accessor.now().clone();
    (f)(varying)
  });
  trajectory_changed (accessor, circle);
}
fn modify_trajectories<A: EventAccessor <Steward = Steward>, F: FnOnce(&mut RelationshipVarying, (&mut CircleVarying, &mut CircleVarying))>(accessor: &A, relationship: &RelationshipHandle, f: F) {
  modify (accessor, & relationship.varying, | relationship_varying | {
    modify (accessor, & relationship.circles.0.varying, | circle_0 | {
      modify (accessor, & relationship.circles.1.varying, | circle_1 | {
        circle_0.position.update_by(accessor.now() - circle_0.last_change);
        circle_0.last_change = accessor.now().clone();
        circle_1.position.update_by(accessor.now() - circle_1.last_change);
        circle_1.last_change = accessor.now().clone();
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
pub enum ObjectType {
  Shot,
  Turret {last_fired: Time, shots_fired: usize, next_shoot: Option <<Steward as TimeSteward>::EventHandle>,},
}

#[derive (Clone, PartialEq, Eq, Serialize, Deserialize, Debug)]
pub struct Globals {
  pub circles: Vec<CircleHandle>,
  pub detector: DataTimelineCell <SimpleTimeline <DataHandle <SimpleGridDetector<Space>>, Steward>>,
}

#[derive (Clone, PartialEq, Eq, Serialize, Deserialize, Debug)]
pub struct Circle {
  pub id: DeterministicRandomId,
  pub radius: SpaceCoordinate,
  pub varying: DataTimelineCell <SimpleTimeline <CircleVarying, Steward>>,
}
#[derive (Clone, PartialEq, Eq, Serialize, Deserialize, Debug)]
pub struct CircleVarying {
  pub object_type: ObjectType,
  pub position: QuadraticTrajectory,
  pub last_change: Time,
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


pub fn to_collision_space (coordinate: SpaceCoordinate)->collisions::Coordinate {
  (coordinate as collisions::Coordinate).wrapping_sub(1u64 << 63)
}
pub fn from_collision_space (coordinate: collisions::Coordinate)->SpaceCoordinate {
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
  type UniqueId = DeterministicRandomId;
  
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
    object.id
  }

  fn current_bounding_box<A: EventAccessor <Steward = Self::Steward>>(&self, accessor: &A, object: &DataHandle<Self::Object>)->BoundingBox <Self> {
    let varying = tracking_query (accessor, & object.varying);
    let center = varying.position.updated_by (accessor.now() - varying.last_change).unwrap().evaluate();
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
    varying.position.approximately_when_escapes (
      varying.last_change.clone(),
      accessor.now().clone(),
      [
        [from_collision_space (bounds.bounds [0] [0]) + object.radius, from_collision_space (bounds.bounds [0] [1]) - object.radius],
        [from_collision_space (bounds.bounds [1] [0]) + object.radius, from_collision_space (bounds.bounds [1] [1]) - object.radius],
      ]
    )
  }
  
  fn become_neighbors<A: EventAccessor <Steward = Self::Steward>>(&self, accessor: &A, objects: [&DataHandle<Self::Object>; 2]) {
    //println!("become {:?}", (objects [0].id, objects [1].id));
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
    //println!("stop {:?}", (objects [0].id, objects [1].id));
    let varying = tracking_query (accessor, & objects[0].varying);
    let relationship = varying.relationships.iter().find (| relationship | (
      [&relationship.circles.0, &relationship.circles.1] == objects
      || [&relationship.circles.1, &relationship.circles.0] == objects
    )).unwrap().clone();
    destroy (accessor, & relationship.varying);
    for object in objects.iter() {
      modify (accessor, &object.varying, | varying | {
        let old_size = varying.relationships.len();
        varying.relationships.retain (| relationship | !(
          [&relationship.circles.0, &relationship.circles.1] == objects
          || [&relationship.circles.1, &relationship.circles.0] == objects
        ));
        assert_eq!(varying.relationships.len(), old_size - 1);
      });
    }
  }
}





pub fn update_relationship_change_prediction <Accessor: EventAccessor <Steward = Steward>>(accessor: &Accessor, relationship_handle: &RelationshipHandle) {
  let circles = &relationship_handle.circles;
  let us = (
    tracking_query_ref (accessor, & circles.0.varying),
    tracking_query_ref (accessor, & circles.1.varying));

  modify (accessor, & relationship_handle.varying, | relationship_varying | {
    let time = QuadraticTrajectory::approximately_when_distance_passes(
      circles.0.radius + circles.1.radius,
      if relationship_varying.induced_acceleration.is_none() { -1 } else { 1 },
      ((us.0).last_change, &(us.0).position),
      ((us.1).last_change, &(us.1).position));
    
    // println!("Planning for {} At {}, {}", id, (us.0).1, (us.1).1);
    if time.is_none() && relationship_varying.induced_acceleration.is_some() {
      panic!(" fail {:?} {:?} {:?}", relationship_handle, relationship_varying, us)
    }
  
    relationship_varying.next_change = time.and_then (| time | (time >= *accessor.now()).as_some_from(||
      // println!(" planned for {}", &yes);
      accessor.create_prediction (
        time,
        DeterministicRandomId::new (&(accessor.extended_now().id, circles.0.id, circles.1.id, 0x6515c48170b61837u64)),
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
      //let new_difference = new.0.position.evaluate ()-new.1.position.evaluate ();
      //println!("event with error {:?}", (new_difference.dot(&new_difference) as f64).sqrt() - (circles.0.radius+circles.1.radius)  as f64);
      if let Some(induced_acceleration) = relationship_varying.induced_acceleration {
        new.0.position.add_acceleration(-induced_acceleration);
        new.1.position.add_acceleration(induced_acceleration);
        relationship_varying.induced_acceleration = None;
        //println!("Parted {} At {}", self.id, mutator.now());
      } else {
        let acceleration = (new.0.position.evaluate() -
                            new.1.position.evaluate()) *
                            (ARENA_SIZE * 4 /
                             (circles.0.radius + circles.1.radius));
        new.0.position.add_acceleration(acceleration);
        new.1.position.add_acceleration(-acceleration);
        relationship_varying.induced_acceleration = Some(acceleration);
        //println!("Joined {} At {}", self.id, mutator.now());
      }
    });
  }
}

pub fn update_boundary_change_prediction <Accessor: EventAccessor <Steward = Steward>>(accessor: &Accessor, circle_handle: &CircleHandle) {
  let arena_center = QuadraticTrajectory::new(TIME_SHIFT,
                                              MAX_DISTANCE_TRAVELED_AT_ONCE,
                                              [ARENA_SIZE / 2, ARENA_SIZE / 2, 0, 0, 0, 0]);
  
  modify (accessor, & circle_handle.varying, | varying | {
    let time = QuadraticTrajectory::approximately_when_distance_passes(
      ARENA_SIZE - circle_handle.radius,
      if varying.boundary_induced_acceleration.is_some() { -1 } else { 1 },
      (varying.last_change, & varying.position),
      (0, & arena_center));
    
    varying.next_boundary_change = time.and_then (| time | (time >= *accessor.now()).as_some_from(||
      // println!(" planned for {}", &yes);
      accessor.create_prediction (
        time,
        DeterministicRandomId::new (&(accessor.extended_now().id, circle_handle.id)),
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
        new.position.add_acceleration(-induced_acceleration);
        new.boundary_induced_acceleration = None;
      } else {
        let acceleration = -(new.position.evaluate() -
                              Vector2::new(ARENA_SIZE / 2,
                                           ARENA_SIZE / 2)) *
                            (ARENA_SIZE * 400 / (ARENA_SIZE - self.circle_handle.radius));
        new.position.add_acceleration(acceleration);
        new.boundary_induced_acceleration = Some(acceleration);
      }
    });
  }
}


pub fn update_shoot_prediction <Accessor: EventAccessor <Steward = Steward>>(accessor: &Accessor, circle_handle: &CircleHandle) {
  modify (accessor, & circle_handle.varying, | new | {
    if let ObjectType::Turret {last_fired, shots_fired, ..} = new.object_type.clone() {
      
      new.object_type = ObjectType::Turret {last_fired: last_fired, shots_fired: shots_fired, next_shoot: Some(accessor.create_prediction (
        last_fired + SECOND/10,
        DeterministicRandomId::new (&(accessor.extended_now().id, circle_handle.id, 0xd3e65114a9b8cdbau64)),
        Shoot {circle_handle: circle_handle.clone()}
      ))};
    }
  });
}


define_event!{
  pub struct Shoot {pub circle_handle: CircleHandle},
  PersistentTypeId(0xc50c51edbe4943f1),
  fn execute (&self, accessor: &mut Accessor) {
    let mut shotpass = None;
    modify_trajectory (accessor, & self.circle_handle, | new | {
      if let ObjectType::Turret {shots_fired, next_shoot, ..} = new.object_type.clone() {
        
        new.object_type = ObjectType::Turret {last_fired: accessor.now().clone(), shots_fired: shots_fired + 1, next_shoot};
        
        let shot = accessor.new_handle(Circle {id: DeterministicRandomId::new (& (self.circle_handle.id, shots_fired)), radius: ARENA_SIZE/60, varying:DataTimelineCell::new(SimpleTimeline::new ())}) ;
        let position_now = new.position.evaluate();
        let position = QuadraticTrajectory::new(TIME_SHIFT,
                              MAX_DISTANCE_TRAVELED_AT_ONCE,
                              [position_now [0] + self.circle_handle.radius,
                               position_now [1],
                               0,
                               0,
                               0,
                               0]);
        set (accessor, & shot.varying, CircleVarying {
          object_type: ObjectType::Shot,
          position: position,
          last_change: *accessor.now(),
          relationships: Vec::new(),
          boundary_induced_acceleration: None,
          next_boundary_change: None,
          collision_data: None,
        });
        shotpass = Some(shot);
      }
    });
    SimpleGridDetector::insert (accessor, &query(accessor, &accessor.globals().detector), & shotpass.unwrap(), None);
    update_shoot_prediction (accessor, & self.circle_handle);
  }
}

define_event!{
  pub struct Initialize {},
  PersistentTypeId(0xbf7ba1ff2ab76640),
  fn execute (&self, accessor: &mut Accessor) {
    set (accessor, &accessor.globals().detector, SimpleGridDetector::new (accessor, Space, (ARENA_SIZE >> 4) as collisions::Coordinate));
    let circles = &accessor.globals().circles;
    let mut varying = Vec::new();
    let mut generator = DeterministicRandomId::new (&2u8).to_rng();
    let thingy = ARENA_SIZE / 20;
    for index in 0..HOW_MANY_CIRCLES {
      let position = QuadraticTrajectory::new(TIME_SHIFT,
                              MAX_DISTANCE_TRAVELED_AT_ONCE,
                              [generator.gen_range(0, ARENA_SIZE),
                               generator.gen_range(0, ARENA_SIZE),
                               generator.gen_range(-thingy, thingy),
                               generator.gen_range(-thingy, thingy),
                               0,
                               0]);
      varying.push (CircleVarying {
        object_type: if index == 0 {
          ObjectType::Turret {last_fired: 0, shots_fired: 0, next_shoot: None}
        } else {ObjectType::Shot},
        position: position,
        last_change: 0,
        relationships: Vec::new(),
        boundary_induced_acceleration: None,
        next_boundary_change: None,
        collision_data: None,
      });
      set (accessor, & circles [index].varying, varying [index].clone());
    }
    for index in 0..HOW_MANY_CIRCLES {
      SimpleGridDetector::insert (accessor, &query(accessor, &accessor.globals().detector), & circles [index], None);
      update_shoot_prediction (accessor, & circles [index]);
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
        let position = varying.position.updated_by(accessor.now() - varying.last_change).unwrap().evaluate();
        let distance_squared = (self.coordinates [0] - position [0]) * (self.coordinates [0] - position [0]) + (self.coordinates [1] - position [1]) * (self.coordinates [1] - position [1]);
        if distance_squared <best_distance_squared {
          best_distance_squared = distance_squared;
          best_handle = Some (circle.clone());
        }
      }
    }
    
    let best_handle = best_handle.unwrap() ;
    modify_trajectory (accessor, & best_handle, | new | {
      let impulse = -(new.position.evaluate() -
                            Vector2::new(ARENA_SIZE / 2,
                                         ARENA_SIZE / 2)) *
                          (ARENA_SIZE * 4 / (ARENA_SIZE ));
      new.position.add_velocity(impulse);
    });
  }
}

pub fn make_globals()-> <Basics as BasicsTrait>::Globals {
  let mut circles = Vec::new();
  let mut generator = DeterministicRandomId::new (&0u8).to_rng();
  
  for index in 0..HOW_MANY_CIRCLES {
    let radius = generator.gen_range(ARENA_SIZE / 30, ARENA_SIZE / 15);

    circles.push (DataHandle::new_for_globals (Circle {
      id: DeterministicRandomId::new (& index),
      radius: radius,
      varying: DataTimelineCell::new(SimpleTimeline::new ())
    }));
  }
  Globals {
    circles: circles,
    detector: DataTimelineCell::new(SimpleTimeline::new ()),
  }
}

