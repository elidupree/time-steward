use time_steward::support;
use time_steward::support::time_functions::QuadraticTrajectory;
use nalgebra::Vector2;
use time_steward::support::rounding_error_tolerant_math::right_shift_round_up;


use time_steward::{DeterministicRandomId};
use time_steward::rowless::api::{PersistentTypeId, ListedType, PersistentlyIdentifiedType, DataHandleTrait, DataTimelineCellTrait, ExtendedTime, Basics as BasicsTrait};
use time_steward::rowless::stewards::{simple_full as steward_module};
use steward_module::{TimeSteward, ConstructibleTimeSteward, Event, DataHandle, DataTimelineCell, Accessor, EventAccessor, FutureCleanupAccessor, SnapshotAccessor, simple_timeline};
use simple_timeline::{SimpleTimeline, GetVarying, IterateUniquelyOwnedPredictions, tracking_query, set, unset};

use rand::Rng;

pub type Time = i64;
pub type SpaceCoordinate = i64;


pub const HOW_MANY_CIRCLES: usize = 20;
pub const ARENA_SIZE_SHIFT: u32 = 20;
pub const ARENA_SIZE: SpaceCoordinate = 1 << 20;
pub const GRID_SIZE_SHIFT: u32 = ARENA_SIZE_SHIFT - 3;
// pub const GRID_SIZE: SpaceCoordinate = 1 << GRID_SIZE_SHIFT;
pub const MAX_DISTANCE_TRAVELED_AT_ONCE: SpaceCoordinate = ARENA_SIZE << 4;
pub const TIME_SHIFT: u32 = 20;
pub const SECOND: Time = 1 << TIME_SHIFT;

#[derive (Copy, Clone, Hash, PartialEq, Eq, PartialOrd, Ord, Serialize, Deserialize, Debug, Default)]
pub struct Basics {}
impl BasicsTrait for Basics {
  type Time = Time;
  type Globals = Vec<CircleHandle>;
  type Types = (ListedType <RelationshipChange>, ListedType <BoundaryChange>, ListedType <Initialize>, ListedType <Disturb>);
}

pub type Steward = steward_module::Steward <Basics>;


#[derive (Clone, PartialEq, Eq, Serialize, Deserialize, Debug)]
pub struct Circle {
  pub index: usize,
  pub radius: SpaceCoordinate,
  pub varying: DataTimelineCell <SimpleTimeline <CircleVarying, Steward>>,
}
#[derive (Clone, PartialEq, Eq, Serialize, Deserialize, Debug)]
pub struct CircleVarying {
  pub position: QuadraticTrajectory,
  pub last_change: Time,
  pub relationships: Vec<RelationshipHandle>,
  pub boundary_induced_acceleration: Option <Vector2<SpaceCoordinate>>,
  pub next_boundary_change: Option <<Steward as TimeSteward>::EventHandle>,
}
impl PersistentlyIdentifiedType for Circle {
  const ID: PersistentTypeId = PersistentTypeId(0xd711cc7240c71607);
}
impl IterateUniquelyOwnedPredictions <Steward> for CircleVarying {
  fn iterate_predictions <F: FnMut (& <Steward as TimeSteward>::EventHandle)> (&self, callback: &mut F) {
    if let Some (prediction) = self.next_boundary_change.as_ref() {
      callback (prediction);
    }
  }
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
impl IterateUniquelyOwnedPredictions <Steward> for RelationshipVarying {
  fn iterate_predictions <F: FnMut (& <Steward as TimeSteward>::EventHandle)> (&self, callback: &mut F) {
    if let Some (prediction) = self.next_change.as_ref() {
      callback (prediction);
    }
  }
}
type RelationshipHandle = DataHandle <Relationship>;

pub fn query_relationship_circles <Accessor: EventAccessor <Steward = Steward>>(accessor: &Accessor, relationship: &Relationship)->(CircleVarying, CircleVarying) {
  (tracking_query (accessor, & relationship.circles.0.varying),
   tracking_query (accessor, & relationship.circles.1.varying))
}

pub fn update_relationship_change_prediction <Accessor: EventAccessor <Steward = Steward>>(accessor: &Accessor, relationship_handle: &RelationshipHandle) {
  let circles = &relationship_handle.circles;
  let now = accessor.extended_now().clone();
  let mut relationship_varying = tracking_query (accessor, & relationship_handle.varying);

  let us = query_relationship_circles (accessor, & relationship_handle);

  let time = QuadraticTrajectory::approximately_when_distance_passes(circles.0.radius +
                                                                   circles.1.radius,
                                                                   if relationship_varying.induced_acceleration.is_none() {
                                                                     -1
                                                                   } else {
                                                                     1
                                                                   },
                                                                   ((us.0).last_change,
                                                                    &(us.0).position),
                                                                   ((us.1).last_change,
                                                                    &(us.1).position));
  // println!("Planning for {} At {}, {}", id, (us.0).1, (us.1).1);
  if time.is_none() && relationship_varying.induced_acceleration.is_some() {
    panic!(" fail {:?} {:?} {:?}", relationship_handle, relationship_varying, us)
  }
  
  if let Some (discarded) = relationship_varying.next_change.take() {accessor.destroy_prediction (&discarded);}
  if let Some(yes) = time {
    if yes >= *accessor.now() {
      // println!(" planned for {}", &yes);
      relationship_varying.next_change = Some(accessor.create_prediction (
        yes,
        DeterministicRandomId::new (&(now.id, circles.0.index, circles.1.index.wrapping_add (0x6515c48170b61837))),
        RelationshipChange {relationship_handle: relationship_handle.clone()}
      ));
    }
  }
  set (accessor, & relationship_handle.varying, relationship_varying);
}

#[derive (Clone, PartialEq, Eq, Serialize, Deserialize, Debug)]
pub struct RelationshipChange {pub relationship_handle: RelationshipHandle} //, Basics, EventId (0x2312e29e341a2495),
impl PersistentlyIdentifiedType for RelationshipChange {
  const ID: PersistentTypeId = PersistentTypeId(0x08c4b60ad5d0ed08);
}
impl Event for RelationshipChange {
  type Steward = Steward;
  type ExecutionData = ();
  fn execute <Accessor: EventAccessor <Steward = Self::Steward>> (&self, accessor: &mut Accessor) {
    let circles = &self.relationship_handle.circles;
    let relationship_varying = tracking_query (accessor, &self.relationship_handle.varying);
    let us = query_relationship_circles (accessor, & self.relationship_handle);
    let mut new_relationship = relationship_varying.clone();
    let mut new = us.clone();
    new.0.position.update_by(accessor.now() - us.0.last_change);
    new.1.position.update_by(accessor.now() - us.1.last_change);
    new.0.last_change = accessor.now().clone();
    new.1.last_change = accessor.now().clone();
    if let Some(induced_acceleration) = relationship_varying.induced_acceleration {
      new.0
        .position
        .add_acceleration(-induced_acceleration);
      new.1
        .position
        .add_acceleration(induced_acceleration);
      new_relationship.induced_acceleration = None;
      //println!("Parted {} At {}", self.id, mutator.now());
    } else {
      let acceleration = (new.0.position.evaluate() -
                          new.1.position.evaluate()) *
                          (ARENA_SIZE * 4 /
                           (circles.0.radius + circles.1.radius));
      new.0.position.add_acceleration(acceleration);
      new.1.position.add_acceleration(-acceleration);
      new_relationship.induced_acceleration = Some(acceleration);
        //println!("Joined {} At {}", self.id, mutator.now());
    }
    set (accessor, & self.relationship_handle.varying, new_relationship);
    set (accessor, & circles.0.varying, new.0.clone());
    set (accessor, & circles.1.varying, new.1.clone());
    // TODO no repeating the relationship between these 2 in particular
    update_predictions (accessor, &circles.0, & new.0);
    update_predictions (accessor, &circles.1, & new.1);
  }

  fn undo <Accessor: FutureCleanupAccessor <Steward = Self::Steward>> (&self, accessor: &mut Accessor, _: ()) {
    unimplemented!()
  }
}

pub fn update_boundary_change_prediction <Accessor: EventAccessor <Steward = Steward>>(accessor: &Accessor, circle_handle: &CircleHandle) {
  let arena_center = QuadraticTrajectory::new(TIME_SHIFT,
                                              MAX_DISTANCE_TRAVELED_AT_ONCE,
                                              [ARENA_SIZE / 2, ARENA_SIZE / 2, 0, 0, 0, 0]);
  let now = accessor.extended_now().clone();
  let mut varying = tracking_query (accessor, & circle_handle.varying);

  let time = QuadraticTrajectory::approximately_when_distance_passes(ARENA_SIZE - circle_handle.radius,
                                                                   if varying.boundary_induced_acceleration.is_some() {
                                                                     -1
                                                                   } else {
                                                                     1
                                                                   },
                                                                   (varying.last_change,
                                                                    & varying.position),
                                                                    (0, & arena_center));
 
  if let Some (discarded) = varying.next_boundary_change.take() {accessor.destroy_prediction (&discarded);}
  if let Some(yes) = time {
    if yes >= *accessor.now() {
      // println!(" planned for {}", &yes);
      varying.next_boundary_change = Some(accessor.create_prediction (
        yes,
        DeterministicRandomId::new (&(now.id, circle_handle.index)),
        BoundaryChange {circle_handle: circle_handle.clone()}
      ));
    }
  }
  set (accessor, & circle_handle.varying, varying);
}

pub fn update_predictions <Accessor: EventAccessor <Steward = Steward>>(accessor: &Accessor, circle_handle: &CircleHandle, varying: &CircleVarying) {
  for handle in varying.relationships.iter() {
    update_relationship_change_prediction (accessor, handle);
  }
  update_boundary_change_prediction (accessor, circle_handle);
}

#[derive (Clone, PartialEq, Eq, Serialize, Deserialize, Debug)]
pub struct BoundaryChange {pub circle_handle: CircleHandle} //, Basics, EventId (0x59732d675b2329ad),
impl PersistentlyIdentifiedType for BoundaryChange {
  const ID: PersistentTypeId = PersistentTypeId(0x6fc5127ff6aeb50d);
}
impl Event for BoundaryChange {
  type Steward = Steward;
  type ExecutionData = ();
  fn execute <Accessor: EventAccessor <Steward = Self::Steward>> (&self, accessor: &mut Accessor) {
    let current_varying = tracking_query (accessor, &self.circle_handle.varying);
    let mut new = current_varying.clone();
    new.position.update_by(accessor.now() - current_varying.last_change);
    new.last_change = accessor.now().clone();
    if let Some(induced_acceleration) = current_varying.boundary_induced_acceleration {
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
    set (accessor, &self.circle_handle.varying, new.clone());
    update_predictions (accessor, &self.circle_handle, & new);
  }

  fn undo <Accessor: FutureCleanupAccessor <Steward = Self::Steward>> (&self, accessor: &mut Accessor, _: ()) {
    unimplemented!()
  }
}

#[derive (Clone, PartialEq, Eq, Serialize, Deserialize, Debug)]
pub struct Initialize {} //, Basics, EventId (0xa2a17317b84f96e5),
impl PersistentlyIdentifiedType for Initialize {
  const ID: PersistentTypeId = PersistentTypeId(0xbf7ba1ff2ab76640);
}
impl Event for Initialize {
  type Steward = Steward;
  type ExecutionData = ();
  fn execute <Accessor: EventAccessor <Steward = Self::Steward>> (&self, accessor: &mut Accessor) {
    let circles = accessor.globals();
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
        position: position,
        last_change: 0,
        relationships: Vec::new(),
        boundary_induced_acceleration: None,
        next_boundary_change: None,
      })
    }
    for first in 0..HOW_MANY_CIRCLES {
      for second in (first + 1)..HOW_MANY_CIRCLES {
        let relationship = Relationship {
          circles: (circles [first].clone(), circles [second].clone()),
          varying: DataTimelineCell::new(SimpleTimeline::new ()),
        };
        set (accessor, & relationship.varying, RelationshipVarying {
            induced_acceleration: None,
            next_change: None,
          });
        let relationship_handle = DataHandle::new (relationship);
        
        varying [first].relationships.push (relationship_handle.clone()) ;
        varying [second].relationships.push (relationship_handle) ;
      }
    }
    
    for index in 0..HOW_MANY_CIRCLES {
      set (accessor, & circles [index].varying, varying [index].clone());
    }
    for index in 0..HOW_MANY_CIRCLES {
      update_predictions (accessor, & circles [index], & varying [index]);
    }
  }

  fn undo <Accessor: FutureCleanupAccessor <Steward = Self::Steward>> (&self, accessor: &mut Accessor, _: ()) {
    unimplemented!()
  }
}


#[derive (Clone, PartialEq, Eq, Serialize, Deserialize, Debug)]
pub struct Disturb {pub coordinates: [SpaceCoordinate; 2]} //, Basics, EventId (0x058cb70d89116605),
impl PersistentlyIdentifiedType for Disturb {
  const ID: PersistentTypeId = PersistentTypeId(0xb8bbf65eaaf08d0e);
}
impl Event for Disturb {
  type Steward = Steward;
  type ExecutionData = ();
  fn execute <Accessor: EventAccessor <Steward = Self::Steward>> (&self, accessor: &mut Accessor) {
    let circles = accessor.globals();
    let mut best_handle = None;
    let mut best_distance_squared = i64::max_value();
    for circle in circles.iter() {
      let varying = tracking_query (accessor, &circle.varying);
      let position = varying.position.updated_by(accessor.now() - varying.last_change).unwrap().evaluate();
      let distance_squared = (self.coordinates [0] - position [0]) * (self.coordinates [0] - position [0]) + (self.coordinates [1] - position [1]) * (self.coordinates [1] - position [1]);
      if distance_squared <best_distance_squared {
        best_distance_squared = distance_squared;
        best_handle = Some (circle.clone());
      }
    }
    
    let best_handle = best_handle.unwrap() ;
    let best = tracking_query (accessor, & best_handle.varying);
    let mut new = best.clone();
    new.position.update_by(accessor.now() - best.last_change);
    new.last_change = accessor.now().clone();
    let impulse = -(new.position.evaluate() -
                            Vector2::new(ARENA_SIZE / 2,
                                         ARENA_SIZE / 2)) *
                          (ARENA_SIZE * 4 / (ARENA_SIZE ));
    new.position.add_velocity(impulse);
    set (accessor, & best_handle.varying, new.clone());
    update_predictions (accessor, & best_handle, & new);
  }

  fn undo <Accessor: FutureCleanupAccessor <Steward = Self::Steward>> (&self, accessor: &mut Accessor, _: ()) {
    unimplemented!()
  }
}

pub fn make_globals()-> <Basics as BasicsTrait>::Globals {
  let mut circles = Vec::new();
  let mut generator = DeterministicRandomId::new (&0u8).to_rng();
  
  for index in 0..HOW_MANY_CIRCLES {
    let radius = generator.gen_range(ARENA_SIZE / 30, ARENA_SIZE / 15);

    circles.push (DataHandle::new (Circle {
      index: index,
      radius: radius,
      varying: DataTimelineCell::new(SimpleTimeline::new ())
    }));
  }
  circles
}

