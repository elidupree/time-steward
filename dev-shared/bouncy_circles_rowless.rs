use time_steward::support;
use time_steward::support::time_functions::QuadraticTrajectory;
use nalgebra::Vector2;
use time_steward::support::rounding_error_tolerant_math::right_shift_round_up;


use time_steward::{DeterministicRandomId};
use time_steward::rowless::api::{self, StewardData, QueryOffset, TypedDataTimelineHandleTrait, ExtendedTime, Basics as BasicsTrait};
use time_steward::rowless::stewards::{simple_flat as steward_module};
use steward_module::{TimeSteward, ConstructibleTimeSteward, Event, DataTimelineHandle, PredictionHandle, MomentaryAccessor, EventAccessor, UndoEventAccessor, SnapshotAccessor, automatic_tracking};
use automatic_tracking::{SimpleTimeline, ConstantTimeline, GetValue, query_constant_timeline, query_simple_timeline, modify_simple_timeline, unmodify_simple_timeline};


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
  type GlobalTimeline = ConstantTimeline <Vec<DataTimelineHandle <SimpleTimeline <Circle, Basics >>>, Basics>;
}

pub type Steward = steward_module::Steward <Basics>;


#[derive (Clone, PartialEq, Eq, Serialize, Deserialize, Debug)]
pub struct Circle {
  pub index: usize,
  pub position: QuadraticTrajectory,
  pub radius: SpaceCoordinate,
  pub relationships: Vec<DataTimelineHandle <SimpleTimeline <Relationship, Basics >>>,
  pub boundary_induced_acceleration: Option <Vector2<SpaceCoordinate>>,
  pub next_boundary_change: Option <PredictionHandle <BoundaryChange>>,
}
impl StewardData for Circle {}

#[derive (Clone, PartialEq, Eq, Serialize, Deserialize, Debug)]
pub struct Relationship {
  pub circles: (DataTimelineHandle <SimpleTimeline <Circle, Basics >>, DataTimelineHandle <SimpleTimeline <Circle, Basics >>),
  pub induced_acceleration: Option <Vector2<SpaceCoordinate>>,
  pub next_change: Option <PredictionHandle <RelationshipChange>>,
}
impl StewardData for Relationship {}

pub fn query_relationship_circles <Accessor: EventAccessor <Steward = Steward>>(accessor: &mut Accessor, relationship: &Relationship)->((ExtendedTime <Basics>, Circle), (ExtendedTime <Basics>, Circle)) {
  (query_simple_timeline (accessor, & relationship.circles.0, QueryOffset::After)
      .expect("a relationship exists for a circle that doesn't"),
   query_simple_timeline (accessor, & relationship.circles.1, QueryOffset::After)
      .expect("a relationship exists for a circle that doesn't"))
}

pub fn update_relationship_change_prediction <Accessor: EventAccessor <Steward = Steward>>(accessor: &mut Accessor, relationship_handle: &DataTimelineHandle <SimpleTimeline <Relationship, Basics >>) {
  let (_, mut relationship) = query_simple_timeline (accessor, relationship_handle, QueryOffset::After).unwrap();

  let us = query_relationship_circles (accessor, &relationship);

  let time = QuadraticTrajectory::approximately_when_distance_passes((us.0).1.radius +
                                                                   (us.1).1.radius,
                                                                   if relationship.induced_acceleration.is_none() {
                                                                     -1
                                                                   } else {
                                                                     1
                                                                   },
                                                                   ((us.0).0.base.clone(),
                                                                    &(us.0).1.position),
                                                                   ((us.1).0.base.clone(),
                                                                    &(us.1).1.position));
  // println!("Planning for {} At {}, {}", id, (us.0).1, (us.1).1);
  if time.is_none() && relationship.induced_acceleration.is_some() {
    panic!(" fail {:?} {:?}", relationship, us)
  }
  
  if let Some (discarded) = relationship.next_change.take() {
    accessor.destroy_prediction (&discarded);
  }
  if let Some(yes) = time {
    if yes >= *accessor.now() {
      // println!(" planned for {}", &yes);
      relationship.next_change = Some(accessor.create_prediction (
        yes,
        DeterministicRandomId::new (&((us.0).1.index, (us.1).1.index)),
        RelationshipChange {relationship_handle: relationship_handle.clone()}
      ));
    }
  }
  modify_simple_timeline (accessor, relationship_handle, Some (relationship)) ;
}

#[derive (Clone, PartialEq, Eq, Serialize, Deserialize, Debug)]
pub struct RelationshipChange {pub relationship_handle: DataTimelineHandle <SimpleTimeline <Relationship, Basics >>} //, Basics, EventId (0x2312e29e341a2495),
impl StewardData for RelationshipChange {}
impl Event for RelationshipChange {
  type Steward = Steward;
  type ExecutionData = ();
  fn execute <Accessor: EventAccessor <Steward = Self::Steward>> (&self, accessor: &mut Accessor) {
    let (_, relationship) = query_simple_timeline (accessor, &self.relationship_handle, QueryOffset::After).unwrap();
    let us = query_relationship_circles (accessor, &relationship);
    let mut new_relationship = relationship.clone();
    let mut new = ((us.0).1.clone(), (us.1).1.clone());
    new.0.position.update_by(accessor.now() - (us.0).0.base);
    new.1.position.update_by(accessor.now() - (us.1).0.base);
    if let Some(induced_acceleration) = relationship.induced_acceleration {
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
                           (new.0.radius + new.1.radius));
      new.0.position.add_acceleration(acceleration);
      new.1.position.add_acceleration(-acceleration);
      new_relationship.induced_acceleration = Some(acceleration);
        //println!("Joined {} At {}", self.id, mutator.now());
    }
    modify_simple_timeline (accessor, & self.relationship_handle, Some (new_relationship)) ;
    modify_simple_timeline (accessor, & relationship.circles.0, Some(new.0.clone()));
    modify_simple_timeline (accessor, & relationship.circles.1, Some(new.1.clone()));
    // TODO no repeating the relationship between these 2 in particular
    update_predictions (accessor, &relationship.circles.0, & new.0);
    update_predictions (accessor, &relationship.circles.1, & new.1);
  }

  fn undo <Accessor: UndoEventAccessor <Steward = Self::Steward>> (&self, accessor: &mut Accessor, _: ()) {
    unimplemented!()
  }
}

pub fn update_boundary_change_prediction <Accessor: EventAccessor <Steward = Steward>>(accessor: &mut Accessor, circle_handle: &DataTimelineHandle <SimpleTimeline <Circle, Basics >>) {
  let arena_center = QuadraticTrajectory::new(TIME_SHIFT,
                                              MAX_DISTANCE_TRAVELED_AT_ONCE,
                                              [ARENA_SIZE / 2, ARENA_SIZE / 2, 0, 0, 0, 0]);
  let mut me = query_simple_timeline (accessor, circle_handle, QueryOffset::After)
    .expect("circles should never not exist");

  let time = QuadraticTrajectory::approximately_when_distance_passes(ARENA_SIZE - me.1.radius,
                                                                   if me.1.boundary_induced_acceleration.is_some() {
                                                                     -1
                                                                   } else {
                                                                     1
                                                                   },
                                                                   (me.0.base.clone(),
                                                                    &me.1.position),
                                                                    (0, & arena_center));
 
  if let Some (discarded) = me.1.next_boundary_change.take() {
    accessor.destroy_prediction (&discarded);
  }
  if let Some(yes) = time {
    if yes >= *accessor.now() {
      // println!(" planned for {}", &yes);
      me.1.next_boundary_change = Some(accessor.create_prediction (
        yes,
        DeterministicRandomId::new (&me.1.index),
        BoundaryChange {circle_handle: circle_handle.clone()}
      ));
    }
  }
  modify_simple_timeline (accessor, circle_handle, Some(me.1));
}

pub fn update_predictions <Accessor: EventAccessor <Steward = Steward>>(accessor: &mut Accessor, circle_handle: &DataTimelineHandle <SimpleTimeline <Circle, Basics >>, circle: &Circle) {
  for handle in circle.relationships.iter() {
    update_relationship_change_prediction (accessor, handle);
  }
  update_boundary_change_prediction (accessor, circle_handle);
}

#[derive (Clone, PartialEq, Eq, Serialize, Deserialize, Debug)]
pub struct BoundaryChange {pub circle_handle: DataTimelineHandle <SimpleTimeline <Circle, Basics >>} //, Basics, EventId (0x59732d675b2329ad),
impl StewardData for BoundaryChange {}
impl Event for BoundaryChange {
  type Steward = Steward;
  type ExecutionData = ();
  fn execute <Accessor: EventAccessor <Steward = Self::Steward>> (&self, accessor: &mut Accessor) {
    let me = query_simple_timeline (accessor, &self.circle_handle, QueryOffset::After)
      .expect("circles should never not exist");
    let mut new = me.1.clone();
    new.position.update_by(accessor.now() - me.0.base);
    if let Some(induced_acceleration) = me.1.boundary_induced_acceleration {
      new.position
        .add_acceleration(-induced_acceleration);
      new.boundary_induced_acceleration = None;
    } else {
      let acceleration = -(new.position.evaluate() -
                            Vector2::new(ARENA_SIZE / 2,
                                         ARENA_SIZE / 2)) *
                          (ARENA_SIZE * 400 / (ARENA_SIZE - me.1.radius));
      new.position.add_acceleration(acceleration);
      new.boundary_induced_acceleration = Some(acceleration);
    }
    modify_simple_timeline (accessor, &self.circle_handle, Some(new.clone()));
    update_predictions (accessor, &self.circle_handle, & new);
  }

  fn undo <Accessor: UndoEventAccessor <Steward = Self::Steward>> (&self, accessor: &mut Accessor, _: ()) {
    unimplemented!()
  }
}

#[derive (Clone, PartialEq, Eq, Serialize, Deserialize, Debug)]
pub struct Initialize {} //, Basics, EventId (0xa2a17317b84f96e5),
impl StewardData for Initialize {}
impl Event for Initialize {
  type Steward = Steward;
  type ExecutionData = ();
  fn execute <Accessor: EventAccessor <Steward = Self::Steward>> (&self, accessor: &mut Accessor) {
    
    let circle_handles = query_constant_timeline (accessor, accessor.global_timeline());
    let mut circles = Vec::new();
    for index in 0..HOW_MANY_CIRCLES {
      let thingy = ARENA_SIZE / 20;
      let radius = accessor.gen_range(ARENA_SIZE / 30, ARENA_SIZE / 15);

      let position =
      QuadraticTrajectory::new(TIME_SHIFT,
                              MAX_DISTANCE_TRAVELED_AT_ONCE,
                              [accessor.gen_range(0, ARENA_SIZE),
                               accessor.gen_range(0, ARENA_SIZE),
                               accessor.gen_range(-thingy, thingy),
                               accessor.gen_range(-thingy, thingy),
                               0,
                               0]);
      
      circles.push (Circle {
        index: index,
        position: position,
        radius: radius,
        relationships: Vec::new(),
        boundary_induced_acceleration: None,
        next_boundary_change: None,
      });
    }
    for first in 0..HOW_MANY_CIRCLES {
      for second in (first + 1)..HOW_MANY_CIRCLES {
        let relationship = Relationship {
          circles: (circle_handles [first].clone(), circle_handles [second].clone()),
          induced_acceleration: None,
          next_change: None,
        };
        let relationship_handle = DataTimelineHandle::new (SimpleTimeline::new ());
        
        modify_simple_timeline (accessor, & relationship_handle, Some (relationship));
        circles [first].relationships.push (relationship_handle.clone()) ;
        circles [second].relationships.push (relationship_handle) ;
      }
    }
    
    for index in 0..HOW_MANY_CIRCLES {
      modify_simple_timeline (accessor, & circle_handles [index], Some (circles [index].clone()));
    }
    for index in 0..HOW_MANY_CIRCLES {
      update_predictions (accessor, & circle_handles [index], & circles [index]);
    }
  }

  fn undo <Accessor: UndoEventAccessor <Steward = Self::Steward>> (&self, accessor: &mut Accessor, _: ()) {
    unimplemented!()
  }
}


#[derive (Clone, PartialEq, Eq, Serialize, Deserialize, Debug)]
pub struct Disturb {pub coordinates: [SpaceCoordinate; 2]} //, Basics, EventId (0x058cb70d89116605),
impl StewardData for Disturb {}
impl Event for Disturb {
  type Steward = Steward;
  type ExecutionData = ();
  fn execute <Accessor: EventAccessor <Steward = Self::Steward>> (&self, accessor: &mut Accessor) {
    let circles = query_constant_timeline (accessor, accessor.global_timeline());
    let mut best_handle = None;
    let mut best_distance_squared = i64::max_value();
    for circle_handle in circles.iter() {
      let (time, circle) = query_simple_timeline (accessor, circle_handle, QueryOffset::After).expect ("circles should never not exist");
      let position = circle.position.updated_by(accessor.now() - time.base).unwrap().evaluate();
      let distance_squared = (self.coordinates [0] - position [0]) * (self.coordinates [0] - position [0]) + (self.coordinates [1] - position [1]) * (self.coordinates [1] - position [1]);
      if distance_squared <best_distance_squared {
        best_distance_squared = distance_squared;
        best_handle = Some (circle_handle.clone());
      }
    }
    
    let best_handle = best_handle.unwrap() ;
    let (time, best) = query_simple_timeline (accessor, & best_handle, QueryOffset::After).expect ("uhhhh");
    let mut new = best.clone();
    new.position.update_by(accessor.now() - time.base);
    let impulse = -(new.position.evaluate() -
                            Vector2::new(ARENA_SIZE / 2,
                                         ARENA_SIZE / 2)) *
                          (ARENA_SIZE * 4 / (ARENA_SIZE ));
    new.position.add_velocity(impulse);
    modify_simple_timeline (accessor, & best_handle, Some(new.clone()));
    update_predictions (accessor, & best_handle, & new);
  }

  fn undo <Accessor: UndoEventAccessor <Steward = Self::Steward>> (&self, accessor: &mut Accessor, _: ()) {
    unimplemented!()
  }
}

