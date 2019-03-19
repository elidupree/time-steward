#![feature (custom_attribute)]

extern crate serde;
#[macro_use]
extern crate serde_derive;
#[macro_use]
extern crate derivative;
extern crate array_ext;

extern crate time_steward;

// Imports for the UI
#[macro_use]
extern crate glium;
#[path = "../dev-shared/emscripten_compatibility.rs"] mod emscripten_compatibility;
pub use crate::emscripten_compatibility::canvas_click;

#[allow (unused_macros)]
macro_rules! printlnerr(
    ($($arg:tt)*) => { {use std::io::Write;
        let r = writeln!(&mut ::std::io::stderr(), $($arg)*);
        r.expect("failed printing to stderr");
    } }
);


use std::cmp::{min, max};


use time_steward::{DeterministicRandomId};
use time_steward::{DataHandleTrait, DataTimelineCellTrait, Basics as BasicsTrait};
use time_steward::type_utils::{PersistentTypeId, PersistentlyIdentifiedType};
use time_steward::type_utils::list_of_types::{ListedType};
use time_steward::stewards::{simple_full as steward_module};
use crate::steward_module::{TimeSteward, ConstructibleTimeSteward, IncrementalTimeSteward, Event, DataHandle, DataTimelineCell, EventHandle, Accessor, EventAccessor, FutureCleanupAccessor, simple_timeline};
use crate::simple_timeline::{SimpleTimeline, query, query_ref, set, just_destroyed};

#[path = "../dev-shared/tree_continuum.rs"] mod tree_continuum;
use crate::tree_continuum::*;

type Time = i64;
type Distance = i64;
type Amount = i64;
const SECOND: Time = (1 as Time) << 10;
const METER: Distance = (1 as Distance) << 10;
const GENERIC_DENSITY: Amount = (SECOND as Amount) << 10;
const GENERIC_INK_AMOUNT: Amount = (METER as Amount)*(METER as Amount)*GENERIC_DENSITY;

// Velocity is "ink units per space unit per time unit"
// and slope is "ink density per space unit", i.e. "ink units per space unit^3"
// 
// Let's say we want flow across a meter^2 cell, from a density of GENERIC_INK_AMOUNT/m^2 to 0, to drain GENERIC_INK_AMOUNT per second. Then
//   slope = GENERIC_INK_AMOUNT/m^3
//   velocity = GENERIC_INK_AMOUNT/m*s
// so
const VELOCITY_PER_SLOPE: Amount = (METER as Amount)*(METER as Amount) / (SECOND as Amount);

type Steward = steward_module::Steward <Basics>;



#[derive (Copy, Clone, Hash, PartialEq, Eq, PartialOrd, Ord, Serialize, Deserialize, Debug, Default)]
struct Physics {}
impl PersistentlyIdentifiedType for Physics {
  const ID: PersistentTypeId = PersistentTypeId(0x4fd93495079caefe);
}
impl TreeContinuumPhysics for Physics {
  const DIMENSIONS: usize = 2;
  type Steward = Steward;
  type NodeVarying = NodeVarying;
  type BoundaryVarying = BoundaryVarying;
  
  //fn before_split <A: EventAccessor <Steward = Self::Steward>> (accessor: &A, splitting_node: & NodeHandle) {}
  fn initialize_split_child <A: EventAccessor <Steward = Self::Steward>> (accessor: &A, child: NewChildInfo<Self>)->Self::NodeVarying {
    NodeVarying {
      last_change: *accessor.now(),
      ink_at_last_change: child.splitting_varying.data.ink_at_last_change >> 2,
      accumulation_rate: 0,
      slope: [0; 2],
    }
  }
  fn initialize_split_boundary <A: EventAccessor <Steward = Self::Steward>> (accessor: &A, boundary: NewBoundaryInfo<Self>)->Self::BoundaryVarying {
    BoundaryVarying {
      transfer_velocity: match boundary.old_boundary {None => 0, Some (previous) => query_ref (accessor, &previous.varying).data.transfer_velocity},
      next_change: None,
    }
  }
  fn after_split <A: EventAccessor <Steward = Self::Steward>> (accessor: &A, split_node: & NodeHandle, new_boundaries: Vec<BoundaryHandle>) {
    let guard = query_ref (accessor, & split_node.varying);
    let branch_varying = unwrap_branch_ref(&guard);
  
    iterate_children (& branch_varying.children, | _coordinates, child | {
      update_node(accessor, child);
      update_inferred_node_properties (accessor, child);
    });
    
    for boundary in new_boundaries {
      //printlnerr!("{:?}", (boundary.center, split_node.center, split_node.width, boundary.nodes[0].center, boundary.nodes[0].width, boundary.nodes[1].center, boundary.nodes[1].width));
      update_transfer_change_prediction (accessor, &boundary);
    }
  }
  
  //fn before_merge <A: EventAccessor <Steward = Self::Steward>> (accessor: &A, merging_node: & NodeHandle <Self>) {}
  //fn initialize_merge_parent <A: EventAccessor <Steward = Self::Steward>> (accessor: &A)->Self::NodeVarying;
  fn initialize_merge_boundary <A: EventAccessor <Steward = Self::Steward>> (_accessor: &A, _boundary: MergeBoundaryInfo<Self>)->Self::BoundaryVarying {
    BoundaryVarying {
      transfer_velocity: 0,
      next_change: None,
    }
  }
  //fn after_merge <A: EventAccessor <Steward = Self::Steward>> (accessor: &A, merged_node: & NodeHandle <Self>, new_boundaries: Vec<BoundaryHandle <Self>>) {}
}

macro_rules! serialization_cheat {
  (
    [$($bounds:tt)*]
    [$($concrete:tt)*]
  ) => {

impl <$($bounds)*> $crate::serde::Serialize for $($concrete)* {
  fn serialize <S: $crate::serde::Serializer> (&self, _: S)->Result <S::Ok, S::Error> {
    unimplemented!()
  }
}

impl <'a, $($bounds)*> $crate::serde::Deserialize <'a> for $($concrete)* {
  fn deserialize <D: $crate::serde::Deserializer<'a>> (_: D)->Result <Self, D::Error> {
    unimplemented!()
  }
}

  };
}


#[derive (Clone, PartialEq, Eq, Serialize, Deserialize, Debug)]
struct NodeVarying {
  last_change: Time,
  ink_at_last_change: Amount,  
  accumulation_rate: Amount,
  slope: [Amount; 2],
}
type NodeHandle = DataHandle <tree_continuum::NodeData <Physics>>;
//serialization_cheat!([][NodeVarying]);

#[derive (Clone, PartialEq, Eq, Serialize, Deserialize, Debug)]
struct BoundaryVarying {
  transfer_velocity: Amount,
  next_change: Option <EventHandle <Basics>>,
}
type BoundaryHandle = DataHandle <tree_continuum::BoundaryData <Physics>>;
//serialization_cheat!([][BoundaryVarying]);

/*
macro_rules! get {
  ($accessor: expr, $cell: expr) => {
    query ($accessor, $cell)
  }
}
macro_rules! set {
  ($accessor: expr, $cell: expr, $field: ident, $value: expr) => {
    {
      let mut value = query ($accessor, $cell);
      value.$field = $value;
      set ($accessor, $cell, value);
    }
  }
}*/
macro_rules! set_with {
  ($accessor: expr, $cell: expr, | $varying: ident | $actions: expr) => {
    {
      let mut $varying = query ($accessor, $cell);
      $actions;
      set ($accessor, $cell, $varying);
    }
  }
}
/*macro_rules! exists {
  ($accessor: expr, $cell: expr) => {
    $accessor.query ($cell, &GetVarying).is_some()
  }
}*/

fn set_leaf<A: EventAccessor <Steward = Steward >> (accessor: &A, varying: & DataTimelineCell <SimpleTimeline <tree_continuum::NodeVarying<Physics>, Steward>>, data:tree_continuum::LeafVarying<Physics>) {
  set (accessor, &varying, tree_continuum::NodeVarying::Leaf (data));
}


/*struct EventContext <'a, A: EventAccessor <Steward = Steward>> {
  accessor: &'a A,
  node_clones: HashMap <DataHandle <NodeData>, Rc<RefCell<Node>>,
  boundary_clones: HashMap <DataHandle <NodeData>, Rc<RefCell<Node>>,
}
impl<A: EventAccessor <Steward = Steward>> EventContext <A> {
  fn new (accessor: & A)->Self {
    EventContext {
      accessor: accessor,
      node_clones: HashMap::new(),
      boundary_clones: HashMap::new(),
    }
  }
  fn get_node (&self, handle: & DataHandle <NodeData>)->Rc {
    self.node_clones.insert_or (self.accessor.query (
  }
}
impl<A: EventAccessor <Steward = Steward>> Drop for EventContext <A> {
  fn drop (&mut self) {
    for (handle, node) in self.node_clones.iter() {
      if node.modified {
        set (self.accessor, handle, node.varying);
      }
    }
    for (handle, boundary) in self.node_clones.iter() {
      if boundary.modified {
        set (self.accessor, handle, boundary.varying);
      }
    }
  }
}*/

fn update_inferred_node_properties <A: EventAccessor <Steward = Steward >> (accessor: &A, node: &NodeHandle) {
  let mut varying = unwrap_leaf (query (accessor, &node.varying));
  varying.data.slope = [0, 0];
  varying.data.accumulation_rate = 0;
  
  {
    let data = &mut varying.data;
    iterate_boundaries (& varying.boundaries, | dimension, direction, boundary | {
      let direction_signum = (direction as Amount*2)-1;
      let transfer_rate = query_ref (accessor, & boundary.varying).data.transfer_velocity*boundary.length as Amount;
      data.accumulation_rate -= transfer_rate*direction_signum;
      data.slope [dimension] -= transfer_rate; // divided by
      // (2*node.width*VELOCITY_PER_SLOPE), but we do that later as an optimization
    });
    for dimension in 0..2 {
      data.slope [dimension] /= (2*node.width as Amount)*VELOCITY_PER_SLOPE;
    }
  }
  set_leaf (accessor, &node.varying, varying);
}

fn update_node <A: EventAccessor <Steward = Steward >> (accessor: &A, node: &NodeHandle) {
  let mut varying = unwrap_leaf (query (accessor, &node.varying));
  
  varying.data.ink_at_last_change = ink_at (accessor, node, *accessor.now());
  varying.data.last_change = *accessor.now();
  
  set_leaf (accessor, &node.varying, varying);
}

fn ink_at <A: Accessor <Steward = Steward >> (accessor: &A, node: &NodeHandle, time: Time)->Amount {
  let guard = query_ref (accessor, &node.varying);
  let varying = unwrap_leaf_ref (&guard);
  varying.data.ink_at_last_change + varying.data.accumulation_rate*((time - varying.data.last_change) as Amount)
}
fn density_at <A: Accessor <Steward = Steward >> (accessor: &A, node: &NodeHandle, time: Time)->Amount {
  ink_at (accessor, node, time)/((node.width*node.width) as Amount)
}

/*fn inferred_density_trajectory_at <A: Accessor <Steward = Steward >> (accessor: &A, node: &NodeHandle, location: [Amount; 2], time: Time)->[Amount; 2] {
  let area = node.width*node.width;
  [
    ink_at(node, time)/area
      + node.slope [0]*(location [0] - node.center [0])
      + node.slope [1]*(location [1] - node.center [1]),
    node.accumulation_rate/area
  ]
}*/


fn density_difference_to_ideal_transfer_velocity (nodes: [& NodeHandle; 2], density_difference: Amount)->Amount {
  //let ideal_transfer_slope = density_difference*2/((nodes [0].width + nodes [1].width) as Amount);
  //ideal_transfer_slope*VELOCITY_PER_SLOPE
  density_difference*(2*VELOCITY_PER_SLOPE)/((nodes [0].width + nodes [1].width) as Amount)
}
fn transfer_velocity_to_ideal_density_difference (nodes: [&NodeHandle;2], transfer_velocity: Amount)->Amount {
  //let ideal_transfer_slope = transfer_velocity/VELOCITY_PER_SLOPE;
  //ideal_transfer_slope*((nodes [0].width + nodes [1].width) as Amount)/2
  transfer_velocity*((nodes [0].width + nodes [1].width) as Amount)/(2*VELOCITY_PER_SLOPE)
}


fn safer_scaling (value: Amount, numerator: Amount, denominator: Amount)->Amount {
  if value == 0 || numerator == 0 { return 0; }
  if numerator.abs() > denominator.abs()*50 {
    value*(numerator/denominator)
  }
  else if value.abs() > denominator.abs()*50 {
    numerator*(value/denominator)
  }
  else if denominator.abs() > numerator.abs()*50 {
    value/(denominator/numerator)
  }
  else if denominator.abs() > value.abs()*50 {
    numerator/(denominator/value)
  }
  else {
    (value*numerator)/denominator
  }
}


fn update_transfer_change_prediction <A: EventAccessor <Steward = Steward >> (accessor: &A, boundary: &BoundaryHandle) {
  set_with!(accessor, &boundary.varying, | boundary_varying | {
  let nodes = &boundary.nodes;
  
  let areas = [
    (nodes [0].width*nodes [0].width) as Amount,
    (nodes [1].width*nodes [1].width) as Amount,
  ];
  let densities = [
    density_at (accessor, &nodes[0], *accessor.now()),
    density_at (accessor, &nodes[1], *accessor.now()),
  ];
  let accumulation_rates = [
    unwrap_leaf_ref (&query_ref(accessor, &nodes[0].varying)).data.accumulation_rate,
    unwrap_leaf_ref (&query_ref(accessor, &nodes[1].varying)).data.accumulation_rate,
  ];
  let scale_factor = areas [0]*areas [1];
  let density_accumulations_scaled = [
    accumulation_rates [0]*areas [1],
    accumulation_rates [1]*areas [0],
  ];
  let difference_now = densities[0] - densities[1];
  let density_accumulation_difference_scaled = density_accumulations_scaled [0] - density_accumulations_scaled [1];
  let numerator = accumulation_rates [0] * areas[1] - accumulation_rates [1] * areas[0];
  let denominator = 4*(nodes[0].width as Amount * areas[1] + nodes[1].width as Amount * areas[0]);
  let max_reasonable_velocity_skew = numerator/denominator;
  
  // the more unstable the boundary is, the more error we permit
  let max_rounding_error = max(VELOCITY_PER_SLOPE, (nodes [0].width + nodes [1].width) as Amount);
  let permissible_error = 2*max_rounding_error + max_reasonable_velocity_skew.abs() + GENERIC_INK_AMOUNT/(SECOND as Amount)/(METER as Amount)/1000000;// + density_accumulation_difference.abs()*boundary.length as Amount/8;
  
  // Note to self: it may look weird for the boundary conditions to be based on
  // the CURRENT transfer_velocity, when it can change, but remember
  // that "when the current gets too far from the ideal" is the same as
  // "when the ideal gets too far from the current"
  let (min_difference, max_difference) = (
    transfer_velocity_to_ideal_density_difference([&nodes[0], &nodes[1]], boundary_varying.data.transfer_velocity - permissible_error),
    transfer_velocity_to_ideal_density_difference([&nodes[0], &nodes[1]], boundary_varying.data.transfer_velocity + permissible_error),
  );
  let time = if difference_now < min_difference || difference_now > max_difference {
    Some(*accessor.now())
  } else if density_accumulation_difference_scaled > 0 {
    Some(*accessor.now() + safer_scaling (max_difference-difference_now, scale_factor, density_accumulation_difference_scaled) as Time)
  }
  else if density_accumulation_difference_scaled < 0 {
    Some(*accessor.now() + safer_scaling (min_difference-difference_now, scale_factor, density_accumulation_difference_scaled) as Time)
  }
  else {
    None
  };
  //printlnerr!("{:?}", (density_accumulation_difference.signum(), max_reasonable_velocity_skew.signum()));
  //printlnerr!("{:?}", (*accessor.now()*1000/SECOND, time.map(|time|time*1000/SECOND), difference_now, density_accumulation_difference_scaled*SECOND as Amount/scale_factor, max_reasonable_velocity_skew*boundary.length as Amount / areas[0], - max_reasonable_velocity_skew*boundary.length as Amount / areas[1], (min_difference, difference_now, max_difference), boundary_varying.transfer_velocity, density_difference_to_ideal_transfer_velocity ([&nodes[0], &nodes[1]], difference_now), max_reasonable_velocity_skew, permissible_error));
  
  boundary_varying.data.next_change = time.map (|time| {
    accessor.create_prediction (
      time,
      DeterministicRandomId::new (&(accessor.id(), boundary.center)),
      TransferChange {boundary: boundary.clone()}
    )
  });
  
  });
}


fn maybe_split <A: EventAccessor <Steward = Steward >> (accessor: &A, node: &NodeHandle)->bool {
  if node.width <METER {return false;}
  
  let mut close_enough = true;
  {
  let guard = query_ref (accessor, &node.varying);
  let varying = unwrap_leaf_ref (&guard);
  let mut averaged_ideal_velocities = [0; 2];
  
  iterate_boundaries (& varying.boundaries, | dimension, _direction, boundary | {
    let density_difference =
      density_at (accessor, &boundary.nodes[0], *accessor.now()) - 
      density_at (accessor, &boundary.nodes[1], *accessor.now());
    let ideal_velocity = density_difference_to_ideal_transfer_velocity ([&boundary.nodes[0], &boundary.nodes[0]], density_difference);
    let ideal_rate = ideal_velocity*boundary.length as Amount;
    averaged_ideal_velocities [dimension] -= ideal_rate;
  });
  for dimension in 0..2 {
    averaged_ideal_velocities [dimension] /= 2*node.width as Amount;
  }
  
  iterate_boundaries (& varying.boundaries, | dimension, _direction, boundary | {
    let density_difference =
      density_at (accessor, &boundary.nodes[0], *accessor.now()) - 
      density_at (accessor, &boundary.nodes[1], *accessor.now());
    let ideal_velocity = density_difference_to_ideal_transfer_velocity ([&boundary.nodes[0], &boundary.nodes[0]], density_difference);
    let averaged_velocity = averaged_ideal_velocities [dimension];
    let discrepancy = (ideal_velocity - averaged_velocity).abs();
    // TODO: should this be proportional in some way to the size of the node(s)?
    if discrepancy*(boundary.length as Amount) > GENERIC_INK_AMOUNT/(SECOND as Amount) {
      close_enough = false;
    }
  });
  }
  
  if !close_enough {
    tree_continuum::split (accessor, node);
  }
  !close_enough
}


fn maybe_merge <A: EventAccessor <Steward = Steward >> (accessor: &A, node: &NodeHandle)->bool {
  if just_destroyed (accessor, &node.varying) { return false; }
  let mut close_enough = true;
  {
  let guard = query_ref (accessor, &node.varying);
  let varying = match *guard  {
    tree_continuum::NodeVarying::Branch (ref b) => b,
    tree_continuum::NodeVarying::Leaf (_) => return false,
  };
  
  let mut averaged_ideal_velocities = [0; 2];
  let mut abort = false;
  iterate_children (& varying.children, | _coordinates, child | {
    let child_guard = query_ref (accessor, &child.varying);
    let child_varying = match *child_guard  {
      tree_continuum::NodeVarying::Branch (_) => {abort = true; return},
      tree_continuum::NodeVarying::Leaf (ref l) => l,
    };
    for dimension in 0..2 {
      for direction in 0..2 {
        match face_by_dimension_and_direction (&child_varying.boundaries, dimension, direction) {
          &FaceBoundaries::WorldEdge => (),
          &FaceBoundaries::SplitBoundary (_) => {abort = true; return},
          &FaceBoundaries::SingleBoundary (ref boundary) => {
            let density_difference =
              density_at (accessor, &boundary.nodes[0], *accessor.now()) - 
              density_at (accessor, &boundary.nodes[1], *accessor.now());
            let ideal_velocity = density_difference_to_ideal_transfer_velocity ([&boundary.nodes[0], &boundary.nodes[0]], density_difference);
            let ideal_rate = ideal_velocity*boundary.length as Amount;
            averaged_ideal_velocities [dimension] -= ideal_rate;
          },
        };
      }
    }
  });
  if abort {return false}
  for dimension in 0..2 {
    averaged_ideal_velocities [dimension] /= 4*node.width as Amount;
  }
  
  iterate_children (& varying.children, | _coordinates, child | {
    let child_guard = query_ref (accessor, &child.varying);
    let child_varying = unwrap_leaf_ref (&child_guard);
    iterate_boundaries (& child_varying.boundaries, | dimension, _direction, boundary | {
      let density_difference =
        density_at (accessor, &boundary.nodes[0], *accessor.now()) - 
        density_at (accessor, &boundary.nodes[1], *accessor.now());
      let ideal_velocity = density_difference_to_ideal_transfer_velocity ([&boundary.nodes[0], &boundary.nodes[0]], density_difference);
      let averaged_velocity = averaged_ideal_velocities [dimension];
      let discrepancy = (ideal_velocity - averaged_velocity).abs();
      // TODO: should this be proportional in some way to the size of the node(s)?
      if discrepancy*(boundary.length as Amount) > GENERIC_INK_AMOUNT/(SECOND as Amount)/4 {
        close_enough = false;
      }
    });
  });
  }
  
  if close_enough {
    merge (accessor, node);
  }
  close_enough
}

fn merge <A: EventAccessor <Steward = Steward >> (accessor: &A, node: &NodeHandle) {
  let mut new_varying;
  {
    let guard = query_ref (accessor, &node.varying);
    let branch_varying = unwrap_branch_ref(&guard);
    new_varying = NodeVarying {
      last_change: *accessor.now(),
      ink_at_last_change: 0,
      accumulation_rate: 0,
      slope: [0; 2],
    };
  
    iterate_children (& branch_varying.children, | _coordinates, child | {
      update_node(accessor, child);
      let child_guard = query_ref (accessor, &child.varying);
      let child_varying = unwrap_leaf_ref (&child_guard);
  
      new_varying.ink_at_last_change += child_varying.data.ink_at_last_change;
    });
  }
  
  tree_continuum::merge (accessor, node, new_varying);
  
  update_inferred_node_properties (accessor, node);
  
  {
    let guard = query_ref (accessor, &node.varying);
    let leaf_varying = unwrap_leaf_ref(&guard);
    
    iterate_boundaries (& leaf_varying.boundaries, | _dimension, _direction, boundary | {
      update_transfer_change_prediction (accessor, boundary);
    });
  }
  
  if let Some(parent) = node.parent.as_ref() {
    //audit(accessor);
    maybe_merge (accessor, parent);
    //audit(accessor);
  }
  
  /*for neighbor in neighbors {
    maybe_merge (accessor, &neighbor);
  }*/
}


#[derive (Clone, PartialEq, Eq, Serialize, Deserialize, Debug)]
struct Globals {
  size: [Distance; 2],

  
  root: NodeHandle,
}



/// The TransferChange event, as used above.
#[derive (Clone, PartialEq, Eq, Serialize, Deserialize, Debug)]
struct TransferChange {boundary: BoundaryHandle}
impl PersistentlyIdentifiedType for TransferChange {
  const ID: PersistentTypeId = PersistentTypeId(0xd6621e9cfad1c765);
}
impl Event for TransferChange {
  type Steward = Steward;
  type ExecutionData = ();
  fn execute <Accessor: EventAccessor <Steward = Self::Steward>> (&self, accessor: &mut Accessor) {
    let nodes = &self.boundary.nodes;
    for node in nodes.iter() {
      update_node (accessor, node);
    }
    let areas = [
      (nodes [0].width*nodes [0].width) as Amount,
      (nodes [1].width*nodes [1].width) as Amount,
    ];
    let densities = [
      density_at (accessor, &nodes[0], *accessor.now()),
      density_at (accessor, &nodes[1], *accessor.now()),
    ];
    let mut accumulation_rates = [
      unwrap_leaf_ref (&query_ref(accessor, &nodes[0].varying)).data.accumulation_rate,
      unwrap_leaf_ref (&query_ref(accessor, &nodes[1].varying)).data.accumulation_rate,
    ];
    let difference_now = densities[0] - densities[1];
    let physics_ideal_velocity = density_difference_to_ideal_transfer_velocity ([&nodes[0], &nodes[1]], difference_now);
    set_with!(accessor, &self.boundary.varying, | boundary_varying | {
      // for the sake of this math, assume that the accumulation rates have already been updated based on the physics-wise ideal value
      accumulation_rates [0] -= (physics_ideal_velocity - boundary_varying.data.transfer_velocity)*self.boundary.length as Amount;
      accumulation_rates [1] += (physics_ideal_velocity - boundary_varying.data.transfer_velocity)*self.boundary.length as Amount;
      // Select a velocity that would neutralize the relative density-accumulation if the rest of each node's perimeter was shifted in a similar way.
      // (accumulation0 - shift*perimeter0)/w0^2 == (accumulation1 + shift*perimeter1)/w1^2.
      // After a little algebra, 
      // (accumulation0*w1^2-accumulation1*w0^2)/(perimeter0*w1^2+perimeter1*w0^2) == shift.
      let numerator = accumulation_rates [0] * areas[1] - accumulation_rates [1] * areas[0];
      let denominator = 4*(nodes[0].width as Amount * areas[1] + nodes[1].width as Amount * areas[0]);
      let max_reasonable_velocity_skew = numerator/denominator;
      assert!(
        (accumulation_rates [0]*areas[1]-accumulation_rates [1]*areas[0]).abs()
        >=
        (
         (accumulation_rates [0] - max_reasonable_velocity_skew*self.boundary.length as Amount)*areas[1]
        -(accumulation_rates [1] + max_reasonable_velocity_skew*self.boundary.length as Amount)*areas[0]
        ).abs()
      );
      //printlnerr!("{:?}", max_reasonable_velocity_skew);
      boundary_varying.data.transfer_velocity = physics_ideal_velocity + numerator.signum()+max_reasonable_velocity_skew/2;
    });
    for node in nodes.iter() {
      update_inferred_node_properties (accessor, node);
    }
    for node in nodes.iter() {
      let guard = query_ref(accessor, &node.varying);
      iterate_boundaries (& unwrap_leaf_ref (&guard).boundaries, | _dimension, _direction, boundary | {
        update_transfer_change_prediction (accessor, boundary);
      });
    }
    
    let mut split = false;
    for node in nodes.iter() {
      if match *query_ref(accessor, &node.varying){tree_continuum::NodeVarying::Leaf(_)=>true, _=>false,} {
        //audit(accessor);
        if maybe_split (accessor, node) {split=true;}
        //audit(accessor);
      }
    }
    if !split {
      for node in nodes.iter() {
        if let Some(parent) = node.parent.as_ref() {
          //audit(accessor);
          maybe_merge (accessor, parent);
          //audit(accessor);
        }
      }
    }
    
    // TODO: invalidation
  }

  fn undo <Accessor: FutureCleanupAccessor <Steward = Self::Steward>> (&self, _accessor: &mut Accessor, _: ()) {
    unimplemented!()
  }
}


#[derive (Clone, PartialEq, Eq, Debug)]
struct AddInk {coordinates: [Distance; 2], amount: Amount, accumulation: Amount}
serialization_cheat!([][AddInk]);
impl PersistentlyIdentifiedType for AddInk {
  const ID: PersistentTypeId = PersistentTypeId(0x3e6d029c3da8b9a2);
}
impl Event for AddInk {
  type Steward = Steward;
  type ExecutionData = ();
  fn execute <Accessor: EventAccessor <Steward = Self::Steward>> (&self, accessor: &mut Accessor) {
    let mut node = accessor.globals().root.clone();
    loop {
      match query (accessor, &node.varying) {
        tree_continuum::NodeVarying::Branch (ref b) => node = child_by_coordinates (& b.children,
           [if self.coordinates [0] > node.center [0] {1} else {0},
            if self.coordinates [1] > node.center [1] {1} else {0}]).clone(),
        tree_continuum::NodeVarying::Leaf (_) => {
          update_node (accessor, &node);
          if node.width <= METER {
            break
          }
          split (accessor, &node);
        },
      };
    }
    let mut varying = unwrap_leaf (query (accessor, &node.varying));
    varying.data.ink_at_last_change += self.amount;
    //node.fiat_accumulation_rate += self.accumulation;
    let changed_boundaries = varying.boundaries.clone();
    set_leaf (accessor, &node.varying, varying) ;
    
    iterate_boundaries (& changed_boundaries, | _dimension, _direction, boundary | {
      update_transfer_change_prediction (accessor, boundary);
    });
  }
  fn undo <Accessor: FutureCleanupAccessor <Steward = Self::Steward>> (&self, _accessor: &mut Accessor, _: ()) {
    unimplemented!()
  }
}


#[derive (Clone, PartialEq, Eq, Serialize, Deserialize, Debug)]
struct Initialize {}
impl PersistentlyIdentifiedType for Initialize {
  const ID: PersistentTypeId = PersistentTypeId(0xf0d2d9134cfe9b49);
}
impl Event for Initialize {
  type Steward = Steward;
  type ExecutionData = ();
  fn execute <Accessor: EventAccessor <Steward = Self::Steward>> (&self, accessor: &mut Accessor) {
    let globals = accessor.globals();
    set_leaf (accessor, &globals.root.varying, tree_continuum::LeafVarying {
      data: NodeVarying {
        last_change: 0,
        ink_at_last_change: 0,
        accumulation_rate: 0,
        slope: [0; 2],
      },
      boundaries: faces_from_fn (|_,_| tree_continuum::FaceBoundaries::WorldEdge),
    });
  }
  fn undo <Accessor: FutureCleanupAccessor <Steward = Self::Steward>> (&self, _accessor: &mut Accessor, _: ()) {
    unimplemented!()
  }
}

fn make_globals()-> Globals {
  Globals {
    size: [64*METER, 64*METER],
    root: DataHandle::new_for_globals (tree_continuum::NodeData {
      width: 64*METER,
      center: [0; 2],
      parent: None,
      varying: DataTimelineCell::new (SimpleTimeline::new ()),
    }),
  }
}

/// Finally, define the Basics type.
#[derive (Copy, Clone, Hash, PartialEq, Eq, PartialOrd, Ord, Serialize, Deserialize, Debug, Default)]
struct Basics {}
impl BasicsTrait for Basics {
  type Time = Time;
  type Globals = Globals;
  type Types = (ListedType <TransferChange>, ListedType <Initialize>, ListedType <AddInk>);
}

use std::time::{Instant, Duration};
use glium::{DisplayBuild, Surface};

#[derive(Copy, Clone)]
struct Vertex {
  center: [f32; 2],
  slope: [f32; 2],
  direction: [f32; 2],
  density: f32,
}
implement_vertex!(Vertex, center, slope, direction, density);

fn main() {
  let mut steward: Steward = Steward::from_globals(make_globals());
  steward.insert_fiat_event(0, DeterministicRandomId::new(&0), Initialize{}).unwrap();
  run (steward, |_,_|());
}


fn run <F: Fn (&mut Steward, Time)>(mut stew: Steward, settle:F) {


  let vertex_shader_source = r#"
#version 100
attribute lowp vec2 center;
attribute lowp vec2 slope;
attribute lowp vec2 direction;
attribute lowp float density;
varying lowp float density_transfer;

void main() {
gl_Position = vec4 (center+direction*0.95, 0.0, 1.0);

density_transfer = density + dot(direction, slope);
}

"#;

  let fragment_shader_source = r#"
#version 100
varying lowp float density_transfer;

void main() {
gl_FragColor = vec4 (vec3(0.5 - density_transfer/2.0), 1.0);
}

"#;

  let mut event_index = 0u64;
  let mut mouse_coordinates = [0,0];

  let display = glium::glutin::WindowBuilder::new()
    .with_dimensions(600, 600)
    .build_glium()
    .expect("failed to create window");
  let program =
    glium::Program::from_source(&display, vertex_shader_source, fragment_shader_source, None)
      .expect("glium program generation failed");
  let parameters = glium::DrawParameters {
    blend: glium::draw_parameters::Blend::alpha_blending(),
    ..Default::default()
  };
  let indices = glium::index::NoIndices(glium::index::PrimitiveType::TrianglesList);

  // take care of the expensive predictions before starting the timer
  stew.snapshot_before(&1);
  let mut previous_real_time = Instant::now();
  let mut previous_time = 1;
  let mut display_state = 0;
  let mut unrestricted_speed = false;
  let mut input_magnitude_shift = 0;
  let mut input_signum = 1;
  let mut input_derivative = 0;

  let frame = || {
    let frame_begin = Instant::now();
    let real_duration_in_simulation_units = ((previous_real_time.elapsed().as_secs() as i64 * 1000000000i64) +
                              previous_real_time.elapsed().subsec_nanos() as i64)
                             / (1000000000i64 / SECOND);
    let time = previous_time + min (SECOND/10, real_duration_in_simulation_units);
    previous_time = time;
    previous_real_time = frame_begin.clone();
    
    let accessor = stew.snapshot_before(& time)
      .expect("steward failed to provide snapshot");
    stew.forget_before (& time);
    settle (&mut stew, time);
    
    for ev in display.poll_events() {
      match ev {
        glium::glutin::Event::Closed => return true,
        glium::glutin::Event::MouseMoved (x,y) => {
          let display_size = display.get_window().unwrap().get_inner_size_pixels().unwrap();
          mouse_coordinates [0] = 
                                          (x as Distance)  * accessor.globals().size [0]
            / display_size.0 as Distance - accessor.globals().size [0]/2;
          mouse_coordinates [1] =
            (display_size.1 as Distance - (y as Distance)) * accessor.globals().size [1]
            / display_size.1 as Distance - accessor.globals().size [1]/2;
        },
        glium::glutin::Event::MouseInput (_,_) => {
          //if in_bounds (globals, mouse_coordinates) {
            event_index += 1;
            stew.insert_fiat_event (time, DeterministicRandomId::new (& event_index), AddInk {
              coordinates: [mouse_coordinates [0], mouse_coordinates [1]],
              amount: ((input_derivative+1)%2)*(GENERIC_INK_AMOUNT << input_magnitude_shift)*input_signum,
              accumulation: input_derivative*(GENERIC_INK_AMOUNT << input_magnitude_shift)*input_signum/(SECOND as Amount),
            }).unwrap();
          //}
        },
        glium::glutin::Event::KeyboardInput (state,_,Some(code)) => {
          if state == glium::glutin::ElementState::Pressed {
            if code == glium::glutin::VirtualKeyCode::Space {
              display_state = (display_state + 1) % 3;
            }
            if code == glium::glutin::VirtualKeyCode::D {
              input_derivative = (input_derivative + 1) % 2;
            }
            if code == glium::glutin::VirtualKeyCode::A {
              unrestricted_speed = !unrestricted_speed;
            }
            if code == glium::glutin::VirtualKeyCode::Grave { input_signum *= -1; }
            if code == glium::glutin::VirtualKeyCode::Key1 { input_magnitude_shift = 0; }
            if code == glium::glutin::VirtualKeyCode::Key2 { input_magnitude_shift = 1; }
            if code == glium::glutin::VirtualKeyCode::Key3 { input_magnitude_shift = 2; }
            if code == glium::glutin::VirtualKeyCode::Key4 { input_magnitude_shift = 3; }
            if code == glium::glutin::VirtualKeyCode::Key5 { input_magnitude_shift = 4; }
            if code == glium::glutin::VirtualKeyCode::Key6 { input_magnitude_shift = 5; }
            if code == glium::glutin::VirtualKeyCode::Key7 { input_magnitude_shift = 6; }
            if code == glium::glutin::VirtualKeyCode::Key8 { input_magnitude_shift = 7; }
            if code == glium::glutin::VirtualKeyCode::Key9 { input_magnitude_shift = 8; }
            
          }
        }
        _ => (),
      }
    }
    while let Some ((x,y)) = emscripten_compatibility::pop_click() {
      // TODO duplicate code
      mouse_coordinates [0] = ((x-0.5)*accessor.globals().size [0] as f64) as Distance;
      mouse_coordinates [1] = ((0.5-y)*accessor.globals().size [0] as f64) as Distance;
      //if in_bounds (globals, mouse_coordinates) {
        event_index += 1;
        stew.insert_fiat_event (time, DeterministicRandomId::new (& event_index), AddInk {
            coordinates: [mouse_coordinates [0], mouse_coordinates [1]],
            amount: ((input_derivative+1)%2)*(GENERIC_INK_AMOUNT*8 << input_magnitude_shift)*input_signum,
            accumulation: input_derivative*(GENERIC_INK_AMOUNT*8 << input_magnitude_shift)*input_signum/(SECOND as Amount),
        }).unwrap();
      /*}
      else {
        display_state = (display_state + 1) % 3;
      }*/
    }

    let mut target = display.draw();
    target.clear_color(1.0, 1.0, 1.0, 1.0);
    let mut vertices = Vec::<Vertex>::new();
    
    let mut to_draw = vec![accessor.globals().root.clone()];
    while let Some(handle) = to_draw.pop() {
      let guard = query_ref(&accessor, &handle.varying);
      match *guard {
        tree_continuum::NodeVarying::Branch (ref b) => iterate_children (&b.children, | _coordinates, child | to_draw.push(child.clone())),
        tree_continuum::NodeVarying::Leaf (ref leaf) => {
          let size_factor = (accessor.globals().size [0]/2) as f32;
          let density_factor = (GENERIC_DENSITY) as f32;
          let (my_current_ink, slope) = match display_state {
            0 => (
               density_at (&accessor, &handle, *accessor.now()) as f32 / density_factor,
                 [leaf.data.slope[0] as f32*size_factor/density_factor, leaf.data.slope[1] as f32*size_factor/density_factor]
            ),
            1 => (
               ((leaf.data.accumulation_rate * (SECOND as Amount) * 100 / (handle.width*handle.width) as Amount) as f32) / density_factor,
               [0.0,0.0]
            ),
            _ => ((accessor.now() - leaf.data.last_change) as f32 / SECOND as f32, [0.0,0.0]),
          };
          let center = handle.center;
          let offset = handle.width/2;
          
          let vertex = |x,y| Vertex {
            direction: [(offset*x) as f32/size_factor, (offset*y) as f32/size_factor],
            center: [center [0] as f32/size_factor, center [1] as f32/size_factor],
            slope: slope,
            density: my_current_ink,
          };
          vertices.extend(&[
            vertex(-1,-1),vertex( 1,-1),vertex( 1, 1),
            vertex(-1,-1),vertex( 1, 1),vertex(-1, 1)
          ]);
        },
      }
    }
    
    target.draw(&glium::VertexBuffer::new(&display, &vertices)
                .expect("failed to generate glium Vertex buffer"),
              &indices,
              &program,
              &glium::uniforms::EmptyUniforms,
              &parameters)
        .expect("failed target.draw");

    target.finish().expect("failed to finish drawing");
     
    if unrestricted_speed {
      while frame_begin.elapsed() < Duration::from_millis (10) {
        for _ in 0..8 {stew.step();}
        previous_time = stew.updated_until_before().expect("oops, the unrestricted speed system can't handle successfully simulating to the end of time in finite steps");
      }
    }
    /*while frame_begin.elapsed() < Duration::from_millis (10) && stew.updated_until_before().map_or (false, | limitation | limitation < time + SECOND) {
        for _ in 0..8 {stew.step();}
    }*/
    false
  };
  
  emscripten_compatibility::main_loop(frame);
}
