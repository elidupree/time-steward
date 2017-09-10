#![feature (i128_type)]

extern crate serde;
#[macro_use]
extern crate serde_derive;

extern crate time_steward;

// Imports for the UI
#[macro_use]
extern crate glium;
#[path = "../dev-shared/emscripten_compatibility.rs"] mod emscripten_compatibility;
pub use emscripten_compatibility::canvas_click;

macro_rules! printlnerr(
    ($($arg:tt)*) => { {use std::io::Write;
        let r = writeln!(&mut ::std::io::stderr(), $($arg)*);
        r.expect("failed printing to stderr");
    } }
);


use std::cmp::{min, max};
use std::collections::HashSet;

use time_steward::{DeterministicRandomId};
use time_steward::{PersistentTypeId, PersistentlyIdentifiedType, ListedType, DataHandleTrait, DataTimelineCellTrait, Basics as BasicsTrait};
use time_steward::stewards::{simple_full as steward_module};
use steward_module::{TimeSteward, ConstructibleTimeSteward, IncrementalTimeSteward, Event, DataHandle, DataTimelineCell, EventHandle, Accessor, EventAccessor, FutureCleanupAccessor, simple_timeline};
use simple_timeline::{SimpleTimeline, query, set, destroy, just_destroyed};


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
struct NodeData {
  width: Distance,
  center: [Distance ; 2],
  parent: Option <NodeHandle>,
  varying: DataTimelineCell <SimpleTimeline <NodeVarying, Steward>>,
}
#[derive (Clone, PartialEq, Eq, Serialize, Deserialize, Debug)]
struct NodeVarying {
  last_change: Time,
  ink_at_last_change: Amount,
  children: Vec<NodeHandle>,
  boundaries: [[Vec<BoundaryHandle>;2];2],
  
  accumulation_rate: Amount,
  slope: [Amount; 2],
}
type NodeHandle = DataHandle <NodeData>;
impl PersistentlyIdentifiedType for NodeData {
  const ID: PersistentTypeId = PersistentTypeId(0x734528f6aefdc1b9);
}
//serialization_cheat!([][NodeVarying]);

#[derive (Clone, PartialEq, Eq, Serialize, Deserialize, Debug)]
struct BoundaryData {
  length: Distance,
  center: [Distance ; 2],
  nodes: [NodeHandle; 2],
  varying: DataTimelineCell <SimpleTimeline <BoundaryVarying, Steward>>,
}
#[derive (Clone, PartialEq, Eq, Serialize, Deserialize, Debug)]
struct BoundaryVarying {
  transfer_velocity: Amount,
  next_change: Option <EventHandle <Basics>>,
}
type BoundaryHandle = DataHandle <BoundaryData>;
impl PersistentlyIdentifiedType for BoundaryData {
  const ID: PersistentTypeId = PersistentTypeId(0x9d643214b58b24dc);
}
//serialization_cheat!([][BoundaryVarying]);

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
}
macro_rules! set_with {
  ($accessor: expr, $cell: expr, | $varying: ident | $actions: expr) => {
    {
      let mut $varying = query ($accessor, $cell);
      $actions;
      set ($accessor, $cell, $varying);
    }
  }
}
macro_rules! exists {
  ($accessor: expr, $cell: expr) => {
    $accessor.query ($cell, &GetVarying).is_some()
  }
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
  let mut varying = query (accessor, &node.varying);
  varying.slope = [0, 0];
  varying.accumulation_rate = 0;
  for dimension in 0..2 {
    for direction in 0..2 {
      let direction_signum = (direction as Amount*2)-1;
      for boundary in varying.boundaries [dimension] [direction].iter() {
        let boundary_varying = get!(accessor, & boundary.varying);
        let transfer_rate = boundary_varying.transfer_velocity*boundary.length as Amount;
        varying.accumulation_rate -= transfer_rate*direction_signum;
        varying.slope [dimension] -= transfer_rate; // divided by
        // (2*node.width*VELOCITY_PER_SLOPE), but we do that later as an optimization
      }
    }
    varying.slope [dimension] /= (2*node.width as Amount)*VELOCITY_PER_SLOPE;
  }
  set (accessor, &node.varying, varying);
}

fn update_node <A: EventAccessor <Steward = Steward >> (accessor: &A, node: &NodeHandle) {
  set_with!(accessor, &node.varying, | varying | {
    varying.ink_at_last_change = ink_at (accessor, node, *accessor.now());
    varying.last_change = *accessor.now();
  });
}

fn ink_at <A: Accessor <Steward = Steward >> (accessor: &A, node: &NodeHandle, time: Time)->Amount {
  let varying = get!(accessor, &node.varying);
  varying.ink_at_last_change + varying.accumulation_rate*((time - varying.last_change) as Amount)
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
    get!(accessor, &nodes[0].varying).accumulation_rate,
    get!(accessor, &nodes[1].varying).accumulation_rate,
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
    transfer_velocity_to_ideal_density_difference([&nodes[0], &nodes[1]], boundary_varying.transfer_velocity - permissible_error),
    transfer_velocity_to_ideal_density_difference([&nodes[0], &nodes[1]], boundary_varying.transfer_velocity + permissible_error),
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
  
  boundary_varying.next_change = time.map (|time| {
    accessor.create_prediction (
      time,
      DeterministicRandomId::new (&(accessor.id(), boundary.center)),
      TransferChange {boundary: boundary.clone()}
    )
  });
  
  });
}


fn audit <A: EventAccessor <Steward = Steward >> (accessor: &A) {
  audit_node (accessor, &accessor.globals().root);
}
fn audit_node <A: EventAccessor <Steward = Steward >> (accessor: &A, node: &NodeHandle) {
  let varying = get!(accessor, &node.varying);
  for dimension in 0..2 {
    for direction in 0..2 {
      for boundary in varying.boundaries [dimension] [direction].iter() {
        assert!(&boundary.nodes[(direction+1)&1] == node);
        audit_boundary (accessor, boundary);
      }
    }
  }
  for child in varying.children.iter() {
    audit_node (accessor, child);
  }
}
fn audit_boundary <A: EventAccessor <Steward = Steward >> (accessor: &A, boundary: &BoundaryHandle) {
  get!(accessor, &boundary.varying);
  for direction in 0..2 {
    assert!(get!(accessor, &boundary.nodes [direction].varying).boundaries.iter().any(|whatever| whatever[(direction+1)&1].iter().any(| other_boundary| other_boundary == boundary)));
  }
}

fn maybe_split <A: EventAccessor <Steward = Steward >> (accessor: &A, node: &NodeHandle)->bool {
  if node.width <METER {return false;}
  let varying = get!(accessor, &node.varying);
  let mut averaged_ideal_velocities = [0; 2];
  for dimension in 0..2 {
    for direction in 0..2 {
      for boundary in varying.boundaries [dimension] [direction].iter() {
        let density_difference =
          density_at (accessor, &boundary.nodes[0], *accessor.now()) - 
          density_at (accessor, &boundary.nodes[1], *accessor.now());
        let ideal_velocity = density_difference_to_ideal_transfer_velocity ([&boundary.nodes[0], &boundary.nodes[0]], density_difference);
        let ideal_rate = ideal_velocity*boundary.length as Amount;
        averaged_ideal_velocities [dimension] -= ideal_rate;
      }
    }
    averaged_ideal_velocities [dimension] /= 2*node.width as Amount;
  }
  let mut close_enough = true;
  for dimension in 0..2 {
    for direction in 0..2 {
      for boundary in varying.boundaries [dimension] [direction].iter() {
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
      }
    }
  }
  
  if !close_enough {
    split (accessor, node);
  }
  !close_enough
}

fn split <A: EventAccessor <Steward = Steward >> (accessor: &A, node: &NodeHandle) {
  let mut varying = get!(accessor, &node.varying);
  //printlnerr!("{:?}", (node.width, node.center, varying.children.len()));
  //if !varying.children.is_empty() {
    //printlnerr!("{:?}", (node.width, node.center, varying.children.len()));
  //}
  assert!(varying.children.is_empty());
  let mut velocities = [[[0;2];2];2];
  let mut old_boundaries = [[[None,None],[None,None]],[[None,None],[None,None]]];
  let mut middle_velocities = [[0;2];2];
  for dimension in 0..2 {
    for direction in 0..2 {
      //let direction_signum = (direction*2)-1;
      let other_direction = (direction + 1) & 1;
      
      match varying.boundaries [dimension] [direction].len() {
        1 => {
          loop {
            {
              let boundary = &varying.boundaries [dimension] [direction] [0];
              let neighbor = &boundary.nodes [direction];
              if neighbor.width <= node.width {
                break;
              }
              split (accessor, &neighbor);
            }
            varying = get!(accessor, &node.varying);
          }
          let boundary = &varying.boundaries [dimension] [direction] [0];
          assert!(&boundary.nodes[other_direction] == node);
          velocities [dimension] [direction] = [
            get!(accessor, &boundary.varying).transfer_velocity,
            get!(accessor, &boundary.varying).transfer_velocity
          ];
          old_boundaries [dimension] [direction] = [
            Some(boundary.clone()),
            Some(boundary.clone()),
          ];
        },
        2 => {
          let boundaries = &varying.boundaries [dimension] [direction];
          assert!(&boundaries[0].nodes[other_direction] == node);
          assert!(&boundaries[1].nodes[other_direction] == node);
          velocities [dimension] [direction] = [
            get!(accessor, &boundaries [0].varying).transfer_velocity,
            get!(accessor, &boundaries [1].varying).transfer_velocity,
          ];
          old_boundaries [dimension] [direction] = [
            Some(boundaries [0].clone()),
            Some(boundaries [1].clone()),
          ];
        },
        0 => (),
        _ => unreachable!(),
      };
    }
    middle_velocities [dimension] = [
      (velocities [dimension] [0] [0] + velocities [dimension] [1] [0]) / 2,
      (velocities [dimension] [0] [1] + velocities [dimension] [1] [1]) / 2,
    ];
  }
  
  // TODO: more accurate computation of ideal middle velocities and ink amounts,
  // and conserve ink correctly
  let mut new_children = [[[None,None],[None,None]],[[None,None],[None,None]]];
  for x in 0..2 {
    let x_signum = (x as Distance*2)-1;
    for y in 0..2 {
      let y_signum = (y as Distance*2)-1;
      let center = [
        node.center[0] + (node.width >> 2)*x_signum,
        node.center[1] + (node.width >> 2)*y_signum,
      ];
      let new_child = accessor.new_handle (NodeData {
        width: node.width >> 1,
        center: center,
        parent: Some (node.clone()),
        varying: DataTimelineCell::new (SimpleTimeline::new ()),
      });
      set (accessor, &new_child.varying, NodeVarying {
        last_change: *accessor.now(),
        ink_at_last_change: varying.ink_at_last_change >> 2,
        children: Vec::new(),
        boundaries: [[Vec::new(), Vec::new()], [Vec::new(), Vec::new()]],
        accumulation_rate: 0,
        slope: [0; 2],
      });
      varying.children.push (new_child.clone());
      new_children [0][x][y] = Some(new_child.clone());
      new_children [1][y][x] = Some(new_child.clone());
    }
  }
  for dimension in 0..2 {
    for direction in 0..2 {
      let other_direction = (direction + 1) & 1;
      for which in 0..2 {
        if let Some(boundary) = old_boundaries [dimension] [direction] [which].as_ref() {
          let neighbor = &boundary.nodes [direction];
          assert!(neighbor.width <= node.width);
          set_with!(accessor, &neighbor.varying, | neighbor_varying | {
            for b in neighbor_varying.boundaries [dimension] [other_direction].iter() {
              assert!(&b.nodes[other_direction] == node);
            }
            neighbor_varying.boundaries [dimension] [other_direction].clear();
          });
        }
      }
    }
  }
  let mut new_boundaries = Vec::new();
  for dimension in 0..2 {
    let other_dimension = (dimension + 1) & 1;
    for direction in 0..2 {
      let other_direction = (direction + 1) & 1;
      let direction_signum = (direction as Distance*2)-1;
      for which in 0..2 {
        let which_signum = (which as Distance*2)-1;
        if let Some(boundary) = old_boundaries [dimension] [direction] [which].as_ref() {
          let child = new_children [dimension] [direction] [which].as_ref().unwrap();
          let neighbor = &boundary.nodes [direction];
          let mut center = node.center;
          center [dimension] += (node.width >> 2)*direction_signum;
          center [other_dimension] += (node.width >> 4)*which_signum;
          let new_boundary = accessor.new_handle (BoundaryData {
            length: node.width >> 1,
            center: center,
            nodes: if direction == 0 { [neighbor.clone(), child.clone()] } else { [child.clone(), neighbor.clone()] },
            varying: DataTimelineCell::new (SimpleTimeline::new ()),
          });
          set (accessor, &new_boundary.varying, BoundaryVarying {
            transfer_velocity: velocities [dimension] [direction] [which],
            next_change: None,
          });
          set_with!(accessor, &child.varying, | child_varying | {
            child_varying.boundaries [dimension] [direction].push(new_boundary.clone());
          });
          set_with!(accessor, &neighbor.varying, | neighbor_varying | {
            neighbor_varying.boundaries [dimension] [other_direction].push(new_boundary.clone());
          });
          new_boundaries.push (new_boundary);
        }
      }
    }
  }
  for dimension in 0..2 {
    let other_dimension = (dimension + 1) & 1;
    for which in 0..2 {
      let which_signum = (which as Distance*2)-1;
      let child0 = new_children [dimension] [0] [which].as_ref().unwrap();
      let child1 = new_children [dimension] [1] [which].as_ref().unwrap();
      let mut center = node.center;
      center [other_dimension] += (node.width >> 4)*which_signum;
      let new_boundary = accessor.new_handle (BoundaryData {
        length: node.width >> 1,
        center: center,
        nodes: [child0.clone(), child1.clone()],
        varying: DataTimelineCell::new (SimpleTimeline::new ()),
      });
      set (accessor, &new_boundary.varying, BoundaryVarying {
        transfer_velocity: middle_velocities [dimension] [which],
        next_change: None,
      });
      set_with!(accessor, &child0.varying, | child0_varying | {
        child0_varying.boundaries [dimension] [1].push(new_boundary.clone());
      });
      set_with!(accessor, &child1.varying, | child1_varying | {
        child1_varying.boundaries [dimension] [0].push(new_boundary.clone());
      });
      new_boundaries.push (new_boundary);
    }
  }
  for dimension in 0..2 {
    for direction in 0..2 {
      for which in 0..2 {
        if let Some(boundary) = old_boundaries [dimension] [direction] [which].as_ref() {
          if !just_destroyed (accessor, & boundary.varying) {
            let boundary_varying = query (accessor, &boundary.varying);
            destroy (accessor, &boundary.varying);
          }
        }
      }
    }
  }
  
  varying.boundaries = [[Vec::new(), Vec::new()], [Vec::new(), Vec::new()]];
  
  set (accessor, &node.varying, varying);
  
  for boundary in new_boundaries {
    update_transfer_change_prediction (accessor, &boundary);
  }
  
}


fn maybe_merge <A: EventAccessor <Steward = Steward >> (accessor: &A, node: &NodeHandle)->bool {
  if just_destroyed (accessor, &node.varying) { return false; }
  let varying = get!(accessor, &node.varying);
  if varying.children.is_empty() {
    for child in varying.children.iter() {
      if maybe_merge(accessor, child) { return true; }
    }
    return false;
  }
  let mut averaged_ideal_velocities = [0; 2];
  for dimension in 0..2 {
    for direction in 0..2 {
      for child in varying.children.iter() {
        let child_varying = get!(accessor, &child.varying);
        if child_varying.children.len() >0 {return false;}
        if child_varying.boundaries [dimension] [direction].len() >1 {return false;}
        for boundary in child_varying.boundaries [dimension] [direction].iter() {
          let density_difference =
            density_at (accessor, &boundary.nodes[0], *accessor.now()) - 
            density_at (accessor, &boundary.nodes[1], *accessor.now());
          let ideal_velocity = density_difference_to_ideal_transfer_velocity ([&boundary.nodes[0], &boundary.nodes[0]], density_difference);
          let ideal_rate = ideal_velocity*boundary.length as Amount;
          averaged_ideal_velocities [dimension] -= ideal_rate;
        }
      }
    }
    averaged_ideal_velocities [dimension] /= 4*node.width as Amount;
  }
  let mut close_enough = true;
  for dimension in 0..2 {
    for direction in 0..2 {
      for child in varying.children.iter() {
        let child_varying = get!(accessor, &child.varying);
        for boundary in child_varying.boundaries [dimension] [direction].iter() {
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
        }
      }
    }
  }
  
  if close_enough {
    merge (accessor, node);
  }
  close_enough
}

fn merge <A: EventAccessor <Steward = Steward >> (accessor: &A, node: &NodeHandle) {
  let mut varying = get!(accessor, &node.varying);
  assert!(!varying.children.is_empty());
  
  varying.last_change =*accessor.now();
  varying.ink_at_last_change = 0;
  let mut prior_boundaries = Vec::new();
  let mut discovered_prior_boundaries = HashSet::new();
  let mut prior_children = HashSet::new();
  let mut neighbors = Vec::new();
  let mut discovered_neighbors = HashSet::new();
  for child in varying.children.iter() {
    update_node(accessor, child);
    let child_varying = get!(accessor, &child.varying);
    assert!(child_varying.children.is_empty());
    assert!(child_varying.last_change == *accessor.now());
    varying.ink_at_last_change += child_varying.ink_at_last_change;
    for (dimension, whatever) in child_varying.boundaries.iter().enumerate() {for (direction, something) in whatever.iter().enumerate() {for other_boundary in something.iter() {
      if discovered_prior_boundaries.insert (other_boundary.clone()) {
        prior_boundaries.push ((dimension, direction, other_boundary.clone()));
      }
    }}}
    prior_children.insert (child.clone()) ;

    destroy (accessor, &child.varying);
  }
  
  let mut new_boundaries = Vec::new();
  for (dimension, direction, boundary) in prior_boundaries {
    let boundary_varying = get!(accessor, &boundary.varying);
    let neighbor = if !prior_children.contains (&boundary.nodes [1]) {
      Some (boundary.nodes [1].clone())
    } else if !prior_children.contains (&boundary.nodes [0]) {
      Some (boundary.nodes [0].clone())
    } else {None};
    if let Some(neighbor) = neighbor {
      if discovered_neighbors.insert (neighbor.clone()) {
        neighbors.push (neighbor.clone());
        let other_direction = (direction+1)&1;
        set_with!(accessor, &neighbor.varying, | neighbor_varying | {
          for b in neighbor_varying.boundaries [dimension] [other_direction].iter() {
            assert!(prior_children.contains(&b.nodes[other_direction]));
          }
          neighbor_varying.boundaries [dimension] [other_direction].clear();
        });
        let mut center = boundary.center;
        if neighbor.width == node.width {
          center = [
            (node.center[0] + neighbor.center [0]) >> 1,
            (node.center[1] + neighbor.center [1]) >> 1,
          ];
        }
        let nodes = if direction == 1 {[node.clone(), neighbor.clone()]}else{[neighbor.clone(), node.clone()]};
        let new_boundary = accessor.new_handle (BoundaryData {
          length: neighbor.width,
          center: center,
          nodes: nodes.clone(),
          varying: DataTimelineCell::new (SimpleTimeline::new ()),
        });
        set (accessor, &new_boundary.varying, BoundaryVarying {
          transfer_velocity: boundary_varying.transfer_velocity,
          next_change: None,
        });
        set_with!(accessor, &neighbor.varying, | neighbor_varying | {
          neighbor_varying.boundaries [dimension] [other_direction].push(new_boundary.clone());
        });
        varying.boundaries [dimension] [direction].push(new_boundary.clone());
        new_boundaries.push (new_boundary);
      }
    }
    destroy (accessor, &boundary.varying);
  }
  
  varying.children.clear();
  set (accessor, &node.varying, varying);
  update_inferred_node_properties (accessor, node);
  
  for boundary in new_boundaries {
    update_transfer_change_prediction (accessor, &boundary);
  }

  if let Some(parent) = node.parent.as_ref() {
    //audit(accessor);
    maybe_merge (accessor, parent);
    //audit(accessor);
  }
  
  for neighbor in neighbors {
    maybe_merge (accessor, &neighbor);
  }
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
      get!(accessor, &nodes[0].varying).accumulation_rate,
      get!(accessor, &nodes[1].varying).accumulation_rate,
    ];
    let difference_now = densities[0] - densities[1];
    let physics_ideal_velocity = density_difference_to_ideal_transfer_velocity ([&nodes[0], &nodes[1]], difference_now);
    set_with!(accessor, &self.boundary.varying, | boundary_varying | {
      // for the sake of this math, assume that the accumulation rates have already been updated based on the physics-wise ideal value
      accumulation_rates [0] -= (physics_ideal_velocity - boundary_varying.transfer_velocity)*self.boundary.length as Amount;
      accumulation_rates [1] += (physics_ideal_velocity - boundary_varying.transfer_velocity)*self.boundary.length as Amount;
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
      boundary_varying.transfer_velocity = physics_ideal_velocity + numerator.signum()+max_reasonable_velocity_skew/2;
    });
    for node in nodes.iter() {
      update_inferred_node_properties (accessor, node);
    }
    for node in nodes.iter() {
      for whatever in get!(accessor, &node.varying).boundaries.iter() {for something in whatever.iter() {for other_boundary in something.iter() {
        update_transfer_change_prediction (accessor, other_boundary);
      }}}
    }
    
    let mut split = false;
    for node in nodes.iter() {
      let varying = get!(accessor, &node.varying);
      if varying.children.is_empty() {
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
    update_node (accessor, &node);
    while node.width > METER {
      if get!(accessor, &node.varying).children.is_empty() {
        split (accessor, &node);
      }
      node = get!(accessor, &node.varying).children
        [if self.coordinates [0] > node.center [0] {2} else {0}+
         if self.coordinates [1] > node.center [1] {1} else {0}].clone();
    }
    set_with!(accessor, &node.varying, | varying | {
      varying.ink_at_last_change += self.amount;
      //node.fiat_accumulation_rate += self.accumulation;
    });
    
    for whatever in get!(accessor, &node.varying).boundaries.iter() {for something in whatever.iter() {for other_boundary in something.iter() {
      update_transfer_change_prediction (accessor, other_boundary);
    }}}
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
    set (accessor, &globals.root.varying, NodeVarying {
      last_change: 0,
      ink_at_last_change: 0,
      children: Vec::new(),
      boundaries: [[Vec::new(), Vec::new()], [Vec::new(), Vec::new()]],
      accumulation_rate: 0,
      slope: [0; 2],
    });
  }
  fn undo <Accessor: FutureCleanupAccessor <Steward = Self::Steward>> (&self, _accessor: &mut Accessor, _: ()) {
    unimplemented!()
  }
}

fn make_globals()-> Globals {
  Globals {
    size: [64*METER, 64*METER],
    root: DataHandle::new_for_globals (NodeData {
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
      let varying: NodeVarying = get!(&accessor, &handle.varying);
      if varying.children.len() > 0 {
        for child in varying.children.iter() {
          to_draw.push(child.clone());
        }
      }
      else {
        let size_factor = (accessor.globals().size [0]/2) as f32;
        let density_factor = (GENERIC_DENSITY) as f32;
        let (my_current_ink, slope) = match display_state {
          0 => (
             density_at (&accessor, &handle, *accessor.now()) as f32 / density_factor,
             [varying.slope[0] as f32*size_factor/density_factor, varying.slope[1] as f32*size_factor/density_factor]
          ),
          1 => (
             ((varying.accumulation_rate * (SECOND as Amount) * 100 / (handle.width*handle.width) as Amount) as f32) / density_factor,
             [0.0,0.0]
          ),
          _ => ((accessor.now() - varying.last_change) as f32 / SECOND as f32, [0.0,0.0]),
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
