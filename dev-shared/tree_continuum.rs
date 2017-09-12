macro_rules! printlnerr(
    ($($arg:tt)*) => { {use std::io::Write;
        let r = writeln!(&mut ::std::io::stderr(), $($arg)*);
        r.expect("failed printing to stderr");
    } }
);

use std::fmt::Debug;
use serde::{Serialize};
use serde::de::DeserializeOwned;


trait TreeContinuumPhysics: Clone + Eq + Serialize + DeserializeOwned + Debug + PersistentlyIdentifiedType + 'static {
  const DIMENSIONS: usize;
  type Steward: TimeSteward;
  type NodeVarying: QueryResult;
  type BoundaryVarying: QueryResult;
}


use std::cmp::{min, max};
use std::collections::HashSet;

use time_steward::{DeterministicRandomId};
use time_steward::{PersistentTypeId, PersistentlyIdentifiedType, ListedType, DataHandleTrait, DataTimelineCellTrait, QueryResult};
use time_steward::stewards::{simple_full as steward_module};
use steward_module::{TimeSteward, ConstructibleTimeSteward, IncrementalTimeSteward, Event, DataHandle, DataTimelineCell, EventHandle, Accessor, EventAccessor, FutureCleanupAccessor, simple_timeline};
use simple_timeline::{SimpleTimeline, query, set, destroy, just_destroyed};

type Distance = i64;

const DIMENSIONS: usize = 2; // $DIMENSIONS;

#[derive (Clone, PartialEq, Eq, Serialize, Deserialize, Debug)]
enum Face<Physics: TreeContinuumPhysics> {
  WorldEdge,
  #[serde(deserialize_with = "::serde::Deserialize::deserialize")]
  SingleBoundary (BoundaryHandle<Physics>),
  #[serde(deserialize_with = "::serde::Deserialize::deserialize")]
  SplitBoundary ([BoundaryHandle<Physics>; 1<<(DIMENSIONS-1)]),
}

#[derive (Clone, PartialEq, Eq, Serialize, Deserialize, Debug)]
struct NodeData<Physics: TreeContinuumPhysics> {
  width: Distance,
  center: [Distance ; DIMENSIONS],
  // Hacky workaround for https://github.com/rust-lang/rust/issues/41617 (see https://github.com/serde-rs/serde/issues/943)
  #[serde(deserialize_with = "::serde::Deserialize::deserialize")]
  parent: Option <NodeHandle<Physics>>,
  #[serde(deserialize_with = "::serde::Deserialize::deserialize")]
  varying: DataTimelineCell <SimpleTimeline <NodeVarying<Physics>, Physics::Steward>>,
}
#[derive (Clone, PartialEq, Eq, Serialize, Deserialize, Debug)]
struct NodeVarying<Physics: TreeContinuumPhysics> {
  #[serde(deserialize_with = "::serde::Deserialize::deserialize")]
  children: Vec<NodeHandle<Physics>>,
  #[serde(deserialize_with = "::serde::Deserialize::deserialize")]
  boundaries: [[Face<Physics>; 2]; DIMENSIONS],
  #[serde(deserialize_with = "::serde::Deserialize::deserialize")]
  data: Physics::NodeVarying,
}
type NodeHandle<Physics> = DataHandle <NodeData<Physics>>;
impl<Physics: TreeContinuumPhysics> PersistentlyIdentifiedType for NodeData<Physics> {
  const ID: PersistentTypeId = PersistentTypeId(Physics::ID.0 ^ 0x0d838bdd804f48d7);
}

#[derive (Clone, PartialEq, Eq, Serialize, Deserialize, Debug)]
struct BoundaryData<Physics: TreeContinuumPhysics> {
  length: Distance,
  center: [Distance ; 2],
  #[serde(deserialize_with = "::serde::Deserialize::deserialize")]
  nodes: [NodeHandle<Physics>; 2],
  #[serde(deserialize_with = "::serde::Deserialize::deserialize")]
  varying: DataTimelineCell <SimpleTimeline <BoundaryVarying<Physics>, Physics::Steward>>,
}
#[derive (Clone, PartialEq, Eq, Serialize, Deserialize, Debug)]
struct BoundaryVarying<Physics: TreeContinuumPhysics> {
  #[serde(deserialize_with = "::serde::Deserialize::deserialize")]
  data: Physics::BoundaryVarying,
}
type BoundaryHandle<Physics> = DataHandle <BoundaryData<Physics>>;
impl<Physics: TreeContinuumPhysics> PersistentlyIdentifiedType for BoundaryData<Physics> {
  const ID: PersistentTypeId = PersistentTypeId(Physics::ID.0 ^ 0x4913f629aef09374);
}

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

/*
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
}*/

struct SubNodeInfo<Physics: TreeContinuumPhysics> {
  node: NodeHandle<Physics>,
  local_coordinates: [usize; DIMENSIONS],
  old_boundaries: [usize; DIMENSIONS*2],
}
struct SubBoundaryInfo<Physics: TreeContinuumPhysics> {
  boundary: BoundaryHandle<Physics>,
  old_boundary: Option<BoundaryHandle<Physics>>,
  normal_dimension: usize,
  // 0-1 in all dimensions other than our the normal;
  // 0-2 in the normal
  local_coordinates: [usize; DIMENSIONS],
  
}



fn split <Physics: TreeContinuumPhysics, A: EventAccessor <Steward = Physics::Steward>, N, B> (accessor: &A, node: &NodeHandle, node_initializer: N, boundary_initializer: B) where N: Fn(SubNodeInfo<Physics>)->Physics::NodeVarying, B: Fn(SubBoundaryInfo<Physics>)->Physics::BoundaryVarying {
  let mut varying = tracking_query (accessor, &node.varying);
  //printlnerr!("{:?}", (node.width, node.center, varying.children.len()));
  //if !varying.children.is_empty() {
    //printlnerr!("{:?}", (node.width, node.center, varying.children.len()));
  //}
  assert!(varying.children.is_empty());

  for index in 0..(1 << DIMENSIONS) {
    let mut center = node.center;
    let mut coordinates = [0; DIMENSIONS];
    for dimension in 0..DIMENSIONS {
      if index & (1 << dimension) {
        coordinates [dimension] = 1;
        center [dimension] += node.width >> 2;
      }
      else {
        center [dimension] -= node.width >> 2;
      }
    }
    let new_child = accessor.new_handle (NodeData {
      width: node.width >> 1,
      center: center,
      parent: Some (node.clone()),
      varying: DataTimelineCell::new (SimpleTimeline::new ()),
    });
    for dimension in 0..DIMENSIONS {
      if let Some(old_boundary) = boundary_by_coordinates_and_dimension (varying.boundaries, coordinates, dimension) {
        let mut boundary_center = center;
        if coordinates [dimension] == 1 {
          boundary_center [dimension] += node.width >> 2;
        }
        else {
          boundary_center [dimension] -= node.width >> 2;
        }
        let new_boundary = accessor.new_handle (BoundaryData {
          length: node.width >> 1,
          center: boundary_center,
          nodes: if direction == 0 { [neighbor.clone(), child.clone()] } else { [child.clone(), neighbor.clone()] },
          varying: DataTimelineCell::new (SimpleTimeline::new ()),
        });
        let mut local_coordinates = coordinates;
        local_coordinates [dimension] <<= 1;
        set (accessor, new_boundary.varying, boundary_initializer (SubBoundaryInfo {
          boundary: new_boundary,
          old_boundary: Some(old_boundary),
          normal_dimension: dimension,
          local_coordinates: coordinates,
        }));
      }
      if coordinates [dimension] == 1 {
        let mut boundary_center = center;
        boundary_center [dimension] = node.center [dimension];
        let new_boundary = accessor.new_handle (BoundaryData {
          length: node.width >> 1,
          center: boundary_center,
          nodes: if direction == 0 { [neighbor.clone(), child.clone()] } else { [child.clone(), neighbor.clone()] },
          varying: DataTimelineCell::new (SimpleTimeline::new ()),
        });
        let mut local_coordinates = coordinates;
        local_coordinates [dimension] = 1;
        set (accessor, new_boundary.varying, boundary_initializer (SubBoundaryInfo {
          boundary: new_boundary,
          old_boundary: Some(old_boundary),
          normal_dimension: dimension,
          local_coordinates: coordinates,
        }));
      }
    }
  }
  
  varying.boundaries = ;
  
  set (accessor, &node.varying, varying);
  
  for boundary in new_boundaries {
    update_transfer_change_prediction (accessor, &boundary);
  }
  
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
