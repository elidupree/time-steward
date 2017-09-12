macro_rules! printlnerr(
    ($($arg:tt)*) => { {use std::io::Write;
        let r = writeln!(&mut ::std::io::stderr(), $($arg)*);
        r.expect("failed printing to stderr");
    } }
);

use std::fmt::Debug;
use serde::{Serialize};
use serde::de::DeserializeOwned;


pub trait TreeContinuumPhysics: Clone + Eq + Serialize + DeserializeOwned + Debug + PersistentlyIdentifiedType + 'static {
  const DIMENSIONS: usize;
  type Steward: TimeSteward;
  type NodeVarying: QueryResult;
  type BoundaryVarying: QueryResult;
}


use std::cmp::{min, max};
use std::collections::HashSet;

use array_ext::*;

use time_steward::{DeterministicRandomId};
use time_steward::{PersistentTypeId, PersistentlyIdentifiedType, ListedType, DataHandleTrait, DataTimelineCellTrait, QueryResult};
use time_steward::stewards::{simple_full as steward_module};
use steward_module::{TimeSteward, ConstructibleTimeSteward, IncrementalTimeSteward, Event, DataHandle, DataTimelineCell, EventHandle, Accessor, EventAccessor, FutureCleanupAccessor, simple_timeline};
use simple_timeline::{SimpleTimeline, tracking_query, tracking_query_ref, set, destroy, just_destroyed};

type Distance = i64;

const DIMENSIONS: usize = 2; // $DIMENSIONS;

#[derive (Clone, PartialEq, Eq, Serialize, Deserialize, Debug)]
enum FaceBoundaries<Physics: TreeContinuumPhysics> {
  WorldEdge,
  #[serde(deserialize_with = "::serde::Deserialize::deserialize")]
  SingleBoundary (BoundaryHandle<Physics>),
  #[serde(deserialize_with = "::serde::Deserialize::deserialize")]
  SplitBoundary (SplitBoundary<Physics>),
}
type SplitBoundary<Physics> = [BoundaryHandle<Physics>; 1<<(DIMENSIONS-1)];

#[derive (Clone, PartialEq, Eq, Serialize, Deserialize, Debug)]
pub struct NodeData<Physics: TreeContinuumPhysics> {
  width: Distance,
  center: [Distance ; DIMENSIONS],
  // Hacky workaround for https://github.com/rust-lang/rust/issues/41617 (see https://github.com/serde-rs/serde/issues/943)
  #[serde(deserialize_with = "::serde::Deserialize::deserialize")]
  parent: Option <NodeHandle<Physics>>,
  #[serde(deserialize_with = "::serde::Deserialize::deserialize")]
  varying: DataTimelineCell <SimpleTimeline <NodeVarying<Physics>, Physics::Steward>>,
}
#[derive (Clone, PartialEq, Eq, Serialize, Deserialize, Debug)]
pub enum NodeVarying<Physics: TreeContinuumPhysics> {
  #[serde(deserialize_with = "::serde::Deserialize::deserialize")]
  Branch (BranchVarying <Physics>),
  #[serde(deserialize_with = "::serde::Deserialize::deserialize")]
  Leaf (LeafVarying <Physics>),
}
#[derive (Clone, PartialEq, Eq, Serialize, Deserialize, Debug)]
pub struct BranchVarying<Physics: TreeContinuumPhysics> {
  #[serde(deserialize_with = "::serde::Deserialize::deserialize")]
  children: [NodeHandle<Physics>; 1<<DIMENSIONS],
}
#[derive (Clone, PartialEq, Eq, Serialize, Deserialize, Debug)]
pub struct LeafVarying<Physics: TreeContinuumPhysics> {
  #[serde(deserialize_with = "::serde::Deserialize::deserialize")]
  boundaries: NodeBoundaries<Physics>,
  #[serde(deserialize_with = "::serde::Deserialize::deserialize")]
  data: Physics::NodeVarying,
}
pub type NodeHandle<Physics> = DataHandle <NodeData<Physics>>;
impl<Physics: TreeContinuumPhysics> PersistentlyIdentifiedType for NodeData<Physics> {
  const ID: PersistentTypeId = PersistentTypeId(Physics::ID.0 ^ 0x0d838bdd804f48d7);
}
pub type NodeFaces<Face> = [[Face; 2]; DIMENSIONS];
type NodeBoundaries<Physics> = NodeFaces<FaceBoundaries<Physics>>;

#[derive (Clone, PartialEq, Eq, Serialize, Deserialize, Debug)]
pub struct BoundaryData<Physics: TreeContinuumPhysics> {
  length: Distance,
  center: [Distance ; 2],
  #[serde(deserialize_with = "::serde::Deserialize::deserialize")]
  nodes: [NodeHandle<Physics>; 2],
  #[serde(deserialize_with = "::serde::Deserialize::deserialize")]
  varying: DataTimelineCell <SimpleTimeline <BoundaryVarying<Physics>, Physics::Steward>>,
}
#[derive (Clone, PartialEq, Eq, Serialize, Deserialize, Debug)]
pub struct BoundaryVarying<Physics: TreeContinuumPhysics> {
  #[serde(deserialize_with = "::serde::Deserialize::deserialize")]
  data: Physics::BoundaryVarying,
}
pub type BoundaryHandle<Physics> = DataHandle <BoundaryData<Physics>>;
impl<Physics: TreeContinuumPhysics> PersistentlyIdentifiedType for BoundaryData<Physics> {
  const ID: PersistentTypeId = PersistentTypeId(Physics::ID.0 ^ 0x4913f629aef09374);
}


fn child_by_coordinates <Child> (children: & [Child; 1 << DIMENSIONS], coordinates: [usize; DIMENSIONS])-> & Child {
  let mut index = 0;
  for dimension in 0..DIMENSIONS {
    if coordinates [index] == 1 {index += 1 << dimension;}
  }
  & children [index]
}
fn children_from_fn <Child, F: FnMut ([usize; DIMENSIONS])->Child> (mut initializer: F)->[Child; 1 << DIMENSIONS] {
  Array::from_fn (| index | {
    let coordinates = Array::from_fn (| dimension | {
      if (index & (1 << dimension)) != 0 {1} else {0}
    });
    initializer (coordinates)
  })
}
fn iterate_children <Child, F: FnMut ([usize; DIMENSIONS], &Child)> (children: & [Child; 1 << DIMENSIONS], mut callback: F) {
  for (index, child) in children.iter().enumerate() {
    let coordinates = Array::from_fn (| dimension | {
      if (index & (1 << dimension)) != 0 {1} else {0}
    });
    callback (coordinates, child);
  }
}

pub fn face_by_dimension_and_direction<Face> (faces: &NodeFaces<Face>, dimension: usize, direction: usize)->&Face {
  & faces [dimension] [direction]
}
fn faces_from_fn<Face, F: FnMut(usize, usize)->Face> (mut initializer: F)->NodeFaces<Face> {
  Array::from_fn (| dimension | {
    Array::from_fn (| direction | {
      initializer (dimension, direction)
    })
  })
}
fn iterate_faces<Face, F: FnMut(usize, usize, &Face)> (faces: &NodeFaces<Face>, mut callback: F) {
  for (dimension, subset) in faces.iter().enumerate() {
    for (direction, face) in subset.iter().enumerate() {
      callback (dimension, direction, face)
    }
  }
}


fn face_boundary_component_by_coordinates_and_dimension<Physics: TreeContinuumPhysics> (boundaries: &FaceBoundaries<Physics>, coordinates: [usize; DIMENSIONS], dimension: usize)->Option<&BoundaryHandle <Physics>> {
  match boundaries {
    &FaceBoundaries::WorldEdge => None,
    &FaceBoundaries::SingleBoundary (ref handle) => Some(handle),
    &FaceBoundaries::SplitBoundary (ref array) => {
      Some(split_boundary_component_from_coordinates(array, dimension, coordinates))
    }
  }
}

fn iterate_face_boundaries <Physics: TreeContinuumPhysics, F: FnMut(& BoundaryHandle <Physics>)> (boundaries: &FaceBoundaries<Physics>, mut callback: F) {
  match boundaries {
    &FaceBoundaries::WorldEdge => (),
    &FaceBoundaries::SingleBoundary (ref handle) => callback (handle),
    &FaceBoundaries::SplitBoundary (ref array) => {
      for handle in array.iter() {
        callback (handle) ;
      }
    }
  }
}


fn boundary_by_coordinates_and_dimension<Physics: TreeContinuumPhysics> (boundaries: &NodeBoundaries<Physics>, coordinates: [usize; DIMENSIONS], dimension: usize)->Option<&BoundaryHandle <Physics>> {
  face_boundary_component_by_coordinates_and_dimension(face_by_dimension_and_direction(boundaries, dimension, coordinates [dimension]), coordinates, dimension)
}

fn iterate_boundaries <Physics: TreeContinuumPhysics, F: FnMut(& BoundaryHandle <Physics>)> (boundaries: &NodeBoundaries<Physics>, mut callback: F) {
  iterate_faces (boundaries, | dimension, direction, face | {
    iterate_face_boundaries (face, | boundary | callback (boundary));
  });
}


// Ignores the coordinate in the normal dimension
fn split_boundary_component_from_coordinates<Physics: TreeContinuumPhysics> (boundary: &SplitBoundary<Physics>, dimension: usize, coordinates: [usize; DIMENSIONS])->&BoundaryHandle<Physics> {
  let mut index = 0;
  for (dimension2_bit, dimension2) in (0..DIMENSIONS).filter (| dimension2 | *dimension2 != dimension).enumerate() {
    if coordinates [dimension2] == 1 {
      index += 1<<dimension2_bit;
    }
  }
  & boundary[index]
}
// leaves the coordinate in the normal dimension at 0
fn split_boundary_from_fn<Physics: TreeContinuumPhysics, F: FnMut([usize; DIMENSIONS])->BoundaryHandle<Physics>>(dimension: usize, mut initializer: F)->SplitBoundary<Physics> {
  Array::from_fn(| index | {
    let mut coordinates = [0; DIMENSIONS];
    for (dimension2_bit, dimension2) in (0..DIMENSIONS).filter (| dimension2 | *dimension2 != dimension).enumerate() {
      if (index & (1<<dimension2_bit)) != 0 {
        coordinates [dimension2] = 1;
      } else {
        coordinates [dimension2] = 0;
      }
    }
    initializer (coordinates)
  })
}
// leaves the coordinate in the normal dimension at 0
fn iterate_split_boundary<Physics: TreeContinuumPhysics, F: FnMut([usize; DIMENSIONS])>(dimension: usize, mut callback: F) {
  for index in 0..(1 << (DIMENSIONS-1)) {
    let mut coordinates = [0; DIMENSIONS];
    for (dimension2_bit, dimension2) in (0..DIMENSIONS).filter (| dimension2 | *dimension2 != dimension).enumerate() {
      if (index & (1<<dimension2_bit)) != 0 {
        coordinates [dimension2] = 1;
      } else {
        coordinates [dimension2] = 0;
      }
    }
    callback (coordinates);
  }
}




fn child_center_by_coordinates<Physics: TreeContinuumPhysics> (parent: &NodeHandle<Physics>, coordinates: [usize; DIMENSIONS])->[Distance ; DIMENSIONS] {
  let mut center = parent.center;
  for dimension in 0..DIMENSIONS {
    if coordinates [dimension] == 1 {
      center [dimension] += parent.width >> 2;
    }
    else {
      center [dimension] -= parent.width >> 2;
    }
  }
  center
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

pub struct NewChildInfo<Physics: TreeContinuumPhysics> {
  pub node: NodeHandle<Physics>,
  pub local_coordinates: [usize; DIMENSIONS],
  pub new_boundaries: NodeFaces<Option<BoundaryHandle<Physics>>>,
  pub old_boundaries: NodeFaces<Option<BoundaryHandle<Physics>>>,
}
pub struct NewBoundaryInfo<Physics: TreeContinuumPhysics> {
  pub boundary: BoundaryHandle<Physics>,
  pub old_boundary: Option<BoundaryHandle<Physics>>,
  pub normal_dimension: usize,
  // 0-1 in all dimensions other than our the normal;
  // 0-2 in the normal
  pub local_coordinates: [usize; DIMENSIONS],
}



pub fn split <Physics: TreeContinuumPhysics, A: EventAccessor <Steward = Physics::Steward>, N, B> (accessor: &A, splitting_node: &NodeHandle<Physics>, child_initializer: N, boundary_initializer: B) where N: Fn(NewChildInfo<Physics>)->Physics::NodeVarying, B: Fn(NewBoundaryInfo<Physics>)->Physics::BoundaryVarying {
  let splitting_leaf;
  {
    let mut splitting_varying_guard = tracking_query_ref (accessor, &splitting_node.varying);
    //printlnerr!("{:?}", (splitting_node.width, splitting_node.center, splitting_varying.children.len()));

    let mut old_leaf = match *splitting_varying_guard {
      NodeVarying::Branch (_) => panic!(),
      NodeVarying::Leaf (l) => l,
    };
    
    for dimension in 0..DIMENSIONS {
      for direction in 0..2 {
        if let &FaceBoundaries::SingleBoundary (ref handle) = face_by_dimension_and_direction (&old_leaf.boundaries, dimension, direction) {
          let neighbor = handle.nodes [direction];
          if neighbor.width > splitting_node.width {
            split (accessor, neighbor);
            splitting_varying_guard = tracking_query_ref (accessor, &splitting_node.varying);
            old_leaf = match *splitting_varying_guard {
              NodeVarying::Branch (_) => panic!(),
              NodeVarying::Leaf (l) => l,
            };
          }
        }
      }
    };
    
    splitting_leaf = old_leaf.clone();
  }
  let new_branch = BranchVarying {
    children: children_from_fn(| coordinates | {
      let center = child_center_by_coordinates(splitting_node, coordinates);
      let new_child = accessor.new_handle (NodeData {
        width: splitting_node.width >> 1,
        center: center,
        parent: Some (splitting_node.clone()),
        varying: DataTimelineCell::new (SimpleTimeline::new ()),
      });
      new_child
    }),
  };
  
  let new_exterior_boundaries: NodeFaces<Option<SplitBoundary<Physics>>> = Array::from_fn (| dimension | {
    Array::from_fn (| direction | {
      let old_face = face_by_dimension_and_direction (&splitting_leaf.boundaries, dimension, direction);
      match old_face {
        &FaceBoundaries::WorldEdge => None,
        _ => {
          Some(split_boundary_from_fn (dimension, | mut coordinates | {
            coordinates [dimension] = direction;
            let child = child_by_coordinates (& new_branch.children, coordinates);
            let old_boundary = face_boundary_component_by_coordinates_and_dimension(old_face, coordinates, dimension).unwrap();
            let mut boundary_center = child.center;
            let boundary_nodes;
            if coordinates [dimension] == 1 {
              boundary_center [dimension] += splitting_node.width >> 2;
              boundary_nodes = [child.clone(), old_boundary.nodes[1].clone()];
            }
            else {
              boundary_center [dimension] -= splitting_node.width >> 2;
              boundary_nodes = [old_boundary.nodes[0].clone(), child.clone()];
            }
            let new_boundary = accessor.new_handle (BoundaryData {
              length: splitting_node.width >> 1,
              center: boundary_center,
              nodes: boundary_nodes,
              varying: DataTimelineCell::new (SimpleTimeline::new ()),
            });
            let mut local_coordinates = coordinates;
            local_coordinates [dimension] <<= 1;
            set (accessor, &new_boundary.varying, BoundaryVarying {
              data: boundary_initializer (NewBoundaryInfo {
                boundary: new_boundary.clone(),
                old_boundary: Some(old_boundary.clone()),
                normal_dimension: dimension,
                local_coordinates: coordinates,
              }),
            });
            new_boundary
          }))
        }
      }
    })
  });
  
  let new_interior_boundaries: [SplitBoundary<Physics>; DIMENSIONS] = Array::from_fn (| dimension | {
    split_boundary_from_fn (dimension, | coordinates | {
      let mut second_coordinates = coordinates; second_coordinates [dimension] = 1;
      let children = [
        child_by_coordinates (& new_branch.children, coordinates),
        child_by_coordinates (& new_branch.children, second_coordinates),
      ];
      let mut boundary_center = children [0].center;
      boundary_center [dimension] = splitting_node.center [dimension];
      let boundary_nodes = [children [0].clone(), children [1].clone()];
      let new_boundary = accessor.new_handle (BoundaryData {
        length: splitting_node.width >> 1,
        center: boundary_center,
        nodes: boundary_nodes,
        varying: DataTimelineCell::new (SimpleTimeline::new ()),
      });
      let mut local_coordinates = coordinates;
      local_coordinates [dimension] = 1;
      set (accessor, &new_boundary.varying, BoundaryVarying {
        data: boundary_initializer (NewBoundaryInfo {
          boundary: new_boundary.clone(),
          old_boundary: None,
          normal_dimension: dimension,
          local_coordinates: coordinates,
        }),
      });
      new_boundary
    })
  });
  
  iterate_children (& new_branch.children, | coordinates, child | {
    let old_boundaries = faces_from_fn (| dimension, direction | {
        if coordinates [dimension] == direction {
          boundary_by_coordinates_and_dimension (& splitting_leaf.boundaries, coordinates, dimension).cloned()
        }
        else {
          None
        }
      });
    let new_boundaries = faces_from_fn (| dimension, direction | {
        if coordinates [dimension] == direction {
          match face_by_dimension_and_direction (& new_exterior_boundaries, dimension, direction) {
            &None => None,
            &Some(ref split) => Some(split_boundary_component_from_coordinates (split, dimension, coordinates).clone()),
          }
        }
        else {
          Some(split_boundary_component_from_coordinates (&new_interior_boundaries [dimension], dimension, coordinates).clone())
        }
      });
    set (accessor, &child.varying, NodeVarying::Leaf(LeafVarying {
      boundaries: faces_from_fn (| dimension, direction | { match face_by_dimension_and_direction (& new_boundaries, dimension, direction).clone() {
        None => FaceBoundaries::WorldEdge,
        Some(boundary) => FaceBoundaries::SingleBoundary(boundary),
      }}),
      data: child_initializer (NewChildInfo {
        node: child.clone(),
        old_boundaries: old_boundaries,
        new_boundaries: new_boundaries,
        local_coordinates: coordinates,
      }),
    }));
  });
  
  set (accessor, &splitting_node.varying, NodeVarying::Branch(new_branch));
  
}

/*
pub fn merge <A: EventAccessor <Steward = Steward >> (accessor: &A, node: &NodeHandle) {
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
*/