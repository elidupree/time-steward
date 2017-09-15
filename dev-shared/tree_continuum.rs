macro_rules! printlnerr(
    ($($arg:tt)*) => { {use std::io::Write;
        let r = writeln!(&mut ::std::io::stderr(), $($arg)*);
        r.expect("failed printing to stderr");
    } }
);

use std::fmt::Debug;
use std::ops::Deref;
use std::mem;
use serde::{Serialize};
use serde::de::DeserializeOwned;


pub trait TreeContinuumPhysics: Clone + Eq + Serialize + DeserializeOwned + Debug + PersistentlyIdentifiedType + 'static {
  const DIMENSIONS: usize;
  type Steward: TimeSteward;
  type NodeVarying: QueryResult;
  type BoundaryVarying: QueryResult;
  
  //fn before_split <A: EventAccessor <Steward = Self::Steward>> (accessor: &A, splitting_node: & NodeHandle <Self>) {}
  fn initialize_split_child <A: EventAccessor <Steward = Self::Steward>> (accessor: &A, child: NewChildInfo<Self>)->Self::NodeVarying;
  fn initialize_split_boundary <A: EventAccessor <Steward = Self::Steward>> (accessor: &A, boundary: NewBoundaryInfo<Self>)->Self::BoundaryVarying;
  fn after_split <A: EventAccessor <Steward = Self::Steward>> (accessor: &A, split_node: & NodeHandle <Self>, new_boundaries: Vec<BoundaryHandle <Self>>) {}
  
  //fn before_merge <A: EventAccessor <Steward = Self::Steward>> (accessor: &A, merging_node: & NodeHandle <Self>) {}
  //fn initialize_merge_parent <A: EventAccessor <Steward = Self::Steward>> (accessor: &A)->Self::NodeVarying;
  fn initialize_merge_boundary <A: EventAccessor <Steward = Self::Steward>> (accessor: &A, boundary: MergeBoundaryInfo<Self>)->Self::BoundaryVarying;
  //fn after_merge <A: EventAccessor <Steward = Self::Steward>> (accessor: &A, merged_node: & NodeHandle <Self>, new_boundaries: Vec<BoundaryHandle <Self>>) {}
}


use std::cmp::{min, max};
use std::collections::HashSet;

use array_ext::*;

use time_steward::{DeterministicRandomId};
use time_steward::{PersistentTypeId, PersistentlyIdentifiedType, ListedType, DataHandleTrait, DataTimelineCellTrait, QueryResult};
use time_steward::stewards::{simple_full as steward_module};
use steward_module::{TimeSteward, ConstructibleTimeSteward, IncrementalTimeSteward, Event, DataHandle, DataTimelineCell, EventHandle, Accessor, EventAccessor, FutureCleanupAccessor, simple_timeline};
use simple_timeline::{SimpleTimeline, query, tracking_query, tracking_query_ref, set, destroy, just_destroyed};

type Distance = i64;

pub const DIMENSIONS: usize = 2; // $DIMENSIONS;

#[derive (Clone, PartialEq, Eq, Serialize, Deserialize, Debug)]
pub enum FaceBoundaries<Physics: TreeContinuumPhysics> {
  WorldEdge,
  #[serde(deserialize_with = "::serde::Deserialize::deserialize")]
  SingleBoundary (BoundaryHandle<Physics>),
  #[serde(deserialize_with = "::serde::Deserialize::deserialize")]
  SplitBoundary (SplitBoundary<Physics>),
}
pub type SplitBoundary<Physics> = [BoundaryHandle<Physics>; 1<<(DIMENSIONS-1)];

#[derive (Clone, PartialEq, Eq, Serialize, Deserialize, Debug)]
pub struct NodeData<Physics: TreeContinuumPhysics> {
  pub width: Distance,
  pub center: [Distance ; DIMENSIONS],
  // Hacky workaround for https://github.com/rust-lang/rust/issues/41617 (see https://github.com/serde-rs/serde/issues/943)
  #[serde(deserialize_with = "::serde::Deserialize::deserialize")]
  pub parent: Option <NodeHandle<Physics>>,
  #[serde(deserialize_with = "::serde::Deserialize::deserialize")]
  pub varying: DataTimelineCell <SimpleTimeline <NodeVarying<Physics>, Physics::Steward>>,
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
  pub children: [NodeHandle<Physics>; 1<<DIMENSIONS],
}
#[derive (Clone, PartialEq, Eq, Serialize, Deserialize, Debug)]
pub struct LeafVarying<Physics: TreeContinuumPhysics> {
  #[serde(deserialize_with = "::serde::Deserialize::deserialize")]
  pub boundaries: NodeBoundaries<Physics>,
  #[serde(deserialize_with = "::serde::Deserialize::deserialize")]
  pub data: Physics::NodeVarying,
}
pub type NodeHandle<Physics> = DataHandle <NodeData<Physics>>;
impl<Physics: TreeContinuumPhysics> PersistentlyIdentifiedType for NodeData<Physics> {
  const ID: PersistentTypeId = PersistentTypeId(Physics::ID.0 ^ 0x0d838bdd804f48d7);
}
pub type NodeFaces<Face> = [[Face; 2]; DIMENSIONS];
type NodeBoundaries<Physics> = NodeFaces<FaceBoundaries<Physics>>;

#[derive (Clone, PartialEq, Eq, Serialize, Deserialize, Debug)]
pub struct BoundaryData<Physics: TreeContinuumPhysics> {
  pub length: Distance,
  pub normal_dimension: usize,
  pub center: [Distance ; 2],
  #[serde(deserialize_with = "::serde::Deserialize::deserialize")]
  pub nodes: [NodeHandle<Physics>; 2],
  #[serde(deserialize_with = "::serde::Deserialize::deserialize")]
  pub varying: DataTimelineCell <SimpleTimeline <BoundaryVarying<Physics>, Physics::Steward>>,
}
#[derive (Clone, PartialEq, Eq, Serialize, Deserialize, Debug)]
pub struct BoundaryVarying<Physics: TreeContinuumPhysics> {
  #[serde(deserialize_with = "::serde::Deserialize::deserialize")]
  pub data: Physics::BoundaryVarying,
}
pub type BoundaryHandle<Physics> = DataHandle <BoundaryData<Physics>>;
impl<Physics: TreeContinuumPhysics> PersistentlyIdentifiedType for BoundaryData<Physics> {
  const ID: PersistentTypeId = PersistentTypeId(Physics::ID.0 ^ 0x4913f629aef09374);
}


pub fn child_by_coordinates <Child> (children: & [Child; 1 << DIMENSIONS], coordinates: [usize; DIMENSIONS])-> & Child {
  let mut index = 0;
  for dimension in 0..DIMENSIONS {
    if coordinates [dimension] == 1 {index += 1 << dimension;}
  }
  & children [index]
}
pub fn children_from_fn <Child, F: FnMut ([usize; DIMENSIONS])->Child> (mut initializer: F)->[Child; 1 << DIMENSIONS] {
  Array::from_fn (| index | {
    let coordinates = Array::from_fn (| dimension | {
      if (index & (1 << dimension)) != 0 {1} else {0}
    });
    initializer (coordinates)
  })
}
pub fn iterate_children <Child, F: FnMut ([usize; DIMENSIONS], &Child)> (children: & [Child; 1 << DIMENSIONS], mut callback: F) {
  for (index, child) in children.iter().enumerate() {
    let coordinates = Array::from_fn (| dimension | {
      if (index & (1 << dimension)) != 0 {1} else {0}
    });
    //printlnerr!("{:?}", (index, coordinates));
    assert!(child_by_coordinates (children, coordinates) as *const Child == child as *const Child);
    callback (coordinates, child);
  }
}

pub fn face_by_dimension_and_direction<Face> (faces: &NodeFaces<Face>, dimension: usize, direction: usize)->&Face {
  & faces [dimension] [direction]
}
pub fn face_by_dimension_and_direction_mut<Face> (faces: &mut NodeFaces<Face>, dimension: usize, direction: usize)->&mut Face {
  &mut faces [dimension] [direction]
}
pub fn faces_from_fn<Face, F: FnMut(usize, usize)->Face> (mut initializer: F)->NodeFaces<Face> {
  Array::from_fn (| dimension | {
    Array::from_fn (| direction | {
      initializer (dimension, direction)
    })
  })
}
pub fn iterate_faces<Face, F: FnMut(usize, usize, &Face)> (faces: &NodeFaces<Face>, mut callback: F) {
  for (dimension, subset) in faces.iter().enumerate() {
    for (direction, face) in subset.iter().enumerate() {
      assert!(face_by_dimension_and_direction (faces, dimension, direction) as *const Face == face as *const Face);
      callback (dimension, direction, face)
    }
  }
}


pub fn face_boundary_component_by_coordinates_and_dimension<Physics: TreeContinuumPhysics> (boundaries: &FaceBoundaries<Physics>, coordinates: [usize; DIMENSIONS], dimension: usize)->Option<&BoundaryHandle <Physics>> {
  match boundaries {
    &FaceBoundaries::WorldEdge => None,
    &FaceBoundaries::SingleBoundary (ref handle) => Some(handle),
    &FaceBoundaries::SplitBoundary (ref array) => {
      Some(split_boundary_component_from_coordinates(array, dimension, coordinates))
    }
  }
}

pub fn iterate_face_boundaries <Physics: TreeContinuumPhysics, F: FnMut(& BoundaryHandle <Physics>)> (boundaries: &FaceBoundaries<Physics>, dimension: usize, mut callback: F) {
  match boundaries {
    &FaceBoundaries::WorldEdge => (),
    &FaceBoundaries::SingleBoundary (ref handle) => callback (handle),
    &FaceBoundaries::SplitBoundary (ref array) => {
      iterate_split_boundary (array, dimension, | coordinates, handle | {
        callback (handle);
      });
    }
  }
}


pub fn boundary_by_coordinates_and_dimension<Physics: TreeContinuumPhysics> (boundaries: &NodeBoundaries<Physics>, coordinates: [usize; DIMENSIONS], dimension: usize)->Option<&BoundaryHandle <Physics>> {
  face_boundary_component_by_coordinates_and_dimension(face_by_dimension_and_direction(boundaries, dimension, coordinates [dimension]), coordinates, dimension)
}


pub fn iterate_boundaries <Physics: TreeContinuumPhysics, F: FnMut(usize, usize, & BoundaryHandle <Physics>)> (boundaries: &NodeBoundaries<Physics>, mut callback: F) {
  iterate_faces (boundaries, | dimension, direction, face | {
    iterate_face_boundaries (face, dimension, | boundary | callback (dimension, direction, boundary));
  });
}


// Ignores the coordinate in the normal dimension
pub fn split_boundary_component_from_coordinates<Physics: TreeContinuumPhysics> (boundary: &SplitBoundary<Physics>, dimension: usize, coordinates: [usize; DIMENSIONS])->&BoundaryHandle<Physics> {
  let mut index = 0;
  for (dimension2_bit, dimension2) in (0..DIMENSIONS).filter (| dimension2 | *dimension2 != dimension).enumerate() {
    if coordinates [dimension2] == 1 {
      index += 1<<dimension2_bit;
    }
  }
  & boundary[index]
}
// leaves the coordinate in the normal dimension at 0
pub fn split_boundary_from_fn<Physics: TreeContinuumPhysics, F: FnMut([usize; DIMENSIONS])->BoundaryHandle<Physics>>(dimension: usize, mut initializer: F)->SplitBoundary<Physics> {
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
pub fn iterate_split_boundary<Physics: TreeContinuumPhysics, F: FnMut([usize; DIMENSIONS], & BoundaryHandle <Physics>)>(boundary: &SplitBoundary<Physics>, dimension: usize, mut callback: F) {
  for index in 0..(1 << (DIMENSIONS-1)) {
    let mut coordinates = [0; DIMENSIONS];
    for (dimension2_bit, dimension2) in (0..DIMENSIONS).filter (| dimension2 | *dimension2 != dimension).enumerate() {
      if (index & (1<<dimension2_bit)) != 0 {
        coordinates [dimension2] = 1;
      } else {
        coordinates [dimension2] = 0;
      }
    }
    callback (coordinates, & boundary [index]);
  }
}




pub fn child_center_by_coordinates<Physics: TreeContinuumPhysics> (parent: &NodeHandle<Physics>, coordinates: [usize; DIMENSIONS])->[Distance ; DIMENSIONS] {
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



fn audit_all <Physics: TreeContinuumPhysics, A: EventAccessor <Steward = Physics::Steward>> (accessor: &A, node: &NodeHandle<Physics>) {
  let mut node = node.clone();
  while let Some(next) = node.parent.as_ref().cloned() {
    node = next;
  }
  audit_node (accessor, &node);
}
fn audit_node <Physics: TreeContinuumPhysics, A: EventAccessor <Steward = Physics::Steward>> (accessor: &A, node: &NodeHandle<Physics>) {
  match query(accessor, &node.varying) {
    NodeVarying::Branch (branch) => {
      iterate_children (& branch.children, | coordinates, child | {
        audit_node (accessor, child);
      });
    },
    NodeVarying::Leaf (leaf) => {
      iterate_boundaries (& leaf.boundaries, | dimension, direction, boundary | {
        assert!(&boundary.nodes[(direction+1)&1] == node);
        audit_boundary (accessor, boundary);
      });
    },
  }
}
fn audit_boundary <Physics: TreeContinuumPhysics, A: EventAccessor <Steward = Physics::Steward>> (accessor: &A, boundary: &BoundaryHandle<Physics>) {
  query(accessor, &boundary.varying);
  for direction in 0..2 {
    let neighbor = &boundary.nodes [direction];
    let coordinates_within_neighbor = Array::from_fn (| dimension | {
      if boundary.center [dimension] > neighbor.center [dimension] {1} else {0}
    });
    assert!(
      boundary_by_coordinates_and_dimension (
        &unwrap_leaf (query(accessor, &neighbor.varying)).boundaries,
        coordinates_within_neighbor,
        boundary.normal_dimension,
      ).unwrap() == boundary
    );
  }
}

pub struct NewChildInfo<'a, Physics: TreeContinuumPhysics> {
  pub splitting_node: &'a NodeHandle<Physics>,
  pub splitting_varying: &'a LeafVarying<Physics>,
  pub node: NodeHandle<Physics>,
  pub local_coordinates: [usize; DIMENSIONS],
  pub new_boundaries: NodeFaces<Option<BoundaryHandle<Physics>>>,
  pub old_boundaries: NodeFaces<Option<BoundaryHandle<Physics>>>,
}
pub struct NewBoundaryInfo<'a, Physics: TreeContinuumPhysics> {
  pub splitting_node: &'a NodeHandle<Physics>,
  pub splitting_varying: &'a LeafVarying<Physics>,
  pub boundary: BoundaryHandle<Physics>,
  pub old_boundary: Option<BoundaryHandle<Physics>>,
  pub normal_dimension: usize,
  // 0-1 in all dimensions other than our the normal;
  // 0-2 in the normal
  pub local_coordinates: [usize; DIMENSIONS],
}
pub struct MergeBoundaryInfo<'a, Physics: TreeContinuumPhysics> {
  pub merging_node: &'a NodeHandle<Physics>,
  pub boundary: BoundaryHandle<Physics>,
  //pub old_boundary: Option<BoundaryHandle<Physics>>,
  pub normal_dimension: usize,
  pub direction: usize,
}

pub fn unwrap_leaf_ref<Physics: TreeContinuumPhysics, T: Deref<Target = NodeVarying<Physics>>>(t: &T)->&LeafVarying<Physics> {
  match **t {
    NodeVarying::Branch (_) => panic!(),
    NodeVarying::Leaf (ref l) => l,
  }
}
pub fn unwrap_branch_ref<Physics: TreeContinuumPhysics, T: Deref<Target = NodeVarying<Physics>>>(t: &T)->&BranchVarying<Physics> {
  match **t {
    NodeVarying::Branch (ref b) => b,
    NodeVarying::Leaf (_) => panic!(),
  }
}
pub fn unwrap_leaf<Physics: TreeContinuumPhysics>(t: NodeVarying<Physics>)->LeafVarying<Physics> {
  match t {
    NodeVarying::Branch (_) => panic!(),
    NodeVarying::Leaf (l) => l,
  }
}
pub fn unwrap_branch<Physics: TreeContinuumPhysics>(t: NodeVarying<Physics>)->BranchVarying<Physics> {
  match t {
    NodeVarying::Branch (b) => b,
    NodeVarying::Leaf (_) => panic!(),
  }
}

pub fn split <Physics: TreeContinuumPhysics, A: EventAccessor <Steward = Physics::Steward>> (accessor: &A, splitting_node: &NodeHandle<Physics>) {
  audit_all(accessor, splitting_node);
  let splitting_leaf;
  {
    let mut splitting_varying = unwrap_leaf(tracking_query (accessor, &splitting_node.varying));
    //printlnerr!("{:?}", (splitting_node.width, splitting_node.center, splitting_varying.children.len())));
    
    for dimension in 0..DIMENSIONS {
      for direction in 0..2 {
        if let Some (handle) = match face_by_dimension_and_direction (&splitting_varying.boundaries, dimension, direction) {&FaceBoundaries::SingleBoundary (ref handle) => Some(handle.clone()),_=> None} {
          let neighbor = &handle.nodes [direction];
          if neighbor.width > splitting_node.width {
            split (accessor, neighbor);
            splitting_varying = unwrap_leaf(tracking_query (accessor, &splitting_node.varying));
          }
        }
      }
    };
    
    splitting_leaf = splitting_varying;
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
  
  let mut all_new_boundaries = Vec::new();
  
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
              normal_dimension: dimension,
              center: boundary_center,
              nodes: boundary_nodes,
              varying: DataTimelineCell::new (SimpleTimeline::new ()),
            });
            let mut local_coordinates = coordinates;
            local_coordinates [dimension] <<= 1;
            set (accessor, &new_boundary.varying, BoundaryVarying {
              data: Physics::initialize_split_boundary (accessor, NewBoundaryInfo {
                splitting_node: splitting_node,
                splitting_varying: &splitting_leaf,
                boundary: new_boundary.clone(),
                old_boundary: Some(old_boundary.clone()),
                normal_dimension: dimension,
                local_coordinates: coordinates,
              }),
            });
            all_new_boundaries.push (new_boundary.clone());
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
        normal_dimension: dimension,
        center: boundary_center,
        nodes: boundary_nodes,
        varying: DataTimelineCell::new (SimpleTimeline::new ()),
      });
      let mut local_coordinates = coordinates;
      local_coordinates [dimension] = 1;
      set (accessor, &new_boundary.varying, BoundaryVarying {
        data: Physics::initialize_split_boundary (accessor, NewBoundaryInfo {
          splitting_node: splitting_node,
          splitting_varying: &splitting_leaf,
          boundary: new_boundary.clone(),
          old_boundary: None,
          normal_dimension: dimension,
          local_coordinates: coordinates,
        }),
      });
      all_new_boundaries.push (new_boundary.clone());
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
      data: Physics::initialize_split_child (accessor, NewChildInfo {
        splitting_node: splitting_node,
        splitting_varying: &splitting_leaf,
        node: child.clone(),
        old_boundaries: old_boundaries,
        new_boundaries: new_boundaries,
        local_coordinates: coordinates,
      }),
    }));
  });
  
  iterate_boundaries (&splitting_leaf.boundaries, | dimension, direction, boundary | {
    let neighbor = &boundary.nodes [direction];
    let mut neighbor_varying = unwrap_leaf (tracking_query (accessor, & neighbor.varying));
    let other_direction = (direction + 1) & 1;
    if neighbor.width == splitting_node.width {
      mem::replace (face_by_dimension_and_direction_mut (&mut neighbor_varying.boundaries, dimension, other_direction), FaceBoundaries::SplitBoundary(face_by_dimension_and_direction (& new_exterior_boundaries, dimension, direction).clone().unwrap()));
    }
    else {
      let coordinates: [usize; DIMENSIONS] = Array::from_fn (| dimension2 | if neighbor.center [dimension2] > splitting_node.center [dimension2] {1} else {0});
      mem::replace (face_by_dimension_and_direction_mut (&mut neighbor_varying.boundaries, dimension, other_direction), FaceBoundaries::SingleBoundary(
        split_boundary_component_from_coordinates (
          face_by_dimension_and_direction (& new_exterior_boundaries, dimension, direction).as_ref().unwrap(),
          dimension,
          coordinates
        ).clone()
      ));
    }
    set (accessor, &neighbor.varying, NodeVarying::Leaf(neighbor_varying));
    destroy (accessor, &boundary.varying);
  });
  
  set (accessor, &splitting_node.varying, NodeVarying::Branch(new_branch));
  
  Physics::after_split (accessor, splitting_node, all_new_boundaries);
  audit_all(accessor, splitting_node);
}


pub fn merge <Physics: TreeContinuumPhysics, A: EventAccessor <Steward = Physics::Steward>> (accessor: &A, merging_node: &NodeHandle<Physics>, new_data: Physics::NodeVarying) {
  audit_all(accessor, merging_node);
  let merging_branch = unwrap_branch (tracking_query (accessor, & merging_node.varying));
  
  //let mut discovered_neighbors = HashSet::new();
  //let mut new_boundaries = faces_from_fn(| dimension, direction | FaceBoundaries::WorldEdge);
  let new_boundaries = faces_from_fn (| dimension, direction | {
    let mut test_coordinates = [0; DIMENSIONS];
    test_coordinates [dimension] = direction;
    let test_child = child_by_coordinates (& merging_branch.children, test_coordinates);
    let test_old_boundary = match face_by_dimension_and_direction (& unwrap_leaf_ref (&tracking_query_ref (accessor, & test_child.varying)).boundaries, dimension, direction) {
      &FaceBoundaries::WorldEdge => return FaceBoundaries::WorldEdge,
      &FaceBoundaries::SplitBoundary (_) => {
        panic!("Attemped to merge a node with small neighbors")
      },
      &FaceBoundaries::SingleBoundary (ref old_boundary) => {
        old_boundary.clone()
      }
    };
    let neighbor = &test_old_boundary.nodes[direction];
    
    fn new_boundary <Physics: TreeContinuumPhysics, A: EventAccessor <Steward = Physics::Steward>> (accessor: &A, dimension: usize, direction: usize, merging_node: &NodeHandle <Physics>, neighbor: &NodeHandle <Physics>)->BoundaryHandle<Physics> {
      let other_direction = (direction + 1) & 1;
      
      let mut neighbor_varying = unwrap_leaf (tracking_query (accessor, & neighbor.varying));
              
              let mut boundary_center = neighbor.center;
              let boundary_nodes;
              if direction == 1 {
                boundary_center [dimension] -= neighbor.width >> 1;
                boundary_nodes = [merging_node.clone(), neighbor.clone()];
              }
              else {
                boundary_center [dimension] += neighbor.width >> 1;
                boundary_nodes = [neighbor.clone(), merging_node.clone()];
              }

              let new_boundary = accessor.new_handle (BoundaryData {
                length: neighbor.width,
                normal_dimension: dimension,
                center: boundary_center,
                nodes: boundary_nodes,
                varying: DataTimelineCell::new (SimpleTimeline::new ()),
              });
              set (accessor, &new_boundary.varying, BoundaryVarying {
                data: Physics::initialize_merge_boundary (accessor, MergeBoundaryInfo {
                  merging_node: merging_node,
                  boundary: new_boundary.clone(),
                  //old_boundary: old_boundary.clone(),
                  normal_dimension: dimension,
                  direction: direction,
                }),
              });
              
              mem::replace (face_by_dimension_and_direction_mut (&mut neighbor_varying.boundaries, dimension, other_direction), FaceBoundaries::SingleBoundary(
                new_boundary.clone()
              ));

              set (accessor, &neighbor.varying, NodeVarying::Leaf(neighbor_varying));
      new_boundary
    }
    
    if neighbor.width == merging_node.width {
      FaceBoundaries::SingleBoundary (new_boundary(accessor, dimension, direction, merging_node, neighbor))
    }
    else {
      FaceBoundaries::SplitBoundary(split_boundary_from_fn (dimension, | mut coordinates | {
        coordinates [dimension] = direction;
        let child = child_by_coordinates (& merging_branch.children, coordinates);
        let old_boundary = match face_by_dimension_and_direction (& unwrap_leaf_ref (&tracking_query_ref (accessor, & child.varying)).boundaries, dimension, direction) {
          &FaceBoundaries::WorldEdge => unreachable!(),
          &FaceBoundaries::SplitBoundary (_) => {
            panic!("Attemped to merge a node with small neighbors")
          },
          &FaceBoundaries::SingleBoundary (ref old_boundary) => {
            old_boundary.clone()
          }
        };
        let neighbor = &old_boundary.nodes[direction];
        new_boundary(accessor, dimension, direction, merging_node, neighbor)
      }))
    }
  });
  iterate_children (& merging_branch.children, | coordinates, child | {
    let removed_leaf = unwrap_leaf (tracking_query (accessor, & child.varying));
    iterate_faces (& removed_leaf.boundaries, | dimension, direction, face | {
      match face {
        &FaceBoundaries::WorldEdge => (),
        &FaceBoundaries::SplitBoundary (_) => {
          panic!("Attemped to merge a node with small neighbors")
        },
        &FaceBoundaries::SingleBoundary (ref old_boundary) => {
          // only destroy shared boundaries once
          if coordinates [dimension] == 0 || direction == 1 {
            destroy (accessor, & old_boundary.varying);
          }
        },
      }
    });
    destroy (accessor, &child.varying);
  });
  
  set (accessor, &merging_node.varying, NodeVarying::Leaf(LeafVarying {
    boundaries: new_boundaries,
    data: new_data,
  }));
  audit_all(accessor, merging_node);
}
