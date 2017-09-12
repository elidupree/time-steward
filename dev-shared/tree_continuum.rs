macro_rules! printlnerr(
    ($($arg:tt)*) => { {use std::io::Write;
        let r = writeln!(&mut ::std::io::stderr(), $($arg)*);
        r.expect("failed printing to stderr");
    } }
);


trait TreeContinuumPhysics {
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

enum Face<Physics: TreeContinuumPhysics> {
  WorldEdge,
  SingleBoundary (BoundaryHandle<Physics>),
  SplitBoundary ([BoundaryHandle<Physics>; 1<<(<Physics as TreeContinuumPhysics>::DIMENSIONS-1)]),
}

#[derive (Clone, PartialEq, Eq, Serialize, Deserialize, Debug)]
struct NodeData<Physics: TreeContinuumPhysics> {
  width: Distance,
  center: [Distance ; Physics::DIMENSIONS],
  parent: Option <NodeHandle<Physics>>,
  varying: DataTimelineCell <SimpleTimeline <NodeVarying<Physics>, Physics::Steward>>,
}
#[derive (Clone, PartialEq, Eq, Serialize, Deserialize, Debug)]
struct NodeVarying<Physics: TreeContinuumPhysics> {
  children: Vec<NodeHandle<Physics>>,
  boundaries: [[Face<Physics>; 2]; Physics::DIMENSIONS],
  data: Physics::NodeVarying,
}
type NodeHandle<Physics> = DataHandle <NodeData<Physics>>;
impl<Physics: TreeContinuumPhysics> PersistentlyIdentifiedType for NodeData<Physics> {
  const ID: PersistentTypeId = PersistentTypeId(Physics::ID ^ 0x0d838bdd804f48d7);
}

#[derive (Clone, PartialEq, Eq, Serialize, Deserialize, Debug)]
struct BoundaryData<Physics: TreeContinuumPhysics> {
  length: Distance,
  center: [Distance ; 2],
  nodes: [NodeHandle<Physics>; 2],
  varying: DataTimelineCell <SimpleTimeline <BoundaryVarying<Physics>, Physics::Steward>>,
}
#[derive (Clone, PartialEq, Eq, Serialize, Deserialize, Debug)]
struct BoundaryVarying<Physics: TreeContinuumPhysics> {
  data: Physics::BoundaryVarying,
}
type BoundaryHandle<Physics> = DataHandle <BoundaryData<Physics>>;
impl<Physics: TreeContinuumPhysics> PersistentlyIdentifiedType for BoundaryData<Physics> {
  const ID: PersistentTypeId = PersistentTypeId(Physics::ID ^ 0x4913f629aef09374);
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
  local_coordinates: [usize; Physics::DIMENSIONS],
  old_boundaries: [usize; Physics::DIMENSIONS*2],
}
struct SubBoundaryInfo<Physics: TreeContinuumPhysics> {
  boundary: BoundaryHandle<Physics>,
  old_boundary: BoundaryHandle<Physics>,
  normal_dimension: usize,
  // 0-1 in all dimensions other than our the normal;
  // 0-2 in the normal
  local_coordinates: [usize; Physics::DIMENSIONS],
  
}

/*

fn split <Physics: TreeContinuumPhysics, A: EventAccessor <Steward = Physics::Steward>, N, B> (accessor: &A, node: &NodeHandle, node_initializer: N, boundary_initializer: B) where N: Fn(SubNodeInfo<Physics>)->Physics::NodeVarying, B: Fn(SubBoundaryInfo<Physics>, [Option<SubNodeInfo<Physics>>; 2])->Physics::BoundaryVarying {
  let mut varying = tracking_query (accessor, &node.varying);
  //printlnerr!("{:?}", (node.width, node.center, varying.children.len()));
  //if !varying.children.is_empty() {
    //printlnerr!("{:?}", (node.width, node.center, varying.children.len()));
  //}
  assert!(varying.children.is_empty());
  let mut old_boundaries = [[[None,None],[None,None]],[[None,None],[None,None]]];
  let mut middle_velocities = [[0;2];2];
  for dimension in 0.. Physics::DIMENSIONS {
    for direction in 0..2 {
      Face::WorldEdge => (),
      //let direction_signum = (direction*2)-1;
      let other_direction = (direction + 1) & 1;
      
      match varying.boundaries [dimension] [direction] {
         => {
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
        
        _ => unreachable!(),
      };
    }
    middle_velocities [dimension] = [
      (velocities [dimension] [0] [0] + velocities [dimension] [1] [0]) / 2,
      (velocities [dimension] [0] [1] + velocities [dimension] [1] [1]) / 2,
    ];
  }
  
  for index in 0..(1 << Physics::DIMENSIONS) {
    let mut center = node.center;
    let mut coordinates = [0; Physics::DIMENSIONS];
    for dimension in 0..Physics::DIMENSIONS {
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
  }
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
}*/
