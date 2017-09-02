#![feature (i128_type)]

extern crate serde;
#[macro_use]
extern crate serde_derive;

#[macro_use]
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
use time_steward::{DeterministicRandomId};
use time_steward::rowless::api::{self, PersistentTypeId, PersistentlyIdentifiedType, ListedType, StewardData, QueryOffset, DataHandleTrait, DataTimelineCellTrait, ExtendedTime, Basics as BasicsTrait};
use time_steward::rowless::stewards::{simple_flat as steward_module};
use steward_module::{TimeSteward, ConstructibleTimeSteward, IncrementalTimeSteward, Event, DataHandle, DataTimelineCell, EventHandle, Accessor, EventAccessor, FutureCleanupAccessor, SnapshotAccessor, simple_timeline};
use simple_timeline::{SimpleTimeline, GetVarying, IterateUniquelyOwnedPredictions, tracking_query, modify_simple_timeline, unmodify_simple_timeline};


use time_steward::support::rounding_error_tolerant_math::Range;

type Time = i64;
type Distance = i64;
type Amount = i128;
const SECOND: Time = (1 as Time) << 20;
const METER: Distance = (1 as Distance) << 20;
const GENERIC_DENSITY: Amount = (SECOND as Amount) << 20;
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

/*thread_local! {
  static SERIALIZATION_CONTEXT: RefCell<Option <SerializationContext>> = RefCell::new (None);
  static DESERIALIZATION_CONTEXT: RefCell<Option <DeserializationContext>> = RefCell::new (None);
}*/


impl <$($bounds)*> $crate::serde::Serialize for $($concrete)* {
  fn serialize <S: $crate::serde::Serializer> (&self, serializer: S)->Result <S::Ok, S::Error> {
    unimplemented!()
  }
}

impl <'a, $($bounds)*> $crate::serde::Deserialize <'a> for $($concrete)* {
  fn deserialize <D: $crate::serde::Deserializer<'a>> (deserializer: D)->Result <Self, D::Error> {
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
#[derive (Clone, PartialEq, Eq, Debug)]
struct NodeVarying {
  last_change: Time,
  ink_at_last_change: Amount,
  children: Vec<NodeHandle>,
  boundaries: [[Vec<BoundaryHandle>;2];2],
  
  accumulation_rate: Amount,
  slope: [Amount; 2],
}
type NodeHandle = DataHandle <NodeData>;
impl StewardData for NodeData {}
impl StewardData for NodeVarying {}
impl PersistentlyIdentifiedType for NodeData {
  const ID: PersistentTypeId = PersistentTypeId(0x734528f6aefdc1b9);
}
impl IterateUniquelyOwnedPredictions <Steward> for NodeVarying {}
serialization_cheat!([][NodeVarying]);

#[derive (Clone, PartialEq, Eq, Serialize, Deserialize, Debug)]
struct BoundaryData {
  length: Distance,
  center: [Distance ; 2],
  nodes: [NodeHandle; 2],
  varying: DataTimelineCell <SimpleTimeline <BoundaryVarying, Steward>>,
}
#[derive (Clone, PartialEq, Eq, Debug)]
struct BoundaryVarying {
  transfer_velocity: Amount,
  next_change: Option <EventHandle <Basics>>,
}
type BoundaryHandle = DataHandle <BoundaryData>;
impl StewardData for BoundaryData {}
impl StewardData for BoundaryVarying {}
impl PersistentlyIdentifiedType for BoundaryData {
  const ID: PersistentTypeId = PersistentTypeId(0x9d643214b58b24dc);
}
impl IterateUniquelyOwnedPredictions <Steward> for BoundaryVarying {
  fn iterate_predictions <F: FnMut (& <Steward as TimeSteward>::EventHandle)> (&self, callback: &mut F) {
    if let Some (prediction) = self.next_change.as_ref() {
      callback (prediction);
    }
  }
}
serialization_cheat!([][BoundaryVarying]);

macro_rules! get {
  ($accessor: expr, $cell: expr) => {
    ($accessor.query ($cell, &GetVarying, QueryOffset::After).unwrap().1)
  }
}
macro_rules! set {
  ($accessor: expr, $cell: expr, $field: ident, $value: expr) => {
    {
      let mut value = $accessor.query ($cell, &GetVarying, QueryOffset::After).unwrap().1;
      value.$field = $value;
      modify_simple_timeline ($accessor, $cell, Some (value));
    }
  }
}
macro_rules! set_with {
  ($accessor: expr, $cell: expr, | $varying: ident | $actions: expr) => {
    {
      let mut $varying = $accessor.query ($cell, &GetVarying, QueryOffset::After).unwrap().1;
      $actions;
      modify_simple_timeline ($accessor, $cell, Some ($varying));
    }
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
        modify_simple_timeline (self.accessor, handle, Some(node.varying));
      }
    }
    for (handle, boundary) in self.node_clones.iter() {
      if boundary.modified {
        modify_simple_timeline (self.accessor, handle, Some(boundary.varying));
      }
    }
  }
}*/

fn update_inferred_node_properties <A: EventAccessor <Steward = Steward >> (accessor: &A, node: &NodeHandle) {
  let mut varying = accessor.query (&node.varying, &GetVarying, QueryOffset::After).unwrap().1;
  varying.slope = [0, 0];
  varying.accumulation_rate = 0;
  for dimension in 0..2 {
    for direction in 0..2 {
      let direction_signum = (direction as Amount*2)-1;
      for boundary in varying.boundaries [dimension] [direction].iter() {
        let boundary_varying = get!(accessor, & boundary.varying);
        let transfer_rate = boundary_varying.transfer_velocity*boundary.length as Amount;
        varying.accumulation_rate += transfer_rate*direction_signum;
        varying.slope [dimension] += transfer_rate; // divided by
        // (2*node.width*VELOCITY_PER_SLOPE), but we do that later as an optimization
      }
    }
    varying.slope [dimension] /= (2*node.width as Amount)*VELOCITY_PER_SLOPE;
  }
  modify_simple_timeline (accessor, &node.varying, Some (varying));
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
  let ideal_transfer_slope = density_difference*2/((nodes [0].width + nodes [1].width) as Amount);
  ideal_transfer_slope*VELOCITY_PER_SLOPE
}
fn transfer_velocity_to_ideal_density_difference (nodes: [&NodeHandle;2], transfer_velocity: Amount)->Amount {
  let ideal_transfer_slope = transfer_velocity/VELOCITY_PER_SLOPE;
  ideal_transfer_slope*((nodes [0].width + nodes [1].width) as Amount)/2
}


fn update_transfer_change_prediction <A: EventAccessor <Steward = Steward >> (accessor: &A, boundary: &BoundaryHandle) {
  set_with!(accessor, &boundary.varying, | boundary_varying | {
  let nodes = &boundary.nodes;
  let densities = [
    density_at (accessor, &nodes[0], *accessor.now()),
    density_at (accessor, &nodes[1], *accessor.now()),
  ];
  let density_accumulations = [
    get!(accessor, &nodes[0].varying).accumulation_rate/((nodes[0].width*nodes[0].width) as Amount),
    get!(accessor, &nodes[1].varying).accumulation_rate/((nodes[1].width*nodes[1].width) as Amount),
  ];
  let difference_now = densities[0] - densities[1];
  let accumulation_difference = density_accumulations[0] - density_accumulations[1];
  // the more stable the boundary is, the less error we permit
  let permissible_error = accumulation_difference.abs()/8 + 2;
  // Note to self: it may look weird for the boundary conditions to be based on
  // the CURRENT difference, when it's ever-changing, but remember
  // that "when the current gets too far from the ideal" is the same as
  // "when the ideal gets too far from the current"
  let (min_difference, max_difference) = (
    transfer_velocity_to_ideal_density_difference([&nodes[0], &nodes[1]], difference_now - permissible_error),
    transfer_velocity_to_ideal_density_difference([&nodes[0], &nodes[1]], difference_now + permissible_error),
  );
  let time = if difference_now < min_difference || difference_now > max_difference {
    Some(*accessor.now())
  } else if accumulation_difference > 0 {
    Some(*accessor.now() + ((max_difference-difference_now)/accumulation_difference) as Time)
  }
  else if accumulation_difference < 0 {
    Some(*accessor.now() + ((min_difference-difference_now)/accumulation_difference) as Time)
  }
  else {
    None
  };
  
  if let Some (discarded) = boundary_varying.next_change.take() {accessor.destroy_prediction (&discarded);}
  boundary_varying.next_change = time.map (|time| {
    accessor.create_prediction (
      time,
      DeterministicRandomId::new (&(accessor.id(), boundary.center)),
      TransferChange {boundary: boundary.clone()}
    )
  });
  
  });
}

fn split <A: EventAccessor <Steward = Steward >> (accessor: &A, node: &NodeHandle) {
  set_with!(accessor, &node.varying, | varying | {
  assert!(varying.children.is_empty());
  let mut velocities = [[[0;2];2];2];
  let mut old_boundaries = [[[None,None],[None,None]],[[None,None],[None,None]]];
  let mut middle_velocities = [[0;2];2];
  for dimension in 0..2 {
    for direction in 0..2 {
      //let direction_signum = (direction*2)-1;
      //let other_direction = (direction + 1) & 1;
      let boundaries = &varying.boundaries [dimension] [direction];
      if boundaries.len() == 1 {
        loop {
          let boundary = &boundaries [0];
          let neighbor = &boundary.nodes [direction];
          if neighbor.width <= node.width {
            break;
          }
          split (accessor, &neighbor);
        }
        velocities [dimension] [direction] = [get!(accessor, &boundaries [0].varying).transfer_velocity, get!(accessor, &boundaries [0].varying).transfer_velocity];
        old_boundaries [dimension] [direction] = [Some(boundaries [0].clone()), Some(boundaries [0].clone())];
      }
      else {
        velocities [dimension] [direction] = [get!(accessor, &boundaries [0].varying).transfer_velocity, get!(accessor, &boundaries [1].varying).transfer_velocity];
        old_boundaries [dimension] [direction] = [Some(boundaries [0].clone()), Some(boundaries [1].clone())];
      }
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
      let new_child = DataHandle::new (NodeData {
        width: node.width >> 1,
        center: center,
        parent: Some (node.clone()),
        varying: DataTimelineCell::new (SimpleTimeline::new ()),
      });
      modify_simple_timeline (accessor, &new_child.varying, Some (NodeVarying {
        last_change: *accessor.now(),
        ink_at_last_change: varying.ink_at_last_change >> 2,
        children: Vec::new(),
        boundaries: [[Vec::new(), Vec::new()], [Vec::new(), Vec::new()]],
        accumulation_rate: 0,
        slope: [0; 2],
      }));
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
          set_with!(accessor, &neighbor.varying, | neighbor_varying | {
            neighbor_varying.boundaries [dimension] [other_direction].clear();
          });
        }
      }
    }
  }
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
          let new_boundary = DataHandle::new (BoundaryData {
            length: node.width >> 1,
            center: center,
            nodes: if direction == 0 { [neighbor.clone(), child.clone()] } else { [child.clone(), neighbor.clone()] },
            varying: DataTimelineCell::new (SimpleTimeline::new ()),
          });
          modify_simple_timeline (accessor, &new_boundary.varying, Some (BoundaryVarying {
            transfer_velocity: velocities [dimension] [direction] [which],
            next_change: None,
          }));
          set_with!(accessor, &child.varying, | child_varying | {
            child_varying.boundaries [dimension] [direction].push(new_boundary.clone());
          });
          set_with!(accessor, &neighbor.varying, | neighbor_varying | {
            neighbor_varying.boundaries [dimension] [other_direction].push(new_boundary);
          });
        }
      }
    }
  }
  for dimension in 0..2 {
    let other_dimension = (dimension + 1) & 1;
    for which in 0..2 {
      let which_signum = (which as Distance*2)-1;
      let child0 = new_children [dimension] [0] [which].as_ref().unwrap();
      let child1 = new_children [dimension] [0] [which].as_ref().unwrap();
      let mut center = node.center;
      center [other_dimension] += (node.width >> 4)*which_signum;
      let new_boundary = DataHandle::new (BoundaryData {
        length: node.width >> 1,
        center: center,
        nodes: [child0.clone(), child1.clone()],
        varying: DataTimelineCell::new (SimpleTimeline::new ()),
      });
      modify_simple_timeline (accessor, &new_boundary.varying, Some (BoundaryVarying {
        transfer_velocity: middle_velocities [dimension] [which],
        next_change: None,
      }));
      set_with!(accessor, &child0.varying, | child0_varying | {
        child0_varying.boundaries [dimension] [1].push(new_boundary.clone());
      });
      set_with!(accessor, &child1.varying, | child1_varying | {
        child1_varying.boundaries [dimension] [0].push(new_boundary);
      });
    }
  }
  
  });
}


#[derive (Clone, PartialEq, Eq, Serialize, Deserialize, Debug)]
struct Globals {
  size: [Distance; 2],

  
  root: NodeHandle,
}
impl StewardData for Globals {}



/// The TransferChange event, as used above.
#[derive (Clone, PartialEq, Eq, Serialize, Deserialize, Debug)]
struct TransferChange {boundary: BoundaryHandle}
impl StewardData for TransferChange {}
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
    set!(accessor, &self.boundary.varying, transfer_velocity,  density_difference_to_ideal_transfer_velocity ([&nodes[0], &nodes[1]],
      density_at (accessor, &nodes[0], *accessor.now())-
      density_at (accessor, &nodes[1], *accessor.now())
    ));
    for node in nodes.iter() {
      update_inferred_node_properties (accessor, node);
    }
    for node in nodes.iter() {
      for whatever in get!(accessor, &node.varying).boundaries.iter() {for something in whatever.iter() {for other_boundary in something.iter() {
        update_transfer_change_prediction (accessor, other_boundary);
      }}}
    }
    
    // TODO: invalidation
  }

  fn undo <Accessor: FutureCleanupAccessor <Steward = Self::Steward>> (&self, accessor: &mut Accessor, _: ()) {
    unimplemented!()
  }
}


#[derive (Clone, PartialEq, Eq, Debug)]
struct AddInk {coordinates: [Distance; 2], amount: Amount, accumulation: Amount}
serialization_cheat!([][AddInk]);
impl StewardData for AddInk {}
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
    });
    //node.fiat_accumulation_rate += self.accumulation;
  }
  fn undo <Accessor: FutureCleanupAccessor <Steward = Self::Steward>> (&self, accessor: &mut Accessor, _: ()) {
    unimplemented!()
  }
}


#[derive (Clone, PartialEq, Eq, Serialize, Deserialize, Debug)]
struct Initialize {}
impl StewardData for Initialize {}
impl PersistentlyIdentifiedType for Initialize {
  const ID: PersistentTypeId = PersistentTypeId(0xf0d2d9134cfe9b49);
}
impl Event for Initialize {
  type Steward = Steward;
  type ExecutionData = ();
  fn execute <Accessor: EventAccessor <Steward = Self::Steward>> (&self, accessor: &mut Accessor) {
    let globals = accessor.globals();
    modify_simple_timeline (accessor, &globals.root.varying, Some (NodeVarying {
      last_change: 0,
      ink_at_last_change: 0,
      children: Vec::new(),
      boundaries: [[Vec::new(), Vec::new()], [Vec::new(), Vec::new()]],
      accumulation_rate: 0,
      slope: [0; 2],
    }));
  }
  fn undo <Accessor: FutureCleanupAccessor <Steward = Self::Steward>> (&self, accessor: &mut Accessor, _: ()) {
    unimplemented!()
  }
}

fn make_globals()-> Globals {
  Globals {
    size: [64*METER, 64*METER],
    root: DataHandle::new (NodeData {
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
gl_Position = vec4 (center+direction, 0.0, 1.0);

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
    let globals = accessor.globals();
    
    for ev in display.poll_events() {
      match ev {
        glium::glutin::Event::Closed => return true,
        glium::glutin::Event::MouseMoved (x,y) => {
          mouse_coordinates [0] = (x as Distance) * accessor.globals().size [0] / display.get_window().unwrap().get_inner_size_pixels().unwrap().0 as Distance;
          mouse_coordinates [1] = (display.get_window().unwrap().get_inner_size_pixels().unwrap().1 as Distance-(y as Distance)) * accessor.globals().size [1] / display.get_window().unwrap().get_inner_size_pixels().unwrap().1 as Distance;
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
    /*while let Some ((x,y)) = emscripten_compatibility::pop_click() {
      // TODO duplicate code
      mouse_coordinates [0] = (x*60.0) as i32;
      mouse_coordinates [1] = ((1.0-y)*60.0) as i32;
      //if in_bounds (globals, mouse_coordinates) {
        event_index += 1;
        stew.insert_fiat_event (time, DeterministicRandomId::new (& event_index), AddInk {
            coordinates: [mouse_coordinates [0], mouse_coordinates [1]],
            amount: ((input_derivative+1)%2)*(generic_amount << input_magnitude_shift)*input_signum,
            accumulation: input_derivative*(generic_amount << input_magnitude_shift)*input_signum/SECOND,
        }).unwrap();
      /*}
      else {
        display_state = (display_state + 1) % 3;
      }*/
    }*/

    let mut target = display.draw();
    target.clear_color(1.0, 1.0, 1.0, 1.0);
    let mut vertices = Vec::<Vertex>::new();
    
    let mut to_draw = vec![accessor.globals().root.clone()];
    while let Some(handle) = to_draw.pop() {
      let varying: NodeVarying = get!(accessor, &handle.varying);
      if varying.children.len() > 0 {
        for child in varying.children.iter() {
          to_draw.push(child.clone());
        }
      }
      else {
        let my_current_ink = match display_state {
          0 => (density_at (&accessor, &handle, *accessor.now()) as f64 / (GENERIC_DENSITY as f64)) as f32,
          1 => (
              ((varying.accumulation_rate * (SECOND as Amount) * 100 / (handle.width*handle.width) as Amount) as f64)
              / (GENERIC_DENSITY as f64)
            ) as f32,
          _ => ((accessor.now() - varying.last_change)*50000000000 / SECOND) as f32,
        };
        let center = handle.center;
        let slope = varying.slope;
        let offset = handle.width/2;
        let size = accessor.globals().size [0] as f32;
        let vertex = |x,y| Vertex {
          direction: [(offset*x) as f32/size, (offset*y) as f32/size],
          center: [center [0] as f32/size, center [1] as f32/size],
          slope: [slope [0] as f32*size, slope [1] as f32*size],
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
