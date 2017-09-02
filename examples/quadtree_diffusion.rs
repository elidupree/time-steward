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
const GENERIC_INK_AMOUNT: Amount = (METER*METER*SECOND) << 20;

// Velocity is "ink units per space unit per time unit"
// and slope is "ink density per space unit", i.e. "ink units per space unit^3"
// 
// Let's say we want flow across a meter^2 cell, from a density of GENERIC_INK_AMOUNT/m^2 to 0, to drain GENERIC_INK_AMOUNT per second. Then
//   slope = GENERIC_INK_AMOUNT/m^3
//   velocity = GENERIC_INK_AMOUNT/m*s
// so
const VELOCITY_PER_SLOPE: Amount = METER*METER / SECOND;

type Steward = steward_module::Steward <Basics>;


#[derive (Clone, PartialEq, Eq, Serialize, Deserialize, Debug)]
struct NodeData {
  width: Distance,
  center: [Distance ; 2],
  parent: Option <DataHandle <NodeData>>,
  varying: DataTimelineCell <SimpleTimeline <NodeVarying, Steward>>,
}
#[derive (Clone, PartialEq, Eq, Serialize, Deserialize, Debug)]
struct NodeVarying {
  last_change: Time,
  ink_at_last_change: Amount,
  children: Vec<DataHandle <NodeData>>,
  boundaries: [[Vec<DataHandle <BoundaryData>>;2];2],
  
  accumulation_rate: Amount,
  slope: [Amount; 2],
}

#[derive (Clone, PartialEq, Eq, Serialize, Deserialize, Debug)]
struct BoundaryData {
  length: Distance,
  center: [Distance ; 2],
  varying: DataTimelineCell <SimpleTimeline <BoundaryVarying, Steward>>,
}
#[derive (Clone, PartialEq, Eq, Serialize, Deserialize, Debug)]
struct BoundaryVarying {
  transfer_velocity: Amount,
  next_change: Option <EventHandle <Basics>>,
}

macro_rules! get {
  ($accessor: expr, $cell: expr, $field: ident) => {
    $accessor.query ($cell, &GetVarying, QueryOffset::After).1.unwrap().$field
  }
}
macro_rules! set {
  ($accessor: expr, $cell: expr, $field: ident, $value: expr) => {
    {
      let mut value = $accessor.query ($cell, &GetVarying, QueryOffset::After).1.unwrap();
      value.$field = value
      modify_simple_timeline ($cell, Some (value));
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

fn update_inferred_node_properties (node: &mut ?????) {
  node.slope = [0, 0];
  node.accumulation_rate = 0;
  for dimension in 0..2 {
    for direction in 0..2 {
      let direction_signum = (direction*2)-1;
      for boundary in node.boundaries [dimension] [direction] {
        let transfer_rate = boundary.transfer_velocity*boundary.length;
        node.accumulation_rate += transfer_rate*direction_signum;
        node.slope [dimension] += transfer_rate; // divided by
        // (2*node.width*VELOCITY_PER_SLOPE), but we do that later as an optimization
      }
    }
    result.slope [dimension] /= (2*node.width*VELOCITY_PER_SLOPE);
  }
  result
}

fn update_node (node: &mut ?????, time: Time) {
  node.ink_at_last_change = ink_at (node, node.center, time);
  note.last_change = time;
}

fn ink_at (node: &?????, time: Time) {
  node.ink_at_last_change + node.accumulation_rate*(time - node.last_change)
}
fn density_at (node: &?????, time: Time) {
  ink_at (node, time)/(node.width*node.width),
}

fn inferred_density_trajectory_at (node: &?????, location: [Amount; 2], time: Time)->[Amount; 2] {
  let area = node.width*node.width;
  [
    ink_at(node, time)/area
      + node.slope [0]*(location [0] - note.center [0])
      + node.slope [1]*(location [1] - note.center [1]),
    node.accumulation_rate/area
  ]
}


fn density_difference_to_ideal_transfer_velocity (nodes: [& ?????;2], density_difference: Amount) {
  let ideal_transfer_slope = density_difference*2/(nodes [0].width + nodes [1].width);
  ideal_transfer_slope*VELOCITY_PER_SLOPE
}
fn transfer_velocity_to_ideal_density_difference (nodes: [& ?????;2], transfer_velocity: Amount) {
  let ideal_transfer_slope = transfer_velocity/VELOCITY_PER_SLOPE;
  ideal_transfer_slope*(nodes [0].width + nodes [1].width)/2
}


fn update_transfer_change_prediction (nodes: [& ?????;2], boundary:?) {
  let densities = [
    density_at (nodes[0], *accessor.now()),
    density_at (nodes[1], *accessor.now()),
  ];
  let density_accumulations = [
    nodes[0].accumulation_rate/(nodes[0].width*nodes[0].width),
    nodes[1].accumulation_rate/(nodes[1].width*nodes[1].width),
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
    transfer_velocity_to_ideal_density_difference(difference_now - permissible_error),
    transfer_velocity_to_ideal_density_difference(difference_now + permissible_error),
  );
  let time = if original_difference < min_difference || original_difference > max_difference {
    Some(*accessor.now())
  } else if accumulation_difference > 0 {
    Some(*accessor.now() + (max_difference-original_difference)/difference_change_rate);
  }
  else if accumulation_difference < 0 {
    Some(*accessor.now() + (min_difference-original_difference)/difference_change_rate);
  }
  else {
    None
  }
  
  if let Some (discarded) = boundary.next_change.take() {accessor.destroy_prediction (&discarded);}
  boundary.next_change = time.map (|time| {
    accessor.create_prediction (
      time,
      DeterministicRandomId::new (&(accessor.id(), boundary.center)),
      TransferChange {boundary: boundary}
    )
  });
  
  modify_simple_timeline (accessor, & boundary, Some(boundary));
}

fn split (node: &mut ?????) {
  assert!(node.children.is_empty());
  let mut velocities = [[[0;2];2];2];
  let mut neighbors = [[[;2];2];2];
  let mut middle_velocities = [[0;2];2];
  for dimension in 0..2 {
    for direction in 0..2 {
      //let direction_signum = (direction*2)-1;
      //let other_direction = (direction + 1) & 1;
      let boundaries = &node.boundaries [dimension] [direction];
      if boundaries.len() == 1 {
        loop {
          let boundary = &boundaries [0];
          let neighbor = boundary.nodes [direction];
          if neighbor.width <= node.width {
            break;
          }
          split (neighbor);
        }
        velocities [dimension] [direction] = [boundaries [0].transfer_velocity, boundaries [0].transfer_velocity];
        neighbors [dimension] [direction] = [boundaries [0], [boundaries [0]];
      }
      else {
        velocities [dimension] [direction] = [boundaries [0].transfer_velocity, boundaries [1].transfer_velocity];
        neighbors [dimension] [direction] = [boundaries [0], [boundaries [1]];
      }
    }
    middle_velocities [dimension] = [
      (velocities [dimension] [0] [0] + velocities [dimension] [1] [0]) / 2,
      (velocities [dimension] [0] [1] + velocities [dimension] [1] [1]) / 2,
    ];
  }
  
  // TODO: more accurate computation of ideal middle velocities and ink amounts,
  // and conserve ink correctly
  let mut new_children = [[[;2];2];2];
  for x in 0..2 {
    let x_signum = (x*2)-1;
    for y in 0..2 {
      let y_signum = (y*2)-1;
      let center = [
        node.center[0] + (node.width >> 2)*x_signum,
        node.center[1] + (node.width >> 2)*y_signum,
      ];
      let new_child = Node {
        width: node.width >> 1,
        center: center,
        ink_at_last_change: node.ink_at_last_change >> 2,
        last_change: time,
        boundaries: Vec::new(),
      }
      node.children.push (new_child)
      new_children [0][x][y] = new_child;
      new_children [1][y][x] = new_child;
    }
  }
  for dimension in 0..2 {
    for direction in 0..2 {
      let other_direction = (direction + 1) & 1;
      for which in 0..2 {
        let neighbor = neighbors [dimension] [direction] [which];
        neighbor.boundaries [dimension] [other_direction].clear();
      }
    }
  }
  for dimension in 0..2 {
    for direction in 0..2 {
      let other_direction = (direction + 1) & 1;
      for which in 0..2 {
        let neighbor = neighbors [dimension] [direction] [which];
        let child = new_children [dimension] [direction] [which];
        let new_boundary = Boundary {
          length: node.width >> 1,
          nodes: if direction == 0 { [neighbor, child] } else { [child, neighbor] },
          transfer_velocity: velocities [dimension] [direction] [which],
        };
        child.boundaries [dimension] [direction].push(new_boundary);
        neighbor.boundaries [dimension] [other_direction].push(new_boundary);
      }
    }
  }
  for dimension in 0..2 {
    for which in 0..2 {
      let child0 = new_children [dimension] [0] [which];
      let child1 = new_children [dimension] [0] [which];
      let new_boundary = Boundary {
        length: node.width >> 1,
        nodes: [child0, child1],
        transfer_velocity: middle_velocities [dimension] [which],
      };
      child0.boundaries [dimension] [1].push(new_boundary);
      child1.boundaries [dimension] [1].push(new_boundary);
    }
  }
}


#[derive (Clone, PartialEq, Eq, Serialize, Deserialize, Debug)]
struct Globals {
  size: [Amount; 2],

  
  root: Node,
}
impl StewardData for Globals {}

impl IterateUniquelyOwnedPredictions <Steward> for CellVarying {}
impl IterateUniquelyOwnedPredictions <Steward> for Boundary {
  fn iterate_predictions <F: FnMut (& <Steward as TimeSteward>::EventHandle)> (&self, callback: &mut F) {
    if let Some (prediction) = self.next_change.as_ref() {
      callback (prediction);
    }
  }
}

/// The TransferChange event, as used above.
#[derive (Clone, PartialEq, Eq, Serialize, Deserialize, Debug)]
struct TransferChange {coordinates: [i32; 2], dimension: usize}
impl StewardData for TransferChange {}
impl PersistentlyIdentifiedType for TransferChange {
  const ID: PersistentTypeId = PersistentTypeId(0xd6621e9cfad1c765);
}
impl Event for TransferChange {
  type Steward = Steward;
  type ExecutionData = ();
  fn execute <Accessor: EventAccessor <Steward = Self::Steward>> (&self, accessor: &mut Accessor) {
    for node in self.boundary.nodes {
      update_node (node);
    }
    boundary.transfer_velocity = density_difference_to_ideal_transfer_velocity (self.boundary.nodes, density_at (nodes[0], *accessor.now())-
        density_at (nodes[1], *accessor.now()));
    for node in self.boundary.nodes {
      update_inferred_node_properties (node);
    }
    for node in self.boundary.nodes {
      for whatever in node.boundaries.iter() {for something in whatever.iter() {for other_boundary in something.iter() {
        update_transfer_change_prediction (other_boundary);
      }}}
    }
    
    // TODO: invalidation
  }

  fn undo <Accessor: FutureCleanupAccessor <Steward = Self::Steward>> (&self, accessor: &mut Accessor, _: ()) {
    unimplemented!()
  }
}


/// In order to get the simulation going, we need two more events:
/// One to be the FiatEvent that initializes the cells to an initial empty state,
/// and one to be the FiatEvent that adds or removes ink in some way at real-time.
///
/// For these simple events, we lazily use the pseudo-closure syntax.
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
    for x in 0..globals.size [0] {
      for y in 0..globals.size [1] {
        let cell = get_cell (accessor, [x,y]).unwrap();
        modify_simple_timeline (accessor, & cell.varying, Some(CellVarying {
          last_change: 0,
          ink_at_last_change: 0,
          fiat_accumulation_rate: 0,
        }));
        for dimension in 0..2 {
          modify_simple_timeline (accessor, & cell.transfers [dimension], Some(TransferVarying {
            rate: 0,
            last_change: 0,
            accumulated_error: 0,
            next_change: None,
          }));
        }
      }
    }
  }
  fn undo <Accessor: FutureCleanupAccessor <Steward = Self::Steward>> (&self, accessor: &mut Accessor, _: ()) {
    unimplemented!()
  }
}
#[derive (Clone, PartialEq, Eq, Serialize, Deserialize, Debug)]
struct AddInk {coordinates: [Distance; 2], amount: i64, accumulation: i64}
impl StewardData for AddInk {}
impl PersistentlyIdentifiedType for AddInk {
  const ID: PersistentTypeId = PersistentTypeId(0x3e6d029c3da8b9a2);
}
impl Event for AddInk {
  type Steward = Steward;
  type ExecutionData = ();
  fn execute <Accessor: EventAccessor <Steward = Self::Steward>> (&self, accessor: &mut Accessor) {
    let mut node = accessor.globals().root;
    update_node (node);
    while node.width > METER {
      if node.children.is_empty() {
        split (node);
      }
      node = node.children
        [if self.coordinates [0] > node.center [0] {1} else {0}]
        [if self.coordinates [1] > node.center [1] {1} else {0}];
    )
    node.ink_at_last_change += self.amount;
    //node.fiat_accumulation_rate += self.accumulation;
  }
  fn undo <Accessor: FutureCleanupAccessor <Steward = Self::Steward>> (&self, accessor: &mut Accessor, _: ()) {
    unimplemented!()
  }
}

fn make_globals()-> Globals {
  Globals {
    size: [64*METER, 64*METER],
    root: cells,
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
  location: [f32; 2],
  ink: f32,
}
implement_vertex!(Vertex, location, ink);

fn main() {
  let mut steward: Steward = Steward::from_globals(make_globals());
  steward.insert_fiat_event(0, DeterministicRandomId::new(&0), Initialize{}).unwrap();
  run (steward, |_,_|());
}


fn run <F: Fn (&mut Steward, Time)>(mut stew: Steward, settle:F) {


  let vertex_shader_source = r#"
#version 100
attribute lowp vec2 location;
attribute lowp vec2 slope;
attribute lowp vec2 direction;
attribute lowp float density;
varying lowp float density_transfer;

void main() {
gl_Position = vec4 (location+direction, 0.0, 1.0);

density_transfer = ink + dot(direction, slope);
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
  let generic_amount = 333333333333; // threes to encourage rounding error
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
          mouse_coordinates [0] = (x as i32) * 60 / display.get_window().unwrap().get_inner_size_pixels().unwrap().0 as i32;
          mouse_coordinates [1] = (display.get_window().unwrap().get_inner_size_pixels().unwrap().1 as i32-(y as i32)) * 60 / display.get_window().unwrap().get_inner_size_pixels().unwrap().1 as i32;
        },
        glium::glutin::Event::MouseInput (_,_) => {
          if in_bounds (globals, mouse_coordinates) {
            event_index += 1;
            stew.insert_fiat_event (time, DeterministicRandomId::new (& event_index), AddInk {
              coordinates: [mouse_coordinates [0], mouse_coordinates [1]],
              amount: ((input_derivative+1)%2)*(generic_amount << input_magnitude_shift)*input_signum,
              accumulation: input_derivative*(generic_amount << input_magnitude_shift)*input_signum/SECOND,
            }).unwrap();
          }
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
      mouse_coordinates [0] = (x*60.0) as i32;
      mouse_coordinates [1] = ((1.0-y)*60.0) as i32;
      if in_bounds (globals, mouse_coordinates) {
        event_index += 1;
        stew.insert_fiat_event (time, DeterministicRandomId::new (& event_index), AddInk {
            coordinates: [mouse_coordinates [0], mouse_coordinates [1]],
            amount: ((input_derivative+1)%2)*(generic_amount << input_magnitude_shift)*input_signum,
            accumulation: input_derivative*(generic_amount << input_magnitude_shift)*input_signum/SECOND,
        }).unwrap();
      }
      else {
        display_state = (display_state + 1) % 3;
      }
    }

    let mut target = display.draw();
    target.clear_color(1.0, 1.0, 1.0, 1.0);
    let mut vertices = Vec::<Vertex>::new();
    
    for x in 0.. globals.size [0] {
      for y in 0.. globals.size [1] {
        let me = get_cell (& accessor, [x,y]).unwrap();
        let my_varying = accessor.query (&me.varying, & GetVarying, QueryOffset::After).unwrap().1;
        let my_current_ink = match display_state {
          0 => ink_at (&my_varying, get_accumulation_rate (& accessor, [x,y]), *accessor.now()) as f32,
          1 => (get_accumulation_rate (&accessor, [x,y]) * SECOND * 100) as f32,
          _ => ((accessor.now() - my_varying.last_change)*50000000000 / SECOND) as f32,
        };
        
        vertices.extend(&[Vertex {
                            location: [((x) as f32)/30.0 -1.0,((y) as f32)/30.0 -1.0],
                            ink: my_current_ink,
                          },
                          Vertex {
                            location: [((x + 1) as f32)/30.0 -1.0,((y) as f32)/30.0 -1.0],
                            ink: my_current_ink,
                          },
                          Vertex {
                            location: [((x) as f32)/30.0 -1.0,((y + 1) as f32)/30.0 -1.0],
                            ink: my_current_ink,
                          },
                          Vertex {
                            location: [((x + 1) as f32)/30.0 -1.0,((y + 1) as f32)/30.0 -1.0],
                            ink: my_current_ink,
                          },
                          Vertex {
                            location: [((x + 1) as f32)/30.0 -1.0,((y) as f32)/30.0 -1.0],
                            ink: my_current_ink,
                          },
                          Vertex {
                            location: [((x) as f32)/30.0 -1.0,((y + 1) as f32)/30.0 -1.0],
                            ink: my_current_ink,
                          }]);

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
