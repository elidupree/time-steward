// In this example, we're going to make a simple diffusion simulation:
// a two-dimensional grid of cells, each containing some amount of ink.
// Ink diffuses from cells with more ink the cells with less ink.

// All field data types must implement serde::Serialize and serde::Deserialize,
// so we include the features for deriving them.
extern crate serde;
#[macro_use]
extern crate serde_derive;

#[macro_use]
extern crate time_steward;

// Imports for the UI
#[macro_use]
extern crate glium;
extern crate docopt;
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
use time_steward::rowless::api::{self, StewardData, QueryOffset, TypedDataTimelineHandleTrait, ExtendedTime, Basics as BasicsTrait};
use time_steward::rowless::stewards::{simple_flat as steward_module};
use steward_module::{TimeSteward, ConstructibleTimeSteward, IncrementalTimeSteward, Event, DataTimelineHandle, PredictionHandle, Accessor, MomentaryAccessor, EventAccessor, UndoEventAccessor, SnapshotAccessor, simple_timeline};
use simple_timeline::{SimpleTimeline, ConstantTimeline, GetConstant, GetVarying, tracking_query, modify_simple_timeline, unmodify_simple_timeline};


use time_steward::support::rounding_error_tolerant_math::Range;

/// i64 makes a good time type:
/// It's big enough to subdivide down to the nanosecond, allowing a smooth simulation,
/// while still representing any reasonable amount of time the simulation could take.
type Time = i64;
const SECOND: Time = 1i64 << 20;

type Steward = steward_module::Steward <Basics>;


/// A type defining simulation constants.
/// This obviously isn't needed for compile-time constants, which can be done normally,
/// but it's useful for things like game settings or commandline parameters.
#[derive (Clone, PartialEq, Eq, Serialize, Deserialize, Debug)]
struct Globals {
  size: [i32; 2],
  
  /// Maximum inaccuracy of transfers in ink-per-update
  max_inaccuracy: i64,
  
  cells: Vec<CellHandle>,
}
impl StewardData for Globals {}

// Derive all the traits required for field data types.
#[derive (Clone, PartialEq, Eq, Serialize, Deserialize, Debug)]
struct Cell {
  coordinates: [i32; 2],
  
  /// The exact amount of ink present in this cell at the last time we updated it.
  ink_at_last_change: i64,
  
  /// The rate at which this cell is currently transferring ink to other cells.
  /// 
  /// The mathematically accurate transfer function between just 2 cells would be
  /// exponential decay, but it would be quickly complicated by other cells anyway. 
  /// Instead, we use a linear approximation, and just update it whenever
  /// it becomes too inaccurate.
  /// 
  /// The values here represent the transfer TOWARDS the cells in the POSITIVE
  /// x and y directions. Transfers from the cells in the negative x and y directions
  /// are stored in the data of *those* cells.
  ink_transfers: [i64; 2],
  transfer_change_times: [Time; 2],
  
  next_transfer_change: Option <PredictionHandle <TransferChange>>,
}
impl StewardData for Cell {}
type CellHandle = DataTimelineHandle <SimpleTimeline <(), Cell, Basics>>;


fn update_transfer_change_prediction <A: EventAccessor <Steward = Steward>> (accessor: &mut A, globals: & Globals, coordinates: [i32; 2]) {
  if !in_bounds (globals, coordinates) {return;}
  let (my_last_change, mut me) = query_cell (accessor, globals, coordinates).unwrap();
  let my_accumulation_rate: i64 = get_accumulation_rate (accessor, globals, coordinates);
  
  if let Some (discarded) = me.next_transfer_change.take() {
    accessor.destroy_prediction (&discarded);
  }
  
  let mut best: Option <(Time, usize)> = None;
  
  for dimension in 0..2 {
    // This update is only in charge of the transfers to the cells
    // in the positive x and y directions. The other transfers will be handled by
    // the updates for the cells in the negative x and y directions.
    let mut neighbor_coordinates = coordinates;
    neighbor_coordinates [dimension] += 1;
    if neighbor_coordinates [dimension] >= globals.size [dimension] {continue;}
    let neighbor_accumulation_rate = get_accumulation_rate (accessor, globals, neighbor_coordinates);
    let (neighbor_last_change, neighbor) = query_cell (accessor, globals, neighbor_coordinates).unwrap();
    
    let last_change = me.transfer_change_times [dimension];
    let current_difference =
      (me.ink_at_last_change + my_accumulation_rate*(last_change - my_last_change)) -
      (neighbor.ink_at_last_change + neighbor_accumulation_rate*(last_change - neighbor_last_change));
    let current_difference_change_rate = my_accumulation_rate - neighbor_accumulation_rate;
    let current_transfer_rate = me.ink_transfers [dimension];
    
    //let permissible_cumulative_error = globals.max_inaccuracy;
    //let permissible_cumulative_error = 8 + Range::exactly (current_difference.abs()).sqrt().unwrap().max();
    // if we're already fairly stable, require smaller error to avoid drift
    // let permissible_cumulative_error = 8 + Range::exactly (current_difference_change_rate.abs()).sqrt().unwrap().max()<<20;
    let permissible_cumulative_error = 16 + (current_difference.abs() >> 4);
    
    // We choose the target transfer rate to be the amount that would
    // equalize the two cells in one second.
    // That is, the target transfer rate is (my ink - other ink)/(SECOND*2).
    //
    // Cumulative error = integral_0^t (actual transfer rate - ideal transfer rate)
    // = integral_0^t (actual transfer rate) - integral_0^t (ideal transfer rate)
    // = t*(actual transfer rate) - integral_0^t (ideal transfer rate)
    //
    // ideal transfer rate(t) = (my ink - other ink(t))/(SECOND*2)
    // = (current_difference + t*current_difference_change_rate)/(SECOND*2)
    // (well… Sort of. current_difference_change_rate values can change when other transfers change,
    //   making this formula behave somewhat weirdly)
    // 
    // so Cumulative error =
    // = t*(actual transfer rate) - integral_0^t (ideal transfer rate)
    // = t*(actual transfer rate) - integral_0^t ((current_difference + t*current_difference_change_rate)/(SECOND*2))
    // = t*(actual transfer rate - current_difference/(SECOND*2)) - integral_0^t ((t*current_difference_change_rate)/(SECOND*2))
    // = t*(actual transfer rate - current_difference/(SECOND*2)) - (0.5t^2)*current_difference_change_rate/(SECOND*2)
    //
    // set = to max_inaccuracy, and
    // multiply everything by (SECOND*2*2) to reduce rounding:
    // 0 = t^2(current_difference_change_rate) + t*2(actual transfer rate*SECOND*2 - current_difference) +/- max_inaccuracy*SECOND*2*2
    
    let a = current_difference_change_rate;
    let b = current_transfer_rate*(SECOND*4) - current_difference*2;
    let c = permissible_cumulative_error*(SECOND*4);
    // quadratic formula: t = (-b +/- \sqrt(b^2-4ac)) / 2a
    // if it's currently going up, we want the first result for positive inaccuracy and the second for negative inaccuracy, and vice versa
    let time;
    if a == 0 {
      if b == 0 {
        time = None;
      }
      else {
        time = Some ((c/b).abs());
      }
    }
    else {
      let sign_a = if a <0 {-1} else {1};
      if b == 0 {
        let discriminant = Range::exactly (4)*a*c*sign_a;
        time = Some ((discriminant.sqrt().unwrap()*sign_a).min() / (2*a));
      }
      else {
        let sign_b = if b <0 {-1} else {1};
        let direct_discriminant_sqrt = if sign_a == sign_b {None} else {(Range::exactly (b)*b-Range::exactly (4)*a*c*sign_a).sqrt()};
        if let Some(square_root) = direct_discriminant_sqrt {
          time = Some (max(0, (-b - (square_root*sign_a).max()) / (2*a)));
        }
        else {
          let later_discriminant = Range::exactly (b)*b-Range::exactly (-4)*a*c*sign_a;
          time = Some ((-b + (later_discriminant.sqrt().unwrap()*sign_a).max()) / (2*a));
        }
      }
    }
    //printlnerr!( "predict {} {} {} {:?}",a,b,c,time);
    
    // 
    // We need to notice when the target transfer rate goes outside of the range
    // [current transfer rate - maximum inaccuracy, current transfer rate + maximum inaccuracy].
    // After a little algebra...
    /*let (min_difference, target_difference, max_difference) = (
      (current_transfer_rate - globals.max_inaccuracy)*(2*SECOND),
      (current_transfer_rate                         )*(2*SECOND),
      (current_transfer_rate + globals.max_inaccuracy)*(2*SECOND)
    );
    if current_difference < min_difference || current_difference > max_difference {
      printlnerr!( "predict wow! {:?} ! {:?}", me, neighbor);
      best = Some((last_change, dimension));
    }
    else if current_difference_change_rate > 0 {
      //printlnerr!( "predict {}/{}",max_difference-current_difference, current_difference_change_rate);
      let time = min (
        me.transfer_change_times [dimension] + SECOND/4,
        last_change + (max_difference-current_difference)/current_difference_change_rate
      );
      if best.map_or (true, | previous | time <previous.0) {best = Some((time, dimension));}
    }
    else if current_difference_change_rate < 0 {
      //printlnerr!( "predict {}/{}",min_difference-current_difference, current_difference_change_rate);
      let time = min (
        me.transfer_change_times [dimension] + SECOND/4,
        last_change + (min_difference-current_difference)/current_difference_change_rate
      );
      if best.map_or (true, | previous | time <previous.0) {best = Some((time, dimension));}
    }*/
    if let Some (time) = time {
      assert!(time >= 0);
      let time = last_change + time;
      if best.map_or (true, | previous | time <previous.0) {best = Some((max (*accessor.now(),time) , dimension));}
    }
  }
  
  let now = accessor.extended_now().clone();
  
  me.next_transfer_change = best.map (|(time, dimension) | {
    assert!(time >= my_last_change) ;
    assert!(time >= now.base) ;
    accessor.create_prediction (
        time,
        DeterministicRandomId::new (&(now.id, coordinates)),
        TransferChange {coordinates: coordinates, dimension: dimension}
      )
  });
  
  // Hack: we only need to modify this because overwriting the cell in order to overwrite it prediction
  // also updates the last change time for the cell as a whole
  me.ink_at_last_change += my_accumulation_rate*(now.base - my_last_change);
  
  modify_cell (accessor, globals, coordinates, me) ;
}

/// A utility function used above. Gets the current rate of change of ink in a cell,
/// by summing up the current transfer rates.
/// 
/// Since this function doesn't make predictions, it only needs to require trait Accessor,
/// which is a supertrait of EventAccessor. Thus, it could also be used in Events,
/// and with Snapshots, if needed.
fn get_accumulation_rate <A: Accessor <Steward = Steward >> (accessor: &A, globals: & Globals, coordinates: [i32; 2])->i64 {
  let mut result = 0;
  let me: Cell = query_cell (accessor, globals, coordinates).unwrap().1;  
  for dimension in 0..2 {
    result -= me.ink_transfers [dimension];
    
    let mut neighbor_coordinates = coordinates;
    neighbor_coordinates [dimension] -= 1;
    
    // Adjacent cells might NOT exist (they could be out of bounds).
    // We could also have just done a bounds check on the coordinates, like above.
    if let Some (neighbor) = query_cell (accessor, globals, neighbor_coordinates) {
      result += neighbor.1.ink_transfers [dimension];
    }
  }
  result
}

fn cell_index (globals: & Globals, coordinates: [i32; 2])->usize {
  (coordinates [0]*globals.size[0] + coordinates [1]) as usize
}
fn in_bounds (globals: & Globals, coordinates: [i32; 2])->bool {
  coordinates [0] >= 0 && coordinates [0] < globals.size [0] &&
  coordinates [1] >= 0 && coordinates [1] < globals.size [1]
}
fn query_cell <A: Accessor <Steward = Steward >> (accessor: &A, globals: & Globals, coordinates: [i32; 2])->Option <(Time, Cell)> {
  if in_bounds (globals, coordinates) {
    let result = accessor.query (& globals.cells[cell_index (globals, coordinates)], & GetVarying, QueryOffset::After).expect ("cells should never not exist");
    Some((result.0.base, result.1))
  }
  else { None }
}
fn modify_cell <A: EventAccessor <Steward = Steward >> (accessor: &A, globals: & Globals, coordinates: [i32; 2], value: Cell) {
  modify_simple_timeline (accessor, & globals.cells [cell_index (globals, coordinates)], Some(value));
}

/// The TransferChange event, as used above.
#[derive (Clone, PartialEq, Eq, Serialize, Deserialize, Debug)]
struct TransferChange {coordinates: [i32; 2], dimension: usize}
impl StewardData for TransferChange {}
impl Event for TransferChange {
  type Steward = Steward;
  type ExecutionData = ();
  fn execute <Accessor: EventAccessor <Steward = Self::Steward>> (&self, accessor: &mut Accessor) {
    let globals = accessor.query (accessor.global_timeline(), & GetConstant, QueryOffset::After);
    let (my_last_change, mut me) = query_cell (accessor, & globals, self.coordinates).expect("cell doesn't exist for TransferChange?");
    let mut neighbor_coordinates = self.coordinates;
    neighbor_coordinates [self.dimension] += 1;
    let (neighbor_last_change, mut neighbor) = query_cell (accessor, & globals, neighbor_coordinates).expect("neighbor doesn't exist for TransferChange?");
    
    let my_current_ink = me.ink_at_last_change + get_accumulation_rate (accessor, & globals, self.coordinates)*(accessor.now() - my_last_change);
    let neighbor_current_ink = neighbor.ink_at_last_change + get_accumulation_rate (accessor, & globals, neighbor_coordinates)*(accessor.now() - neighbor_last_change);
    let current_difference = my_current_ink - neighbor_current_ink;
 
    me.ink_at_last_change = my_current_ink;
    neighbor.ink_at_last_change = neighbor_current_ink;
    me.ink_transfers [self.dimension] = current_difference/(2*SECOND);
    me.transfer_change_times [self.dimension] = *accessor.now() ;
    
    modify_cell (accessor, &globals, self.coordinates, me) ;
    modify_cell (accessor, &globals, neighbor_coordinates, neighbor) ;

    // TODO: some of the ones at the corners don't need to be updated
    for offsx in -1.. (if self.dimension == 0 {2} else {1}) {
      for offsy in -1.. (if self.dimension == 1 {2} else {1}) {
        let coordinates = [self.coordinates [0] + offsx, self.coordinates [1] + offsy];
        update_transfer_change_prediction (accessor, & globals, coordinates) ;
      }
    }
    
    
    // TODO: invalidation
  }

  fn undo <Accessor: UndoEventAccessor <Steward = Self::Steward>> (&self, accessor: &mut Accessor, _: ()) {
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
impl Event for Initialize {
  type Steward = Steward;
  type ExecutionData = ();
  fn execute <Accessor: EventAccessor <Steward = Self::Steward>> (&self, accessor: &mut Accessor) {
    let globals = accessor.query (accessor.global_timeline(), & GetConstant, QueryOffset::After);
    for x in 0..globals.size [0] {
      for y in 0..globals.size [1] {
        modify_cell (accessor, &globals, [x,y], Cell {
          coordinates: [x, y],
          ink_at_last_change: 0,
          ink_transfers: [0, 0],
          transfer_change_times: [0, 0],
          next_transfer_change: None,
        });
      }
    }
  }
  fn undo <Accessor: UndoEventAccessor <Steward = Self::Steward>> (&self, accessor: &mut Accessor, _: ()) {
    unimplemented!()
  }
}
#[derive (Clone, PartialEq, Eq, Serialize, Deserialize, Debug)]
struct AddInk {coordinates: [i32; 2], amount: i64}
impl StewardData for AddInk {}
impl Event for AddInk {
  type Steward = Steward;
  type ExecutionData = ();
  fn execute <Accessor: EventAccessor <Steward = Self::Steward>> (&self, accessor: &mut Accessor) {
    let globals = accessor.query (accessor.global_timeline(), & GetConstant, QueryOffset::After);
    let (my_last_change, mut me) = query_cell (accessor, & globals, self.coordinates).expect("cell doesn't exist for AddInk?");
    let my_current_ink = me.ink_at_last_change + get_accumulation_rate (accessor, & globals, self.coordinates)*(accessor.now() - my_last_change);
    me.ink_at_last_change = my_current_ink + self.amount;
    modify_cell (accessor, & globals, self.coordinates, me) ;
    // TODO: some of the ones at the corners don't need to be updated
    for offsx in -1..1 {
      for offsy in -1..1 {
        let coordinates = [self.coordinates [0] + offsx, self.coordinates [1] + offsy];
        update_transfer_change_prediction (accessor, & globals, coordinates) ;
      }
    }
  }
  fn undo <Accessor: UndoEventAccessor <Steward = Self::Steward>> (&self, accessor: &mut Accessor, _: ()) {
    unimplemented!()
  }
}

fn make_global_timeline()-> <Basics as BasicsTrait>::GlobalTimeline {
  <Basics as BasicsTrait>::GlobalTimeline::new ({
    let mut cells = Vec::new();
    for index in 0..60*60 {
      cells.push (DataTimelineHandle::new (SimpleTimeline::new (())));
    }
    Globals {
      size: [60, 60],
      max_inaccuracy: 1 << 30,
      cells: cells,
    }
  })
}

/// Finally, define the Basics type.
#[derive (Copy, Clone, Hash, PartialEq, Eq, PartialOrd, Ord, Serialize, Deserialize, Debug, Default)]
struct Basics {}
impl BasicsTrait for Basics {
  type Time = Time;
  type GlobalTimeline = ConstantTimeline <Globals, Basics>;
}


// User interface details.
use docopt::Docopt;

const USAGE: &'static str = "
Simple diffusion, a TimeSteward usage example.

Usage:
  simple_diffusion
  simple_diffusion (-l | --listen) <host> <port>
  simple_diffusion (-c | --connect) <host> <port>
  
Options:
  -l, --listen   Start a synchronized simulation by listening for TCP connections.
  -c, --connect  Start a synchronized simulation by making a TCP connection.
";

#[derive(Debug, Deserialize)]
struct Args {
  flag_listen: bool,
  flag_connect: bool,
    
  arg_host: Option <String>,
  arg_port: Option <u16>,
}

use std::time::{Instant, Duration};
use glium::{DisplayBuild, Surface};

#[derive(Copy, Clone)]
struct Vertex {
  location: [f32; 2],
  ink: f32,
}
implement_vertex!(Vertex, location, ink);

use std::net::{TcpListener, TcpStream};
use std::io::{BufReader, BufWriter};

fn main() {
  let global_timeline = make_global_timeline();

  // For some reason, docopt checking the arguments caused build_glium() to fail in emscripten.
  /*if !cfg!(target_os = "emscripten") {
    let arguments: Args = Docopt::new(USAGE)
                            .and_then(|d| d.deserialize())
                            .unwrap_or_else(|e| e.exit());
    
    if arguments.flag_listen {
      let listener = TcpListener::bind ((arguments.arg_host.as_ref().map_or("localhost", | string | string as & str), arguments.arg_port.unwrap())).unwrap();
      let stream = listener.accept().unwrap().0;
      let mut steward: simply_synchronized::Steward<Basics, DefaultSteward <Basics>> = simply_synchronized::Steward::new(DeterministicRandomId::new (& 0u32), 0, SECOND>>3, constants, BufReader::new (stream.try_clone().unwrap()), BufWriter::new (stream));
      steward.insert_fiat_event(0, DeterministicRandomId::new(&0), Initialize::new()).unwrap();
      run (steward, |a,b| (a.settle_before (b)));
      return;
    }
    else if arguments.flag_connect {
      let stream = TcpStream::connect ((arguments.arg_host.as_ref().map_or("localhost", | string | string as & str), arguments.arg_port.unwrap())).unwrap();
      let steward: simply_synchronized::Steward<Basics, DefaultSteward <Basics>> = simply_synchronized::Steward::new(DeterministicRandomId::new (& 1u32), 0, SECOND>>3, constants, BufReader::new (stream.try_clone().unwrap()), BufWriter::new (stream));
      run (steward, |a,b| (a.settle_before (b)));
      return;
    }
  }*/
  {
    let mut steward: Steward = Steward::from_global_timeline(global_timeline);
    steward.insert_fiat_event(0, DeterministicRandomId::new(&0), Initialize{}).unwrap();
    run (steward, |_,_|());
  }
}


fn run <F: Fn (&mut Steward, Time)>(mut stew: Steward, settle:F) {


  let vertex_shader_source = r#"
#version 100
attribute lowp vec2 location;
attribute lowp float ink;
varying lowp float ink_transfer;

void main() {
gl_Position = vec4 (location, 0.0, 1.0);
ink_transfer = ink;
}

"#;

  let fragment_shader_source = r#"
#version 100
varying lowp float ink_transfer;

void main() {
gl_FragColor = vec4 (vec3(0.5 - ink_transfer/100000000000.0), 1.0);
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
  let start = Instant::now();
  let mut previous_time = 0;

  let frame = || {
    let frame_begin = Instant::now();
    let time = min (previous_time + SECOND/10, 1 + ((start.elapsed().as_secs() as i64 * 1000000000i64) +
                          start.elapsed().subsec_nanos() as i64) *
                         SECOND / 1000000000i64);
    previous_time = time;
    
    let accessor = stew.snapshot_before(& time)
      .expect("steward failed to provide snapshot");
    stew.forget_before (& time);
    settle (&mut stew, time);
    let globals = accessor.query (accessor.global_timeline(), & GetConstant, QueryOffset::After);
    
    for ev in display.poll_events() {
      match ev {
        glium::glutin::Event::Closed => return true,
        glium::glutin::Event::MouseMoved (x,y) => {
          mouse_coordinates [0] = (x as i32) * 60 / display.get_window().unwrap().get_inner_size_pixels().unwrap().0 as i32;
          mouse_coordinates [1] = (display.get_window().unwrap().get_inner_size_pixels().unwrap().1 as i32-(y as i32)) * 60 / display.get_window().unwrap().get_inner_size_pixels().unwrap().1 as i32;
        },
        glium::glutin::Event::MouseInput (_,_) => {
          if in_bounds (&globals, mouse_coordinates) {
            event_index += 1;
            stew.insert_fiat_event (time, DeterministicRandomId::new (& event_index), AddInk {
              coordinates: [mouse_coordinates [0], mouse_coordinates [1]],
              amount: (DeterministicRandomId::new (& event_index).data() [0] & ((1u64<<40)-1)) as i64 - (1<<39)
            }).unwrap();
          }
        },
        _ => (),
      }
    }
    while let Some ((x,y)) = emscripten_compatibility::pop_click() {
      // TODO duplicate code
      mouse_coordinates [0] = (x*60.0) as i32;
      mouse_coordinates [1] = ((1.0-y)*60.0) as i32;
      event_index += 1;
      stew.insert_fiat_event (time, DeterministicRandomId::new (& event_index), AddInk {
            coordinates: [mouse_coordinates [0], mouse_coordinates [1]],
            amount: (DeterministicRandomId::new (& event_index).data() [0] & ((1u64<<40)-1)) as i64 - (1<<39)
          }).unwrap();
    }

    let mut target = display.draw();
    target.clear_color(1.0, 1.0, 1.0, 1.0);
    let mut vertices = Vec::<Vertex>::new();
    
    for x in 0.. globals.size [0] {
      for y in 0.. globals.size [1] {
        let (my_last_change, me) = query_cell (& accessor, & globals, [x,y]).unwrap();
        let my_current_ink = (me.ink_at_last_change + get_accumulation_rate (& accessor, & globals, me.coordinates)*(accessor.now() - my_last_change)) as f32;
        
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
      
    /*while frame_begin.elapsed() < Duration::from_millis (10) && stew.updated_until_before().map_or (false, | limitation | limitation < time + SECOND) {
        for _ in 0..8 {stew.step();}
    }*/
    false
  };
  
  emscripten_compatibility::main_loop(frame);
}
