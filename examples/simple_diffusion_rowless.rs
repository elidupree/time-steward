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
use time_steward::rowless::api::{self, PersistentTypeId, PersistentlyIdentifiedType, ListedType, StewardData, QueryOffset, DataHandleTrait, DataTimelineCellTrait, ExtendedTime, Basics as BasicsTrait};
use time_steward::rowless::stewards::{simple_flat as steward_module};
use steward_module::{TimeSteward, ConstructibleTimeSteward, IncrementalTimeSteward, Event, DataHandle, DataTimelineCell, EventHandle, Accessor, EventAccessor, FutureCleanupAccessor, SnapshotAccessor, simple_timeline};
use simple_timeline::{SimpleTimeline, GetVarying, IterateUniquelyOwnedPredictions, tracking_query, modify_simple_timeline, unmodify_simple_timeline};


use time_steward::support::rounding_error_tolerant_math::Range;

/// i64 makes a good time type:
/// It's big enough to subdivide down to the nanosecond, allowing a smooth simulation,
/// while still representing any reasonable amount of time the simulation could take.
type Time = i64;
const SECOND: Time = 1i64 << 20;
const GENERIC_INK_AMOUNT: i64 = SECOND << 20;
const AMOUNT_DIFFERENCE_PER_TRANSFER_RATE: i64 = 2*SECOND;

type Steward = steward_module::Steward <Basics>;


/// A type defining simulation constants.
/// This obviously isn't needed for compile-time constants, which can be done normally,
/// but it's useful for things like game settings or commandline parameters.
#[derive (Clone, PartialEq, Eq, Serialize, Deserialize, Debug)]
struct Globals {
  size: [i32; 2],
  
  cells: Vec<Cell>,
}
impl StewardData for Globals {}

// Derive all the traits required for field data types.
#[derive (Clone, PartialEq, Eq, Serialize, Deserialize, Debug)]
struct Cell {
  varying: DataTimelineCell <SimpleTimeline <CellVarying, Steward>>,
  transfers: [DataTimelineCell <SimpleTimeline <TransferVarying, Steward>>; 2],
}
#[derive (Clone, PartialEq, Eq, Serialize, Deserialize, Debug)]
struct CellVarying {
  /// The exact amount of ink present in this cell at the last time we updated it.
  last_change: Time,
  ink_at_last_change: i64,
  fiat_accumulation_rate: i64,
  
  // cached values
  accumulation_rate: i64,
  total_neighbor_accumulation_rate: i64,
}
#[derive (Clone, PartialEq, Eq, Serialize, Deserialize, Debug)]
struct TransferVarying {
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
  rate: i64,
  next_change: Option <EventHandle <Basics>>,
}
impl StewardData for Cell {}
impl StewardData for CellVarying {}
impl StewardData for TransferVarying {}
impl IterateUniquelyOwnedPredictions <Steward> for CellVarying {}
impl IterateUniquelyOwnedPredictions <Steward> for TransferVarying {
  fn iterate_predictions <F: FnMut (& <Steward as TimeSteward>::EventHandle)> (&self, callback: &mut F) {
    if let Some (prediction) = self.next_change.as_ref() {
      callback (prediction);
    }
  }
}

fn ink_at (cell: &CellVarying, time: Time)->i64 {
  cell.ink_at_last_change + cell.accumulation_rate*(time - cell.last_change)
}

fn transfer_rate_for_amount_difference (amount_difference: i64)->i64 {
  amount_difference / AMOUNT_DIFFERENCE_PER_TRANSFER_RATE
}
fn possible_amount_differences_for_transfer_rate (transfer_rate: i64)->[i64;2] {
  match transfer_rate.signum() {
    -1 => [transfer_rate * AMOUNT_DIFFERENCE_PER_TRANSFER_RATE - AMOUNT_DIFFERENCE_PER_TRANSFER_RATE + 1,
           transfer_rate * AMOUNT_DIFFERENCE_PER_TRANSFER_RATE],
    0 => [- AMOUNT_DIFFERENCE_PER_TRANSFER_RATE + 1,
            AMOUNT_DIFFERENCE_PER_TRANSFER_RATE - 1],
    1 => [transfer_rate * AMOUNT_DIFFERENCE_PER_TRANSFER_RATE,
          transfer_rate * AMOUNT_DIFFERENCE_PER_TRANSFER_RATE + AMOUNT_DIFFERENCE_PER_TRANSFER_RATE - 1],
    _ => unreachable!(),
  }
}
fn time_ideal_transfer_rate_reaches (cells: [&CellVarying; 2], target_rate: i64, start: Time, direction: i64)->i64 {
  let closer_side_index = match direction {1=>0, -1=>1, _=>panic!()};
  let starting_difference = ink_at (cells [0], start) - ink_at (cells [1], start);
  let accumulation_difference = cells [0].accumulation_rate - cells [1].accumulation_rate;
  let target_difference = possible_amount_differences_for_transfer_rate (target_rate)[closer_side_index];
  assert!(accumulation_difference*direction > 0);
  if (target_difference - starting_difference)*direction <= 0 {return start}
  // algebra: starting_difference + time*accumulation_difference = target_difference
  // time = (target_difference - starting_difference) / accumulation_difference
  let result = ((target_difference - starting_difference) + (accumulation_difference-direction)) / accumulation_difference;
  //printlnerr!("{:?}", (target_difference, starting_difference, accumulation_difference, direction, result));
  assert!((starting_difference + result*accumulation_difference)*direction >= target_difference*direction);
  assert!((starting_difference + (result-1)*accumulation_difference)*direction < target_difference*direction);
  assert!(transfer_rate_for_amount_difference (starting_difference + result*accumulation_difference)*direction >= target_rate*direction);
  assert!(transfer_rate_for_amount_difference (starting_difference + (result-1)*accumulation_difference)*direction < target_rate*direction);
  start+result
}

fn desired_transfer_change_time (cells: [&CellVarying; 2], transfer: & TransferVarying, start: Time)->Option <Time> {
  //printlnerr!("{:?}", (start));
  let starting_difference = ink_at (cells [0], start) - ink_at (cells [1], start);
  let accumulation_difference = cells [0].accumulation_rate - cells [1].accumulation_rate;
  if accumulation_difference.signum() == 0 { return None }
  //assert!(transfer_rate_for_amount_difference (starting_difference)*accumulation_difference.signum() >= transfer.rate*accumulation_difference.signum(), "the simulation invariants were broken");
  
  // don't update before anything changes
  let min_time = time_ideal_transfer_rate_reaches (cells, transfer.rate + accumulation_difference.signum(), start, accumulation_difference.signum());
  
  // don't invert the accumulation difference all by yourself,
  // although we permit going one step too far in order to avoid having rounding error create a contradiction.
  let max_time_1 = time_ideal_transfer_rate_reaches (cells, transfer.rate + (accumulation_difference+accumulation_difference.signum())/2, start, accumulation_difference.signum());
  //assert!(max_time_1 >= min_time, "the simulation invariants were broken");
  let mut max_time = max_time_1;
  
  for which in 0..2 {
    let direction_signum = (which as i64*2)-1;
    let accumulation_scaled = cells[which].accumulation_rate*4;
    let current_difference_from_total = accumulation_scaled - cells [which].total_neighbor_accumulation_rate;
    if current_difference_from_total.signum() == -direction_signum*accumulation_difference.signum() {
      let limit = current_difference_from_total*-direction_signum/16+accumulation_difference.signum();
      assert!(limit.signum() == accumulation_difference.signum()) ;
      let max_time_2 = time_ideal_transfer_rate_reaches (cells, transfer.rate +limit, start, accumulation_difference.signum());
      if max_time_2-start  < SECOND/100 {printlnerr!("{:?}", ("problem", limit,max_time_2-start ));}
      max_time = min(max_time, max_time_2);
    }
  }
  
  max_time = max(max_time, min_time);
  
  Some(max_time)
}

fn update_cell <A: EventAccessor <Steward = Steward>> (accessor: &A, coordinates: [i32; 2]) {
  let me = get_cell (accessor, coordinates).unwrap();
  let mut my_varying = accessor.query (&me.varying, & GetVarying, QueryOffset::After).unwrap().1;
  //printlnerr!("{:?}", ("updating", my_varying.ink_at_last_change, my_varying.last_change, accessor.now().clone() - my_varying.last_change));
  my_varying.ink_at_last_change = ink_at (& my_varying, accessor.now().clone());
  my_varying.last_change = accessor.now().clone();
  modify_simple_timeline (accessor, & me.varying, Some(my_varying));
}
fn add_accumulation_rate <A: EventAccessor <Steward = Steward>> (accessor: &A, coordinates: [i32; 2], amount: i64)->bool {
  let me = get_cell (accessor, coordinates).unwrap();
  let mut my_varying = accessor.query (&me.varying, & GetVarying, QueryOffset::After).unwrap().1;
  let old_rate = my_varying.accumulation_rate;
  my_varying.accumulation_rate += amount;
  let new_rate = my_varying.accumulation_rate;
  modify_simple_timeline (accessor, & me.varying, Some(my_varying));
  let mut flipped_anything = false;
  for dimension in 0..2 {
    for direction in 0..2 {
      let mut coordinates1 = coordinates;
      coordinates1[dimension] -= (direction*2)-1;
      if in_bounds (accessor.globals(), coordinates1) {
        let neighbor = get_cell (accessor, coordinates1).unwrap();
        let mut neighbor_varying = accessor.query (&neighbor.varying, & GetVarying, QueryOffset::After).unwrap().1;
        if (neighbor_varying.accumulation_rate - old_rate).signum() !=
           (neighbor_varying.accumulation_rate - new_rate).signum() {
          flipped_anything = true;
        }
        neighbor_varying.total_neighbor_accumulation_rate += amount;
        modify_simple_timeline (accessor, & neighbor.varying, Some(neighbor_varying));
      }
    }
  }
  flipped_anything
}


fn update_transfer_change_prediction <A: EventAccessor <Steward = Steward>> (accessor: &A, coordinates: [i32; 2], dimension: usize) {
  if !in_bounds (accessor.globals(), coordinates) {return;}
  // This update is only in charge of the transfer to the cell
  // in the positive x and y directions. The other transfers will be handled by
  // the updates for the cells in the negative x and y directions.
  let mut neighbor_coordinates = coordinates;
  neighbor_coordinates [dimension] += 1;
  if neighbor_coordinates [dimension] >= accessor.globals().size [dimension] {return;}
  
  let cells = [
    get_cell (accessor, coordinates).unwrap(),
    get_cell (accessor, neighbor_coordinates).unwrap(),
  ];
  let varying = [
    accessor.query (&cells[0].varying, & GetVarying, QueryOffset::After).unwrap().1,
    accessor.query (&cells[1].varying, & GetVarying, QueryOffset::After).unwrap().1,
  ];
  
  let mut transfer = accessor.query (&cells[0].transfers [dimension], & GetVarying, QueryOffset::After).unwrap().1;
  
  let time = desired_transfer_change_time ([&varying[0], &varying[1]], & transfer, *accessor.now());
  
  if let Some (discarded) = transfer.next_change.take() {accessor.destroy_prediction (&discarded);}
  transfer.next_change = time.map (|time| {
    accessor.create_prediction (
        time,
        DeterministicRandomId::new (&(accessor.id(), coordinates, dimension)),
        TransferChange {coordinates: coordinates, dimension: dimension}
      )
  });
  
  modify_simple_timeline (accessor, & cells[0].transfers [dimension], Some(transfer));
}

#[derive (Default)]
struct EventContext {
  transfers_needing_update: Vec<([i32;2], usize)>
}

fn update_transfer <Accessor: EventAccessor <Steward = Steward>> (accessor: &Accessor, coordinates: [i32; 2], dimension: usize) {
  let mut neighbor_coordinates = coordinates;
  neighbor_coordinates [dimension] += 1;
  let other_dimension = (dimension + 1) & 1;
  
  update_cell (accessor, coordinates);
  update_cell (accessor, neighbor_coordinates);
  let me = get_cell (accessor, coordinates).unwrap();
  let my_varying = accessor.query (&me.varying, & GetVarying, QueryOffset::After).unwrap().1;
  let neighbor = get_cell (accessor, neighbor_coordinates).unwrap();
  let neighbor_varying = accessor.query (&neighbor.varying, & GetVarying, QueryOffset::After).unwrap().1;
  let mut transfer = accessor.query (&me.transfers [dimension], & GetVarying, QueryOffset::After).unwrap().1;
  
  let old_rate = transfer.rate;
  let new_rate = transfer_rate_for_amount_difference (my_varying.ink_at_last_change - neighbor_varying.ink_at_last_change);

  transfer.rate = new_rate;
  
  modify_simple_timeline (accessor, & me.transfers [dimension], Some(transfer));

  let mut transfers_needing_update: Vec<([i32;2], usize)> = Vec::new();
  if add_accumulation_rate (accessor, coordinates, -(new_rate - old_rate)) {
    printlnerr!("{:?}", ("recurse"));
    for dimension in 0..2 {
      for direction in 0..2 {
        let mut coordinates1 = coordinates;
        coordinates1[dimension] -= direction;
        transfers_needing_update.push ((coordinates1, dimension));
      }
    }
  }
  if add_accumulation_rate (accessor, neighbor_coordinates, new_rate - old_rate) {
    printlnerr!("{:?}", ("recurse1"));
    for dimension in 0..2 {
      for direction in 0..2 {
        let mut coordinates1 = neighbor_coordinates;
        coordinates1[dimension] -= direction;
        transfers_needing_update.push ((coordinates1, dimension));
      }
    }
  }
  
  for (coordinates, dimension) in transfers_needing_update {
    update_transfer (accessor, coordinates, dimension);
  }
  
    // if the other algorithms are right, we only need to update exactly the
    // seven transfers immediately adjacent to the two updated cells.
    let mut walking_coordinates = coordinates.clone();
    for offset in -1..2 {
      walking_coordinates[dimension] = coordinates[dimension] + offset;
      update_transfer_change_prediction (accessor, walking_coordinates, dimension);
    }
    for offsa in 0..2 {
      for offsb in -1..1 {
        walking_coordinates[dimension] = coordinates[dimension] + offsa;
        walking_coordinates[other_dimension] = coordinates[other_dimension] + offsb;
        update_transfer_change_prediction (accessor, walking_coordinates, other_dimension);
      }
    }
}


fn cell_index (globals: & Globals, coordinates: [i32; 2])->usize {
  (coordinates [0]*globals.size[0] + coordinates [1]) as usize
}
fn in_bounds (globals: & Globals, coordinates: [i32; 2])->bool {
  coordinates [0] >= 0 && coordinates [0] < globals.size [0] &&
  coordinates [1] >= 0 && coordinates [1] < globals.size [1]
}
fn get_cell <A: Accessor <Steward = Steward >> (accessor: &A, coordinates: [i32; 2])->Option <&Cell> {
  if in_bounds (accessor.globals(), coordinates) {
    Some(& accessor.globals().cells[cell_index (accessor.globals(), coordinates)])
  }
  else { None }
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
    //let (my_last_change, mut me) = query_cell (accessor, self.coordinates).expect("cell doesn't exist for TransferChange?");
    let other_dimension = (self.dimension + 1) & 1;
    let mut neighbor_coordinates = self.coordinates;
    neighbor_coordinates [self.dimension] += 1;
    
    update_transfer (accessor, self.coordinates, self.dimension);
    
    // TODO: invalidation
  }

  fn undo <Accessor: FutureCleanupAccessor <Steward = Self::Steward>> (&self, accessor: &mut Accessor, _: ()) {
    unimplemented!()
  }
}

#[derive (Clone, PartialEq, Eq, Serialize, Deserialize, Debug)]
struct AddInk {coordinates: [i32; 2], amount: i64, accumulation: i64}
impl StewardData for AddInk {}
impl PersistentlyIdentifiedType for AddInk {
  const ID: PersistentTypeId = PersistentTypeId(0x3e6d029c3da8b9a2);
}
impl Event for AddInk {
  type Steward = Steward;
  type ExecutionData = ();
  fn execute <Accessor: EventAccessor <Steward = Self::Steward>> (&self, accessor: &mut Accessor) {
    for offsx in -1..1 {
      for offsy in -1..1 {
        let coordinates = [self.coordinates [0] + offsx, self.coordinates [1] + offsy];
      }
    }{
    update_cell (accessor, self.coordinates);
    let me = get_cell (accessor, self.coordinates).unwrap();
    let mut my_varying = accessor.query (&me.varying, & GetVarying, QueryOffset::After).unwrap().1;
    my_varying.ink_at_last_change += self.amount;
    my_varying.fiat_accumulation_rate += self.accumulation;
    modify_simple_timeline (accessor, & me.varying, Some(my_varying));
    add_accumulation_rate (accessor, self.coordinates, self.accumulation);
    }
    for dimension in 0..2 {
      for direction in 0..2 {
        let mut coordinates = self.coordinates;
        coordinates[dimension] -= direction;
        update_transfer (accessor, coordinates, dimension);
      }
    }
    for dimension1 in 0..2 {
      for direction1 in 0..2 {
        let mut coordinates1 = self.coordinates;
        coordinates1[dimension1] -= (direction1*2)-1;
        for dimension in 0..2 {
          for direction in 0..2 {
            let mut coordinates = coordinates1;
            coordinates[dimension] -= direction;
            update_transfer_change_prediction (accessor, coordinates, dimension);
          }
        }
      }
    }
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
          accumulation_rate: 0,
          total_neighbor_accumulation_rate: 0, 
        }));
        for dimension in 0..2 {
          modify_simple_timeline (accessor, & cell.transfers [dimension], Some(TransferVarying {
            rate: 0,
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

fn make_globals()-> Globals {
  let mut cells = Vec::new();
  for index in 0..60*60 {
    cells.push (Cell{
      varying: DataTimelineCell::new (SimpleTimeline::new ()),
      transfers: [
        DataTimelineCell::new (SimpleTimeline::new ()),
        DataTimelineCell::new (SimpleTimeline::new ()),
      ],
    });
  }
  Globals {
    size: [60, 60],
    cells: cells,
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
  let globals = make_globals();

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
    let mut steward: Steward = Steward::from_globals(globals);
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
gl_FragColor = vec4 (vec3(0.5 - ink_transfer/2.0), 1.0);
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
          mouse_coordinates [0] = (x as i32) * 60 / display.get_window().unwrap().get_inner_size_pixels().unwrap().0 as i32;
          mouse_coordinates [1] = (display.get_window().unwrap().get_inner_size_pixels().unwrap().1 as i32-(y as i32)) * 60 / display.get_window().unwrap().get_inner_size_pixels().unwrap().1 as i32;
        },
        glium::glutin::Event::MouseInput (_,_) => {
          if in_bounds (globals, mouse_coordinates) {
            event_index += 1;
            stew.insert_fiat_event (time, DeterministicRandomId::new (& event_index), AddInk {
              coordinates: [mouse_coordinates [0], mouse_coordinates [1]],
              amount: ((input_derivative+1)%2)*(GENERIC_INK_AMOUNT << input_magnitude_shift)*input_signum,
              accumulation: input_derivative*(GENERIC_INK_AMOUNT << input_magnitude_shift)*input_signum/SECOND,
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
            amount: ((input_derivative+1)%2)*(GENERIC_INK_AMOUNT << input_magnitude_shift)*input_signum,
            accumulation: input_derivative*(GENERIC_INK_AMOUNT << input_magnitude_shift)*input_signum/SECOND,
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
          0 => ink_at (&my_varying, *accessor.now()) as f32 / GENERIC_INK_AMOUNT as f32,
          1 => (my_varying.accumulation_rate * SECOND * 100) as f32 / GENERIC_INK_AMOUNT as f32,
          _ => ((accessor.now() - my_varying.last_change) / SECOND) as f32,
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
