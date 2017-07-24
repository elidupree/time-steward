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

extern crate fnv;

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
use time_steward::{DefaultSteward, Accessor, MomentaryAccessor, PredictorAccessor, Mutator, DeterministicRandomId, TimeSteward, TimeStewardFromConstants, IncrementalTimeSteward, Column, ColumnId, RowId, EventId, PredictorId, ColumnType, PredictorType, EventType};
use time_steward::stewards::simply_synchronized;


/// i64 makes a good time type:
/// It's big enough to subdivide down to the nanosecond, allowing a smooth simulation,
/// while still representing any reasonable amount of time the simulation could take.
type Time = i64;
const SECOND: Time = 1i64 << 20;


/// A type defining simulation constants.
/// This obviously isn't needed for compile-time constants, which can be done normally,
/// but it's useful for things like game settings or commandline parameters.
#[derive (Copy, Clone, PartialEq, Eq, Serialize, Deserialize, Debug)]
struct Constants {
  size: [i32; 2],
  
  /// Maximum inaccuracy of transfers in ink-per-time-unit
  max_inaccuracy: i64,
}


// Derive all the traits required for field data types.
#[derive (Copy, Clone, PartialEq, Eq, Serialize, Deserialize, Debug)]
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
}

/// impl the trait Column, to identify Cell as something that
/// can be stored in a TimeSteward field.
impl Column for Cell {
  /// Column Cell has the field type Cell.
  /// The *Column* type is essentially just the name. The *Field* type is the data stored.
  /// Thus, you can have multiple Columns with the same data type,
  /// and you can have a Column with a data type from a different crate.
  /// In this case, we don't need to do that, so we conveniently just name it after itself.
  type FieldType = Self;
  
  fn column_id()->ColumnId {
    // A randomly generated id for this column.
    // For your own code, generate a new random id.
    // The documentation provides an easy way to do this.
    ColumnId(0x66484c24cbec1484)
  }
}

/// To be able to respond when transfers have become too inaccurate,
/// we need a Predictor.
///
/// This macro creates a new unit struct that can be given to a TimeSteward.
time_steward_predictor!{
  struct TransferChangePredictor,
  
  // Basics is a type that we will define later, which is used to inform TimeSteward
  // of all the types we will use with it (including TransferChangePredictor itself).
  // In C++, this would create a mess of forward-declarations, but in Rust, it's trivial.
  Basics,
  
  // A random id for this predictor, generated using the documentation.
  PredictorId(0x364a67f059741d7f),
  
  // TransferChangePredictor 'watches' Cell.
  watching Cell,
  
  // This parameter identifies the predictor function itself.
  // You can also write the function in the macro directly, as a pseudo-closure:
  // |accessor, id| { ...code... }
  // But for complex predictors, it's generally better to use a free-standing
  // function, because it is easier to debug and profile.
  fn transfer_change_predictor
}

fn transfer_change_predictor <PA: PredictorAccessor <Basics = Basics>> (accessor: &mut PA, id: RowId) {
  // The Predictor is *always* called with the idea of an existent Cell, so
  // we can safely get and unwrap it.
  // To get field data, we call in Accessor method, which takes the Column type
  // (as a generic parameter) and the RowId (as a function argument).
  let (me, my_last_change) = accessor.data_and_last_change::<Cell>(id).unwrap();
  let my_accumulation_rate: i64 = get_accumulation_rate (accessor, me.coordinates);
  for dimension in 0..2 {
    // This call to the Predictor is only in charge of the transfers to the cells
    // in the positive x and y directions. The other transfers will be handled by
    // the Predictor calls for the cells in the negative x and y directions.
    let mut neighbor_coordinates = me.coordinates;
    neighbor_coordinates [dimension] += 1;
    if neighbor_coordinates [dimension] >= accessor.constants().size [dimension] {continue;}
    let neighbor_id = cell_id(neighbor_coordinates);
    let neighbor_accumulation_rate = get_accumulation_rate (accessor, neighbor_coordinates);
    let (neighbor, neighbor_last_change) = accessor.data_and_last_change::<Cell>(neighbor_id).unwrap();
    
    let last_change = max (my_last_change, neighbor_last_change);
    let current_difference =
      (me.ink_at_last_change + my_accumulation_rate*(last_change - my_last_change)) -
      (neighbor.ink_at_last_change + neighbor_accumulation_rate*(last_change - neighbor_last_change));
    let current_difference_change_rate = my_accumulation_rate - neighbor_accumulation_rate;
    let current_transfer_rate = me.ink_transfers [dimension];
    
    // We choose the target transfer rate to be the amount that would
    // equalize the two cells in one second.
    // That is, the target transfer rate is (my ink - other ink)/(SECOND*2).
    // 
    // We need to notice when the target transfer rate goes outside of the range
    // [current transfer rate - maximum inaccuracy, current transfer rate + maximum inaccuracy].
    // After a little algebra...
    let (min_difference, max_difference) = (
      (current_transfer_rate - accessor.constants().max_inaccuracy)*(2*SECOND),
      (current_transfer_rate + accessor.constants().max_inaccuracy)*(2*SECOND)
    );
    if current_difference < min_difference || current_difference > max_difference {
      printlnerr!( "predict wow! {:?} ! {:?}", me, neighbor);
      accessor.predict_at_time (*last_change, TransferChange::new (id, dimension));
    }
    else if current_difference_change_rate > 0 {
      //printlnerr!( "predict {}/{}",max_difference-current_difference, current_difference_change_rate);
      accessor.predict_at_time (
        last_change + min (SECOND/4, (max_difference-current_difference)/current_difference_change_rate),
        TransferChange::new (id, dimension)
      );
    }
    else if current_difference_change_rate < 0 {
      //printlnerr!( "predict {}/{}",min_difference-current_difference, current_difference_change_rate);
      
      accessor.predict_at_time (
        last_change + min (SECOND/4, (min_difference-current_difference)/current_difference_change_rate),
        TransferChange::new (id, dimension)
      );
    }
  }
}

/// A utility function used above. Gets the current rate of change of ink in a cell,
/// by summing up the current transfer rates.
/// 
/// Since this function doesn't make predictions, it only needs to require trait Accessor,
/// which is a supertrait of PredictorAccessor. Thus, it could also be used in Events,
/// and with Snapshots, if needed.
fn get_accumulation_rate <A: Accessor <Basics = Basics>> (accessor: &A, coordinates: [i32; 2])->i64 {
  let mut result = 0;
  let me: &Cell = accessor.get::<Cell>(cell_id(coordinates)).unwrap();  
  for dimension in 0..2 {
    result -= me.ink_transfers [dimension];
    
    let mut neighbor_coordinates = me.coordinates;
    neighbor_coordinates [dimension] -= 1;
    
    // Adjacent cells might NOT exist (they could be out of bounds).
    // We could also have just done a bounds check on the coordinates, like above.
    if let Some (neighbor) = accessor.get::<Cell>(cell_id(neighbor_coordinates)) {
      result += neighbor.ink_transfers [dimension];
    }
  }
  result
}

/// Tragically, RowId::new() is actually slower than caching the ids and looking them up using a faster hash function.
fn cell_id (coordinates: [i32; 2])->RowId {
  use fnv::FnvHashMap;
  use std::cell::RefCell;
  thread_local! {
    static CACHE: RefCell<FnvHashMap <[i32; 2], RowId>> = Default::default();
  }
  CACHE.with (|map| map.borrow_mut().entry (coordinates).or_insert_with (|| RowId::new(&coordinates)).clone())
}


/// The TransferChange event, as used above.
///
/// This is much like the time_steward_predictor! macro,
/// but it creates a struct that *does* contain data –
/// essentially, a glorified function object.
///
/// The macro automatically creates a new() function that takes
/// the fields in order. You may also use {} initialization if you prefer.
time_steward_event!{
  struct TransferChange {id: RowId, dimension: usize},
  
  // The other metadata is much like for predictors, just without the "watching" parameter.
  Basics,
  EventId(0x734af0602dc924f4),
  
  // Like with predictors, the event function can also be written as a pseudo-closure:
  // |&self, mutator| { ...code... }
  fn transfer_change
}

fn transfer_change <M: Mutator <Basics = Basics>> (mutator: &mut M, data: &TransferChange) {
  // trait Mutator also inherits from trait Accessor, so we can use most of
  // the same methods on it.
  //
  // This call to unwrap() is just as safe as the one from the Predictor,
  // because if the cell was deleted, the prediction would become invalid
  // and so this event would not be called.
  //
  // This time, we need to clone, so that we're not still holding an
  // immutable reference into the mutator when we try to
  // call the mutable method set().
  let mut me;
  let neighbor_id;
  let mut neighbor;
  {
    let (me_ref, my_last_change) = mutator.data_and_last_change::<Cell>(data.id).unwrap();
    me = me_ref.clone();
    let mut neighbor_coordinates = me.coordinates;
    neighbor_coordinates [data.dimension] += 1;
    neighbor_id = cell_id(neighbor_coordinates);
    let (neighbor_ref, neighbor_last_change) = mutator.data_and_last_change::<Cell>(neighbor_id).unwrap();
    neighbor = neighbor_ref.clone();
    
    let my_current_ink = me.ink_at_last_change + get_accumulation_rate (mutator, me.coordinates)*(mutator.now() - my_last_change);
    let neighbor_current_ink = neighbor.ink_at_last_change + get_accumulation_rate (mutator, neighbor_coordinates)*(mutator.now() - neighbor_last_change);
    let current_difference = my_current_ink - neighbor_current_ink;
 
    me.ink_at_last_change = my_current_ink;
    neighbor.ink_at_last_change = neighbor_current_ink;
    me.ink_transfers [data.dimension] = current_difference/(2*SECOND);
  }
  
  // To set field data, you specify the Column type
  // (as a generic parameter), the RowId (as a function argument),
  // and the new contents of the field (as a Option <FieldType>).
  // Passing None deletes the field.
  mutator.set::<Cell> (data.id, Some (me));
  mutator.set::<Cell> (neighbor_id, Some (neighbor));
}


/// In order to get the simulation going, we need two more events:
/// One to be the FiatEvent that initializes the cells to an initial empty state,
/// and one to be the FiatEvent that adds or removes ink in some way at real-time.
///
/// For these simple events, we lazily use the pseudo-closure syntax.
time_steward_event!{
  struct Initialize {},
  Basics,
  EventId(0x3504e024b177d96d),
  
  |&self, mutator| {
    for x in 0..mutator.constants().size [0] {
      for y in 0..mutator.constants().size [1] {
        mutator.set::<Cell> (cell_id([x,y]), Some (Cell {coordinates: [x, y], ink_at_last_change: 0, ink_transfers: [0, 0]}));
      }
    }
  }
}
time_steward_event!{
  struct AddInk {coordinates: [i32; 2], amount: i64},
  Basics,
  EventId(0xe96a5a842a47295a),
  
  |&self, mutator| {
    let my_id =cell_id(self.coordinates);
    let mut me;
    {
      let (me_ref, my_last_change) = mutator.data_and_last_change::<Cell>(my_id).unwrap();
      me = me_ref.clone();
      let my_current_ink = me.ink_at_last_change + get_accumulation_rate (mutator, me.coordinates)*(mutator.now() - my_last_change);
      me.ink_at_last_change = my_current_ink + self.amount;
    }
    mutator.set::<Cell> (my_id, Some (me));
  }
}


/// Finally, define the Basics type.
time_steward_basics!(struct Basics {
  type Time = Time;
  type Constants = Constants;
  
  /// IncludedTypes is a list of types, formed as a tuple.
  /// This list must include every Column, Event, and Predictor type that you use.
  /// This can also be a nested tree of tuples – for instance, you can
  /// export all the types from each of your files as a single tuple, then
  /// define IncludedTypes as the tuple of all of those tuples.
  type IncludedTypes = (
    ColumnType <Cell>,
    PredictorType <TransferChangePredictor>,
    EventType <TransferChange>,
    EventType <Initialize>,
    EventType <AddInk>,
  );
  
  fn max_iteration()->u32 {8}
});


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
  let constants = Constants {
    size: [60, 60],
    max_inaccuracy: 100_00,
  };

  // For some reason, docopt checking the arguments caused build_glium() to fail in emscripten.
  if !cfg!(target_os = "emscripten") {
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
  }
  {
    let mut steward: DefaultSteward <Basics> = DefaultSteward ::from_constants(constants);
    steward.insert_fiat_event(0, DeterministicRandomId::new(&0), Initialize::new()).unwrap();
    run (steward, |_,_|());
  }
}


fn run <Steward: IncrementalTimeSteward <Basics = Basics>, F: Fn (&mut Steward, Time)>(mut stew: Steward, settle:F) {


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

  let frame = || {
    let frame_begin = Instant::now();
    let time = 1 + ((start.elapsed().as_secs() as i64 * 1000000000i64) +
                          start.elapsed().subsec_nanos() as i64) *
                         SECOND / 1000000000i64;
    for ev in display.poll_events() {
      match ev {
        glium::glutin::Event::Closed => return true,
        glium::glutin::Event::MouseMoved (x,y) => {
          mouse_coordinates [0] = (x as i32) * 60 / display.get_window().unwrap().get_inner_size_pixels().unwrap().0 as i32;
          mouse_coordinates [1] = (display.get_window().unwrap().get_inner_size_pixels().unwrap().1 as i32-(y as i32)) * 60 / display.get_window().unwrap().get_inner_size_pixels().unwrap().1 as i32;
        },
        glium::glutin::Event::MouseInput (_,_) => {
          event_index += 1;
          stew.insert_fiat_event (time, DeterministicRandomId::new (& event_index), AddInk::new (
            [mouse_coordinates [0], mouse_coordinates [1]],
            (DeterministicRandomId::new (& event_index).data() [0] & ((1u64<<40)-1)) as i64 - (1<<39)
          )).unwrap();
        },
        _ => (),
      }
    }
    while let Some ((x,y)) = emscripten_compatibility::pop_click() {
      // TODO duplicate code
      mouse_coordinates [0] = (x*60.0) as i32;
      mouse_coordinates [1] = ((1.0-y)*60.0) as i32;
      event_index += 1;
      stew.insert_fiat_event (time, DeterministicRandomId::new (& event_index), AddInk::new (
            [mouse_coordinates [0], mouse_coordinates [1]],
            (DeterministicRandomId::new (& event_index).data() [0] & ((1u64<<40)-1)) as i64 - (1<<39)
          )).unwrap();
    }

    let mut target = display.draw();
    target.clear_color(1.0, 1.0, 1.0, 1.0);
    let mut vertices = Vec::<Vertex>::new();

    
    let snapshot = stew.snapshot_before(& time)
      .expect("steward failed to provide snapshot");
    settle (&mut stew, time);
    
    for x in 0.. snapshot.constants().size [0] {
      for y in 0.. snapshot.constants().size [1] {
        let (me, my_last_change) = snapshot.data_and_last_change::<Cell>(cell_id([x,y])).unwrap().clone();
        let my_current_ink = (me.ink_at_last_change + get_accumulation_rate (&snapshot, me.coordinates)*(snapshot.now() - my_last_change)) as f32;
        
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
      
    while frame_begin.elapsed() < Duration::from_millis (10) && stew.updated_until_before().map_or (false, | limitation | limitation < time + SECOND) {
        for _ in 0..8 {stew.step();}
    }
    false
  };
  
  emscripten_compatibility::main_loop(frame);
}
