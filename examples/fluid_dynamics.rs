#![feature (custom_attribute)]

extern crate serde;
#[macro_use]
extern crate serde_derive;
#[macro_use]
extern crate derivative;
extern crate array_ext;

#[macro_use]
extern crate dimensioned;
extern crate rand;

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


use rand::Rng;
use std::cmp::{min, max};
use std::collections::HashSet;

//use dimensioned::

use time_steward::{DeterministicRandomId};
use time_steward::{PersistentTypeId, PersistentlyIdentifiedType, ListedType, DataHandleTrait, DataTimelineCellTrait, Basics as BasicsTrait};
use time_steward::stewards::{simple_full as steward_module};
use steward_module::{TimeSteward, ConstructibleTimeSteward, IncrementalTimeSteward, Event, DataHandle, DataTimelineCell, EventHandle, Accessor, EventAccessor, FutureCleanupAccessor, simple_timeline};
use simple_timeline::{SimpleTimeline, query, query_ref, set, destroy, just_destroyed};

#[path = "../dev-shared/tree_continuum.rs"] mod tree_continuum;
use tree_continuum::*;

mod units {
  make_units! {
    DimensionedStruct;
    ONE: Unitless;
    
    base {
      LENGTH_UNIT: LengthUnit, "len", Length;
      TIME_UNIT: TimeUnit, "time", Time;
      MASS_UNIT: MassUnit, "mass", Mass;
      
    }
    
    derived {
      AREA_UNIT: AreaUnit = (LengthUnit*LengthUnit), Area;
      VOLUME_UNIT: VolumeUnit = (LengthUnit*LengthUnit*LengthUnit), Volume;
      DENSITY_UNIT: DensityUnit = (MassUnit/VolumeUnit);
      MASS_PER_TIME_UNIT: MassPerTimeUnit = (MassUnit/TimeUnit);
      MOMENTUM_UNIT: MomentumUnit = (MassUnit*LengthUnit/TimeUnit);
      FORCE_UNIT: ForceUnit = (MassUnit*LengthUnit/TimeUnit/TimeUnit), Force;
    }
    constants {
      
    }
    
    fmt = true;
  }
  
  macro_rules! unit {
    ($CONSTANT: ident: $Type: ident; $Basis: ident) => {
      pub type $Type = $Basis<i64>;
      pub const $CONSTANT: $Type = $Type {
        value_unsafe: 1,
        _marker: ::std::marker::PhantomData,
      };

    }
  }
  
        impl<V, U> ::serde::Serialize for DimensionedStruct<V, U> {
  fn serialize <S: ::serde::Serializer> (&self, serializer: S)->Result <S::Ok, S::Error> {
    self.value_unsafe.serialize(serializer)
  }
}

impl<'a, V, U> ::serde::Deserialize <'a> for DimensionedStruct<V, U> {
  fn deserialize <D: ::serde::Deserializer<'a>> (deserializer: D)->Result <Self, D::Error> {
    Ok(DimensionedStruct {
        value_unsafe: i64::deserialize(deserializer)?,
        _marker: ::std::marker::PhantomData,
      })

  }
}
  
  unit!(TIME_UNIT: Time; TimeUnit);
  unit!(LENGTH_UNIT: Distance; LengthUnit);
  unit!(MASS_UNIT: Mass; MassUnit);
  unit!(MASS_PER_TIME_UNIT: MassPerTime; MassPerTimeUnit );
  unit!(DENSITY_UNIT: Density; DensityUnit);
  unit!(MOMENTUM_UNIT: Momentum; MomentumUnit );
  unit!(FORCE_UNIT: Force; ForceUnit );
  pub const SECOND: Time = (1<<10)*TIME_UNIT;
  pub const METER: Distance = (1<<10)*LENGTH_UNIT;
  pub const DEPTH: Distance = LENGTH_UNIT;
  pub const GENERIC_DENSITY: Density = (1<<10)*DENSITY_UNIT;
  pub const GENERIC_MASS: Mass = GENERIC_DENSITY*METER*METER*DEPTH;
}

use units::*;


type Steward = steward_module::Steward <Basics>;

#[derive (Clone, PartialEq, Eq, Serialize, Deserialize, Debug)]
struct NodeVarying {
  last_change: Time,
  mass_at_last_change: Mass,
  
  accumulation_rate: MassPerTime,
  
  next_change: Option <EventHandle <Basics>>,
}

#[derive (Clone, PartialEq, Eq, Serialize, Deserialize, Debug)]
struct BoundaryVarying {
  last_change: Time,
  momentum: Momentum, // its effects on mass movement only change discretely at change times; but it theoretically changes continuously based on the forces
  forces: [Force; 2],
  
  next_change: Option <EventHandle <Basics>>,
}

type NodeHandle = DataHandle <tree_continuum::NodeData <Physics>>;
type BoundaryHandle = DataHandle <tree_continuum::BoundaryData <Physics>>;


#[derive (Copy, Clone, Hash, PartialEq, Eq, PartialOrd, Ord, Serialize, Deserialize, Debug, Default)]
struct Physics {}
impl PersistentlyIdentifiedType for Physics {
  const ID: PersistentTypeId = PersistentTypeId(0x9dc9e968d2864e09);
}
impl TreeContinuumPhysics for Physics {
  const DIMENSIONS: usize = 2;
  type Steward = Steward;
  type NodeVarying = NodeVarying;
  type BoundaryVarying = BoundaryVarying;
  
  //fn before_split <A: EventAccessor <Steward = Self::Steward>> (accessor: &A, splitting_node: & NodeHandle) {}
  fn initialize_split_child <A: EventAccessor <Steward = Self::Steward>> (accessor: &A, child: NewChildInfo<Self>)->Self::NodeVarying {
    NodeVarying {
      last_change: *accessor.now(),
      ink_at_last_change: child.splitting_varying.data.mass_at_last_change >> 2,
      accumulation_rate: 0,
      slope: [0; 2],
    }
  }
  fn initialize_split_boundary <A: EventAccessor <Steward = Self::Steward>> (accessor: &A, boundary: NewBoundaryInfo<Self>)->Self::BoundaryVarying {
    BoundaryVarying {
      momentum_at_last_change: 0*MOMENTUM_UNIT,
      forces: [0*FORCE_UNIT; 2],
      next_change: None,
    }
  }
  fn after_split <A: EventAccessor <Steward = Self::Steward>> (accessor: &A, split_node: & NodeHandle, new_boundaries: Vec<BoundaryHandle>) {
    let guard = query_ref (accessor, & split_node.varying);
    let branch_varying = unwrap_branch_ref(&guard);
  
    iterate_children (& branch_varying.children, | coordinates, child | {
      update_node(accessor, child);
      update_inferred_node_properties (accessor, child);
    });
    
    for boundary in new_boundaries {
      //printlnerr!("{:?}", (boundary.center, split_node.center, split_node.width, boundary.nodes[0].center, boundary.nodes[0].width, boundary.nodes[1].center, boundary.nodes[1].width));
      update_transfer_change_prediction (accessor, &boundary);
    }
  }
  
  //fn before_merge <A: EventAccessor <Steward = Self::Steward>> (accessor: &A, merging_node: & NodeHandle <Self>) {}
  //fn initialize_merge_parent <A: EventAccessor <Steward = Self::Steward>> (accessor: &A)->Self::NodeVarying;
  fn initialize_merge_boundary <A: EventAccessor <Steward = Self::Steward>> (accessor: &A, boundary: MergeBoundaryInfo<Self>)->Self::BoundaryVarying {
    BoundaryVarying {
      momentum_at_last_change: 0*MOMENTUM_UNIT,
      forces: [0*FORCE_UNIT; 2],
      next_change: None,
    }
  }
  //fn after_merge <A: EventAccessor <Steward = Self::Steward>> (accessor: &A, merged_node: & NodeHandle <Self>, new_boundaries: Vec<BoundaryHandle <Self>>) {}
}


fn set_leaf<A: EventAccessor <Steward = Steward >> (accessor: &A, varying: & DataTimelineCell <SimpleTimeline <tree_continuum::NodeVarying<Physics>, Steward>>, data:tree_continuum::LeafVarying<Physics>) {
  set (accessor, &varying, tree_continuum::NodeVarying::Leaf (data));
}



fn update_inferred_node_properties <A: EventAccessor <Steward = Steward >> (accessor: &A, node: &NodeHandle) {
  let mut varying = unwrap_leaf (query (accessor, &node.varying));
  varying.data.accumulation_rate = 0*MASS_UNIT/TIME_UNIT;
  
  {
    let data = &mut varying.data;
    iterate_boundaries (& varying.boundaries, | dimension, direction, boundary | {
      let direction_signum = (direction as i64*2)-1;
      let neighbor = boundary.nodes [dimension];
      let transfer_rate = query_ref (accessor, & boundary.varying).data.momentum / ((node.width + neighbor.width) >> 1);
      data.accumulation_rate -= transfer_rate*direction_signum;
    });
  }
  set_leaf (accessor, &node.varying, varying);
}
fn update_forces <A: EventAccessor <Steward = Steward>> (accessor: &A, node: &NodeHandle) {
  {
    let density = density_at(accessor, node, *accessor.now());
    
    let guard = query_ref (accessor, &node.varying);
    let varying = unwrap_leaf_ref (&guard);
    //assert!(varying.last_change == *accessor.now());
    
    let pressure = density*(GENERIC_MASS/SECOND/SECOND/METER)/(4*GENERIC_DENSITY);
  
    iterate_boundaries (& varying.boundaries, | dimension, direction, boundary | {
      update_boundary(accessor, boundary);
      let mut boundary_varying = query (accessor, &boundary.varying);
  
      boundary_varying.data.forces [(direction + 1) & 1] = pressure * (boundary.length*DEPTH);
  
      set (accessor, &boundary.varying, boundary_varying);
    });
  }
  update_inferred_node_properties (accessor, node);
}

fn update_node <A: EventAccessor <Steward = Steward >> (accessor: &A, node: &NodeHandle) {
  let mut varying = unwrap_leaf (query (accessor, &node.varying));
  
  varying.data.mass_at_last_change = mass_at (accessor, node, *accessor.now());
  varying.data.last_change = *accessor.now();
  
  set_leaf (accessor, &node.varying, varying);
}

fn update_boundary <A: EventAccessor <Steward = Steward >> (accessor: &A, boundary: &BoundaryHandle) {
  let mut varying = query (accessor, &boundary.varying);
  
  varying.data.momentum = momentum_at (accessor, boundary, *accessor.now());
  varying.data.last_change = *accessor.now();
  
  set (accessor, &boundary.varying, varying);
}

fn mass_at <A: Accessor <Steward = Steward >> (accessor: &A, node: &NodeHandle, time: Time)->Mass {
  let guard = query_ref (accessor, &node.varying);
  let varying = unwrap_leaf_ref (&guard);
  varying.data.mass_at_last_change + varying.data.accumulation_rate*(time - varying.data.last_change)
}
fn density_at <A: Accessor <Steward = Steward >> (accessor: &A, node: &NodeHandle, time: Time)->Density {
  mass_at (accessor, node, time)/(node.width*LENGTH_UNIT*node.width*LENGTH_UNIT*DEPTH)
}
fn momentum_at <A: Accessor <Steward = Steward >> (accessor: &A, node: &BoundaryHandle, time: Time)->Momentum {
  let guard = query_ref (accessor, &node.varying);
  guard.data.momentum + (guard.data.forces[0]-guard.data.forces[1])*(time - guard.data.last_change)
}


fn update_momentum_change_prediction <A: EventAccessor <Steward = Steward >> (accessor: &A, boundary: &BoundaryHandle) {
  let mut varying = query (accessor, &boundary.varying);
  
  let time = Some(varying.data.last_change + accessor.id().to_rng().gen_range(SECOND.value_unsafe/4, SECOND.value_unsafe/3)*TIME_UNIT);
    
  varying.data.next_change = time.map (|time| {
    accessor.create_prediction (
      time,
      DeterministicRandomId::new (&(accessor.id(), boundary.center)),
      TransferChange {boundary: boundary.clone()}
    )
  });
  
  set (accessor, &boundary.varying, varying);
}

fn update_force_change_prediction <A: EventAccessor <Steward = Steward >> (accessor: &A, node: &NodeHandle) {
  let mut varying = unwrap_leaf (query (accessor, &node.varying));
  
  
  let time = Some(varying.data.last_change + accessor.id().to_rng().gen_range(SECOND.value_unsafe/4, SECOND.value_unsafe/3)*TIME_UNIT);
    
  varying.data.next_change = time.map (|time| {
    accessor.create_prediction (
      time,
      DeterministicRandomId::new (&(accessor.id(), node.center)),
      ForceChange {node: node.clone()}
    )
  });
  
  set_leaf (accessor, &node.varying, varying);
}



#[derive (Clone, PartialEq, Eq, Serialize, Deserialize, Debug)]
struct Globals {
  size: [Distance; 2],

  
  root: NodeHandle,
}



#[derive (Clone, PartialEq, Eq, Serialize, Deserialize, Debug)]
struct MomentumChange {boundary: BoundaryHandle}
impl PersistentlyIdentifiedType for MomentumChange {
  const ID: PersistentTypeId = PersistentTypeId(0x202c5edfe6f3332d);
}
impl Event for MomentumChange {
  type Steward = Steward;
  type ExecutionData = ();
  fn execute <Accessor: EventAccessor <Steward = Self::Steward>> (&self, accessor: &mut Accessor) {
    
    // TODO: invalidation
  }

  fn undo <Accessor: FutureCleanupAccessor <Steward = Self::Steward>> (&self, _accessor: &mut Accessor, _: ()) {
    unimplemented!()
  }
}

#[derive (Clone, PartialEq, Eq, Serialize, Deserialize, Debug)]
struct ForceChange {node: NodeHandle}
impl PersistentlyIdentifiedType for ForceChange {
  const ID: PersistentTypeId = PersistentTypeId(0x047cd6caa4fb5958);
}
impl Event for ForceChange {
  type Steward = Steward;
  type ExecutionData = ();
  fn execute <Accessor: EventAccessor <Steward = Self::Steward>> (&self, accessor: &mut Accessor) {
    
    // TODO: invalidation
  }

  fn undo <Accessor: FutureCleanupAccessor <Steward = Self::Steward>> (&self, _accessor: &mut Accessor, _: ()) {
    unimplemented!()
  }
}


#[derive (Clone, PartialEq, Eq, Serialize, Deserialize, Debug)]
struct AddMass {coordinates: [Distance; 2], amount: Mass, accumulation: MassPerTime}
impl PersistentlyIdentifiedType for AddMass {
  const ID: PersistentTypeId = PersistentTypeId(0x90ef948b75373af9);
}
impl Event for AddMass {
  type Steward = Steward;
  type ExecutionData = ();
  fn execute <Accessor: EventAccessor <Steward = Self::Steward>> (&self, accessor: &mut Accessor) {
    let mut node = accessor.globals().root.clone();
    loop {
      match query (accessor, &node.varying) {
        tree_continuum::NodeVarying::Branch (ref b) => node = child_by_coordinates (& b.children,
           [if self.coordinates [0] > node.center [0]*LENGTH_UNIT {1} else {0},
            if self.coordinates [1] > node.center [1]*LENGTH_UNIT {1} else {0}]).clone(),
        tree_continuum::NodeVarying::Leaf (_) => {
          update_node (accessor, &node);
          if node.width*LENGTH_UNIT <= METER {
            break
          }
          split (accessor, &node);
        },
      };
    }
    let mut varying = unwrap_leaf (query (accessor, &node.varying));
    varying.data.mass_at_last_change += self.amount;
    //node.fiat_accumulation_rate += self.accumulation;
    let changed_boundaries = varying.boundaries.clone();
    set_leaf (accessor, &node.varying, varying);
    
    iterate_boundaries (& changed_boundaries, | dimension, direction, boundary | {
      update_transfer_change_prediction (accessor, boundary);
    });
  }
  fn undo <Accessor: FutureCleanupAccessor <Steward = Self::Steward>> (&self, _accessor: &mut Accessor, _: ()) {
    unimplemented!()
  }
}


#[derive (Clone, PartialEq, Eq, Serialize, Deserialize, Debug)]
struct Initialize {}
impl PersistentlyIdentifiedType for Initialize {
  const ID: PersistentTypeId = PersistentTypeId(0x8b6a3e1d0c1967f6);
}
impl Event for Initialize {
  type Steward = Steward;
  type ExecutionData = ();
  fn execute <Accessor: EventAccessor <Steward = Self::Steward>> (&self, accessor: &mut Accessor) {
    let globals = accessor.globals();
    set_leaf (accessor, &globals.root.varying, tree_continuum::LeafVarying {
      data: NodeVarying {
        last_change: 0*TIME_UNIT,
        mass_at_last_change: 0*MASS_UNIT,
        accumulation_rate: 0*MASS_PER_TIME_UNIT,
        next_change: None,
      },
      boundaries: faces_from_fn (|_,_| tree_continuum::FaceBoundaries::WorldEdge),
    });
  }
  fn undo <Accessor: FutureCleanupAccessor <Steward = Self::Steward>> (&self, _accessor: &mut Accessor, _: ()) {
    unimplemented!()
  }
}

fn make_globals()-> Globals {
  Globals {
    size: [64*METER, 64*METER],
    root: DataHandle::new_for_globals (tree_continuum::NodeData {
      width: 64*METER.value_unsafe,
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
  type Types = (ListedType <MomentumChange>, ListedType <ForceChange>, ListedType <Initialize>, ListedType <AddMass>);
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
  steward.insert_fiat_event(0*TIME_UNIT, DeterministicRandomId::new(&0), Initialize{}).unwrap();
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
  stew.snapshot_before(&(1*TIME_UNIT));
  let mut previous_real_time = Instant::now();
  let mut previous_time = 1*TIME_UNIT;
  let mut display_state = 0;
  let mut unrestricted_speed = false;
  let mut input_magnitude_shift = 0;
  let mut input_signum: i64 = 1;
  let mut input_derivative: i64 = 0;

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
                                          (x as i64)  * accessor.globals().size [0].value_unsafe
            / display_size.0 as i64 - accessor.globals().size [0].value_unsafe/2;
          mouse_coordinates [1] =
            (display_size.1 as i64 - (y as i64)) * accessor.globals().size [1].value_unsafe
            / display_size.1 as i64 - accessor.globals().size [1].value_unsafe/2;
        },
        glium::glutin::Event::MouseInput (_,_) => {
          //if in_bounds (globals, mouse_coordinates) {
            event_index += 1;
            stew.insert_fiat_event (time, DeterministicRandomId::new (& event_index), AddMass {
              coordinates: [mouse_coordinates [0]*LENGTH_UNIT, mouse_coordinates [1]*LENGTH_UNIT],
              amount: ((input_derivative+1)%2)*(GENERIC_MASS << input_magnitude_shift)*input_signum,
              accumulation: input_derivative*(GENERIC_MASS << input_magnitude_shift)*input_signum/(SECOND),
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
      mouse_coordinates [0] = ((x-0.5)*accessor.globals().size [0].value_unsafe as f64) as i64;
      mouse_coordinates [1] = ((0.5-y)*accessor.globals().size [0].value_unsafe as f64) as i64;
      //if in_bounds (globals, mouse_coordinates) {
        event_index += 1;
        stew.insert_fiat_event (time, DeterministicRandomId::new (& event_index), AddMass {
            coordinates: [mouse_coordinates [0]*LENGTH_UNIT, mouse_coordinates [1]*LENGTH_UNIT],
            amount: ((input_derivative+1)%2)*(GENERIC_MASS*8 << input_magnitude_shift)*input_signum,
            accumulation: input_derivative*(GENERIC_MASS*8 << input_magnitude_shift)*input_signum/(SECOND),
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
      let guard = query_ref(&accessor, &handle.varying);
      match *guard {
        tree_continuum::NodeVarying::Branch (ref b) => iterate_children (&b.children, | coordinates, child | to_draw.push(child.clone())),
        tree_continuum::NodeVarying::Leaf (ref leaf) => {
          let size_factor = (accessor.globals().size [0]/2).value_unsafe as f32;
          let density_factor = (GENERIC_DENSITY.value_unsafe) as f32;
          let (my_current_ink, slope) = match display_state {
            0 => (
               density_at (&accessor, &handle, *accessor.now()).value_unsafe as f32 / density_factor,
                 [0.0,0.0]//[leaf.data.slope[0] as f32*size_factor/density_factor, leaf.data.slope[1] as f32*size_factor/density_factor]
            ),
            1 => (
               ((leaf.data.accumulation_rate * SECOND * 100 / (handle.width*handle.width)).value_unsafe as f32) / density_factor,
               [0.0,0.0]
            ),
            _ => ((*accessor.now() - leaf.data.last_change).value_unsafe as f32 / SECOND.value_unsafe as f32, [0.0,0.0]),
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
        },
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
