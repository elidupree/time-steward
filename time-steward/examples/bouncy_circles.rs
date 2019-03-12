extern crate time_steward;

#[macro_use]
extern crate glium;

extern crate nalgebra;
extern crate rand;
extern crate boolinator;
extern crate docopt;

extern crate serde;
#[macro_use]
extern crate serde_derive;

//use docopt::Docopt;

/*const USAGE: &'static str = "
Bouncy Circles, a simple TimeSteward test case.

Usage:
  bouncy_circles
  bouncy_circles (-l | --listen) <host> <port>
  bouncy_circles (-c | --connect) <host> <port>
  
Options:
  -l, --listen   Start a synchronized simulation by listening for TCP connections.
  -c, --connect  Start a synchronized simulation by making a TCP connection.
";*/

#[derive(Debug, Deserialize)]
struct Args {
  flag_listen: bool,
  flag_connect: bool,
    
  arg_host: Option <String>,
  arg_port: Option <u16>,
}


use std::time::{Instant};
use glium::{DisplayBuild, Surface};

use time_steward::{DeterministicRandomId};
//use time_steward::stewards::{simple_full as steward_module};
use steward_module::{TimeSteward, ConstructibleTimeSteward, Accessor, simple_timeline};
use simple_timeline::{query};

#[path = "../dev-shared/bouncy_circles.rs"] mod bouncy_circles;
use bouncy_circles::*;
#[path = "../dev-shared/emscripten_compatibility.rs"] mod emscripten_compatibility;
pub use emscripten_compatibility::canvas_click;


#[derive(Copy, Clone)]

struct Vertex {
  direction: [f32; 2],
  center: [f32; 2],
  radius: f32,
}
implement_vertex!(Vertex, direction, center, radius);

//use std::net::{TcpListener, TcpStream};
//use std::io::{BufReader, BufWriter};

fn main() {
  // For some reason, docopt checking the arguments caused build_glium() to fail in emscripten.
  /*if !cfg!(target_os = "emscripten") {
    let arguments: Args = Docopt::new(USAGE)
                            .and_then(|d| d.deserialize())
                            .unwrap_or_else(|e| e.exit());
    
    if arguments.flag_listen {
      let listener = TcpListener::bind ((arguments.arg_host.as_ref().map_or("localhost", | string | string as & str), arguments.arg_port.unwrap())).unwrap();
      let stream = listener.accept().unwrap().0;
      let mut steward: simply_synchronized::Steward<Basics, amortized::Steward<Basics>> = simply_synchronized::Steward::new(DeterministicRandomId::new (& 0u32), 0, SECOND>>3,(), BufReader::new (stream.try_clone().unwrap()), BufWriter::new (stream));
      steward.insert_fiat_event(0, DeterministicRandomId::new(&0), Initialize::new()).unwrap();
      run (steward, |a,b| (a.settle_before (b)));
      return;
    }
    else if arguments.flag_connect {
      let stream = TcpStream::connect ((arguments.arg_host.as_ref().map_or("localhost", | string | string as & str), arguments.arg_port.unwrap())).unwrap();
      let steward: simply_synchronized::Steward<Basics, amortized::Steward<Basics>> = simply_synchronized::Steward::new(DeterministicRandomId::new (& 1u32), 0, SECOND>>3,(), BufReader::new (stream.try_clone().unwrap()), BufWriter::new (stream));
      run (steward, |a,b| (a.settle_before (b)));
      return;
    }
  }*/
  {
    //let mut steward: s::Steward<Basics,
                                //inefficient_flat::Steward<Basics>,
                                //memoized_flat::Steward<Basics>> = s::Steward::from_constants(());
    let mut steward: Steward = Steward::from_globals(make_globals());
    steward.insert_fiat_event(0, DeterministicRandomId::new(&0), Initialize{}).unwrap();
    run (steward, |_,_|());
  }
}


fn run <F: Fn (&mut Steward, Time)>(mut stew: Steward, settle:F) {


  let vertex_shader_source = r#"
#version 100
attribute lowp vec2 direction;
attribute lowp vec2 center;
attribute lowp float radius;
varying lowp vec2 direction_transfer;

void main() {
direction_transfer = direction*1.5;
gl_Position = vec4 ( center + direction*1.5*radius, 0.0, 1.0);
}

"#;

  let fragment_shader_source = r#"
#version 100
varying lowp vec2 direction_transfer;

void main() {
if (dot (direction_transfer,direction_transfer) <1.0) {
gl_FragColor = vec4 (1.0, 0.0, 0.0, 1.0);
} else {
gl_FragColor = vec4 (0.0, 0.0, 0.0, 0.0);

}
}

"#;

  //let mut snapshots = Vec::new();
  
  let mut event_index = 0u64;
  let mut mouse_coordinates = [0,0];

  if true {
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

    // take care of the expensive initial predictions before starting the timer
    stew.snapshot_before(&1);
    let start = Instant::now();

    let frame = || {
      //let frame_begin = Instant::now();
      let time = 1+((start.elapsed().as_secs() as i64 * 1000000000i64) +
                            start.elapsed().subsec_nanos() as i64) *
                           SECOND / 1000000000i64;
      for ev in display.poll_events() {
        match ev {
          glium::glutin::Event::Closed => return true,
          glium::glutin::Event::MouseMoved (x,y) => {
            mouse_coordinates [0] = ((x as SpaceCoordinate) - 150) * ARENA_SIZE / 300;
            mouse_coordinates [1] = (450-(y as SpaceCoordinate)) * ARENA_SIZE / 300;
            //println!("mouse {} {} {:?}", x,y,mouse_coordinates);
          },
          glium::glutin::Event::MouseInput (_,_) => {
            event_index += 1;
            stew.insert_fiat_event (time, DeterministicRandomId::new (& event_index), Disturb {coordinates: [mouse_coordinates [0], mouse_coordinates [1]]}).unwrap();
          },
          _ => (),
        }
      }
      while let Some ((x,y)) = emscripten_compatibility::pop_click() {
        // TODO duplicate code
        mouse_coordinates [0] = (((x*600.0) as SpaceCoordinate) - 150) * ARENA_SIZE / 300;
        mouse_coordinates [1] = (450-((y*600.0) as SpaceCoordinate)) * ARENA_SIZE / 300;
        event_index += 1;
        stew.insert_fiat_event (time, DeterministicRandomId::new (& event_index), Disturb {coordinates: [mouse_coordinates [0], mouse_coordinates [1]]}).unwrap();
      }

      let mut target = display.draw();
      target.clear_color(0.0, 0.0, 0.0, 1.0);
      let mut vertices = Vec::<Vertex>::new();

      let accessor = stew.snapshot_before(& time)
        .expect("steward failed to provide snapshot");
      stew.forget_before(& time);
      settle (&mut stew, time);
      for handle in accessor.globals().circles.iter() {
        let circle = query (& accessor, &handle.varying);
        let position = circle.position.value (*accessor.now(), STATIC_TIME_SHIFT).unwrap();
        let center = [position[0] as f32 / ARENA_SIZE as f32 - 0.5,
                      position[1] as f32 / ARENA_SIZE as f32 - 0.5];
        let radius = handle.radius as f32 / ARENA_SIZE as f32;
        // println!("drawing circ at {}, {}", center[0],center[1]);
        vertices.extend(&[Vertex {
                            center: center,
                            radius: radius,
                            direction: [1.0, 0.0],
                          },
                          Vertex {
                            center: center,
                            radius: radius,
                            direction: [-1.0, 0.0],
                          },
                          Vertex {
                            center: center,
                            radius: radius,
                            direction: [0.0, 1.0],
                          },
                          Vertex {
                            center: center,
                            radius: radius,
                            direction: [1.0, 0.0],
                          },
                          Vertex {
                            center: center,
                            radius: radius,
                            direction: [-1.0, 0.0],
                          },
                          Vertex {
                            center: center,
                            radius: radius,
                            direction: [0.0, -1.0],
                          }]);
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
    
    emscripten_compatibility::main_loop (frame);
  }

  /*
  for increment in 1..21 {
    snapshots.push(stew.snapshot_before(&(increment * SECOND * 2)));
  }
  for snapshot in snapshots.iter_mut()
    .map(|option| option.as_mut().expect("all these snapshots should have been valid")) {
    println!("snapshot for {}", snapshot.now());
    // for index in 0..HOW_MANY_CIRCLES {
    // println!("{}", snapshot.get::<Circle> (get_circle_id (index)).expect("missing circle").position);
    // }
  }*/
  // panic!("anyway")
}
