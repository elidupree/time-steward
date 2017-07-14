#[macro_use]
extern crate time_steward;

#[macro_use]
extern crate glium;

extern crate nalgebra;
extern crate rustc_serialize;
extern crate docopt;

extern crate serde;
#[macro_use]
extern crate serde_derive;

use docopt::Docopt;

const USAGE: &'static str = "
Bouncy Circles, a simple TimeSteward test case.

Usage:
  bouncy_circles
  bouncy_circles (-l | --listen) <host> <port>
  bouncy_circles (-c | --connect) <host> <port>
  
Options:
  -l, --listen   Start a synchronized simulation by listening for TCP connections.
  -c, --connect  Start a synchronized simulation by making a TCP connection.
";

#[derive(Debug, RustcDecodable)]
struct Args {
  flag_listen: bool,
  flag_connect: bool,
    
  arg_host: Option <String>,
  arg_port: Option <u16>,
}


//use time_steward::stewards::crossverified as s;
use time_steward::{TimeSteward, TimeStewardFromConstants, IncrementalTimeSteward, DeterministicRandomId, Accessor,
     MomentaryAccessor};
use std::thread::sleep;
use std::time::{Instant, Duration};
use glium::{DisplayBuild, Surface};
use time_steward::stewards::{amortized, simply_synchronized};

#[path = "../dev-shared/bouncy_circles.rs"] mod bouncy_circles;
use bouncy_circles::*;


#[derive(Copy, Clone)]

struct Vertex {
  direction: [f32; 2],
  center: [f32; 2],
  radius: f32,
}
implement_vertex!(Vertex, direction, center, radius);

use std::net::{TcpListener, TcpStream};
use std::io::{BufReader, BufWriter};

fn main() {
  let arguments: Args = Docopt::new(USAGE)
                            .and_then(|d| d.decode())
                            .unwrap_or_else(|e| e.exit());
  
  if arguments.flag_listen {
    let listener = TcpListener::bind ((arguments.arg_host.as_ref().map_or("localhost", | string | string as & str), arguments.arg_port.unwrap())).unwrap();
    let stream = listener.accept().unwrap().0;
    let mut steward: simply_synchronized::Steward<Basics, amortized::Steward<Basics>> = simply_synchronized::Steward::new(DeterministicRandomId::new (& 0u32), 0, SECOND>>3,(), BufReader::new (stream.try_clone().unwrap()), BufWriter::new (stream));
    steward.insert_fiat_event(0, DeterministicRandomId::new(&0), Initialize::new()).unwrap();
    run (steward, |a,b| (a.settle_before (b)));
  }
  else if arguments.flag_connect {
    let stream = TcpStream::connect ((arguments.arg_host.as_ref().map_or("localhost", | string | string as & str), arguments.arg_port.unwrap())).unwrap();
    let steward: simply_synchronized::Steward<Basics, amortized::Steward<Basics>> = simply_synchronized::Steward::new(DeterministicRandomId::new (& 1u32), 0, SECOND>>3,(), BufReader::new (stream.try_clone().unwrap()), BufWriter::new (stream));
    run (steward, |a,b| (a.settle_before (b)));
  }
  else {
    //let mut steward: s::Steward<Basics,
                                //inefficient_flat::Steward<Basics>,
                                //memoized_flat::Steward<Basics>> = s::Steward::from_constants(());
    let mut steward: amortized::Steward<Basics> = amortized::Steward::from_constants(());
    steward.insert_fiat_event(0, DeterministicRandomId::new(&0), Initialize::new()).unwrap();
    run (steward, |_,_|());
  }
}


fn run <Steward: IncrementalTimeSteward <Basics = Basics>,F: Fn (&mut Steward, Time)>(mut stew: Steward, settle:F) {


  let vertex_shader_source = r#"
#version 140
in vec2 direction;
in vec2 center;
in float radius;
out vec2 direction_transfer;

void main() {
direction_transfer = direction*1.5;
gl_Position = vec4 ( center + direction*1.5*radius, 0.0, 1.0);
}

"#;

  let fragment_shader_source = r#"
#version 140
in vec2 direction_transfer;
out vec4 color;

void main() {
if (dot (direction_transfer,direction_transfer) <1.0) {
color = vec4 (1.0, 0.0, 0.0, 1.0);
} else {
color = vec4 (0.0, 0.0, 0.0, 0.0);

}
}

"#;

  let mut snapshots = Vec::new();
  
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

    loop {
      let frame_begin = Instant::now();
      let time =((start.elapsed().as_secs() as i64 * 1000000000i64) +
                            start.elapsed().subsec_nanos() as i64) *
                           SECOND / 1000000000i64;
      for ev in display.poll_events() {
        match ev {
          glium::glutin::Event::Closed => return,
          glium::glutin::Event::MouseMoved (x,y) => {
            mouse_coordinates [0] = ((x as SpaceCoordinate) - 150) * ARENA_SIZE / 300;
            mouse_coordinates [1] = (450-(y as SpaceCoordinate)) * ARENA_SIZE / 300;
            //println!("mouse {} {} {:?}", x,y,mouse_coordinates);
          },
          glium::glutin::Event::MouseInput (_,_) => {
            event_index += 1;
            stew.insert_fiat_event (time, DeterministicRandomId::new (& event_index), Disturb::new ([mouse_coordinates [0], mouse_coordinates [1]])).unwrap();
          },
          _ => (),
        }
      }

      let mut target = display.draw();
      target.clear_color(0.0, 0.0, 0.0, 1.0);
      let mut vertices = Vec::<Vertex>::new();

      let snapshot = stew.snapshot_before(& time)
        .expect("steward failed to provide snapshot");
      settle (&mut stew, time);
      for index in 0..HOW_MANY_CIRCLES {
        if let Some ((circle, time)) = snapshot.data_and_last_change::<Circle>(get_circle_id(index)){
        let position = circle.position.updated_by(snapshot.now() - time).unwrap().evaluate();
        let center = [position[0] as f32 / ARENA_SIZE as f32 - 0.5,
                      position[1] as f32 / ARENA_SIZE as f32 - 0.5];
        let radius = circle.radius as f32 / ARENA_SIZE as f32;
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
      sleep(Duration::from_millis(10));
    }
  }


  for increment in 1..21 {
    snapshots.push(stew.snapshot_before(&(increment * SECOND * 2)));
  }
  for snapshot in snapshots.iter_mut()
    .map(|option| option.as_mut().expect("all these snapshots should have been valid")) {
    println!("snapshot for {}", snapshot.now());
    // for index in 0..HOW_MANY_CIRCLES {
    // println!("{}", snapshot.get::<Circle> (get_circle_id (index)).expect("missing circle").position);
    // }
  }
  // panic!("anyway")
}
