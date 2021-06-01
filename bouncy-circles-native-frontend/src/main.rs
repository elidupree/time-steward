use glium::{glutin, implement_vertex, Surface};
use serde::Deserialize;
use std::time::{Duration, Instant};

use time_steward::{
  ConstructibleTimeSteward, EntityId, InitializedAccessor, ReadAccess, TimeSteward,
};

use bouncy_circles::physics::*;
use glium::glutin::dpi::LogicalPosition;

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

  arg_host: Option<String>,
  arg_port: Option<u16>,
}

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
      let mut steward: simply_synchronized::Steward<SimulationSpec, amortized::Steward<SimulationSpec>> = simply_synchronized::Steward::new(EntityId::hash_of (& 0u32), 0, SECOND>>3,(), BufReader::new (stream.try_clone().unwrap()), BufWriter::new (stream));
      steward.insert_fiat_event(0, EntityId::hash_of(&0), Initialize::new()).unwrap();
      run (steward, |a,b| (a.settle_before (b)));
      return;
    }
    else if arguments.flag_connect {
      let stream = TcpStream::connect ((arguments.arg_host.as_ref().map_or("localhost", | string | string as & str), arguments.arg_port.unwrap())).unwrap();
      let steward: simply_synchronized::Steward<SimulationSpec, amortized::Steward<SimulationSpec>> = simply_synchronized::Steward::new(EntityId::hash_of (& 1u32), 0, SECOND>>3,(), BufReader::new (stream.try_clone().unwrap()), BufWriter::new (stream));
      run (steward, |a,b| (a.settle_before (b)));
      return;
    }
  }*/
  {
    //let mut steward: s::Steward<SimulationSpec,
    //inefficient_flat::Steward<SimulationSpec>,
    //memoized_flat::Steward<SimulationSpec>> = s::Steward::from_constants(());
    let mut steward =
      time_steward_simple_flat::Steward::from_construct_globals((), BouncyCirclesSpec);
    steward
      .insert_fiat_event::<Initialize>(0, EntityId::hash_of(&0), (), ())
      .unwrap();
    run(steward, |_, _| ());
  }
}

fn run<
  Steward: TimeSteward<SimulationSpec = BouncyCirclesSpec>,
  F: Fn(&mut Steward, Time) + 'static,
>(
  mut stew: Steward,
  settle: F,
) {
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
  let mut mouse_coordinates = [0, 0];

  if true {
    let events_loop = glutin::event_loop::EventLoop::new();
    let wb = glutin::window::WindowBuilder::new()
      .with_title("bouncy-circles")
      .with_inner_size(glutin::dpi::LogicalSize::new(600, 600));
    let cb = glutin::ContextBuilder::new();
    let display = glium::Display::new(wb, cb, &events_loop).unwrap();
    let program =
      glium::Program::from_source(&display, vertex_shader_source, fragment_shader_source, None)
        .expect("glium program generation failed");
    let parameters = glium::DrawParameters {
      blend: glium::draw_parameters::Blend::alpha_blending(),
      ..Default::default()
    };
    let indices = glium::index::NoIndices(glium::index::PrimitiveType::TrianglesList);

    // take care of the expensive initial predictions before starting the timer
    stew.snapshot_before(1);
    let start = Instant::now();
    let mut scale_factor = 1.0;

    events_loop.run(move |ev, _, control_flow| {
      //let frame_begin = Instant::now();
      let next_frame_time = Instant::now() + Duration::from_millis(10);
      *control_flow = glutin::event_loop::ControlFlow::WaitUntil(next_frame_time);
      let time = 1
        + ((start.elapsed().as_secs() as i64 * 1000000000i64)
          + start.elapsed().subsec_nanos() as i64)
          * SECOND
          / 1000000000i64;
      if let glutin::event::Event::WindowEvent { event, .. } = ev {
        match event {
          glutin::event::WindowEvent::CloseRequested => {
            *control_flow = glutin::event_loop::ControlFlow::Exit;
          }
          glutin::event::WindowEvent::ScaleFactorChanged {
            scale_factor: new_scale_factor,
            ..
          } => {
            scale_factor = new_scale_factor;
          }
          glutin::event::WindowEvent::CursorMoved { position, .. } => {
            let position: LogicalPosition<f64> = position.to_logical(scale_factor);
            mouse_coordinates[0] =
              ((position.x - 150.0) * ARENA_SIZE as f64 / 300.0) as SpaceCoordinate;
            mouse_coordinates[1] =
              ((450.0 - position.y) * ARENA_SIZE as f64 / 300.0) as SpaceCoordinate;
            //println!("mouse {} {} {:?}", x,y,mouse_coordinates);
          }
          glutin::event::WindowEvent::MouseInput { .. } => {
            event_index += 1;
            //println!("Sending Disturb at {}: {:?}", time, mouse_coordinates);
            stew
              .insert_fiat_event::<Disturb>(
                time,
                EntityId::hash_of(&event_index),
                Disturb {
                  coordinates: mouse_coordinates,
                },
                (),
              )
              .unwrap();
          }
          _ => {}
        }
        return;
      }

      let mut target = display.draw();
      target.clear_color(0.0, 0.0, 0.0, 1.0);
      let mut vertices = Vec::<Vertex>::new();

      let accessor = stew
        .snapshot_before(time)
        .expect("steward failed to provide snapshot");
      stew.forget_before(time);
      settle(&mut stew, time);
      for handle in accessor.globals().circles.iter() {
        let circle = handle.read(&accessor);
        let position = circle
          .position
          .value(*accessor.now(), STATIC_TIME_SHIFT)
          .unwrap();
        let center = [
          position[0] as f32 / ARENA_SIZE as f32 - 0.5,
          position[1] as f32 / ARENA_SIZE as f32 - 0.5,
        ];
        let radius = handle.radius as f32 / ARENA_SIZE as f32;
        // println!("drawing circ at {}, {}", center[0],center[1]);
        vertices.extend(&[
          Vertex {
            center,
            radius,
            direction: [1.0, 0.0],
          },
          Vertex {
            center,
            radius,
            direction: [-1.0, 0.0],
          },
          Vertex {
            center,
            radius,
            direction: [0.0, 1.0],
          },
          Vertex {
            center,
            radius,
            direction: [1.0, 0.0],
          },
          Vertex {
            center,
            radius,
            direction: [-1.0, 0.0],
          },
          Vertex {
            center,
            radius,
            direction: [0.0, -1.0],
          },
        ]);
      }
      target
        .draw(
          &glium::VertexBuffer::new(&display, &vertices)
            .expect("failed to generate glium Vertex buffer"),
          &indices,
          &program,
          &glium::uniforms::EmptyUniforms,
          &parameters,
        )
        .expect("failed target.draw");

      target.finish().expect("failed to finish drawing");

      /*while frame_begin.elapsed() < Duration::from_millis (10) && stew.updated_until_before().map_or (false, | limitation | limitation < time + SECOND) {
        for _ in 0..8 {stew.step();}
      }*/
    });
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
