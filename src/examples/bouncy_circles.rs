extern crate nalgebra;

use memoized_flat_time_steward as s;
use {TimeSteward, DeterministicRandomId, Column, ColumnId, RowId, PredictorId, Mutator,
     TimeStewardStaticMethods, Accessor, MomentaryAccessor, PredictorAccessor};
use collision_detection::inefficient as collisions;

use time_functions::QuadraticTrajectory;
use std::rc::Rc;
use rand::Rng;
use nalgebra::Vector2;
use std::thread::sleep;
use std::time::{Instant, Duration};
use glium;
use glium::{DisplayBuild, Surface};

use std::io::Write;
macro_rules! printlnerr(
    ($($arg:tt)*) => { {
        let r = writeln!(&mut ::std::io::stderr(), $($arg)*);
        r.expect("failed printing to stderr");
    } }
);

type Time = i64;
type SpaceCoordinate = i64;


const HOW_MANY_CIRCLES: i32 = 20;
const ARENA_SIZE: SpaceCoordinate = (1 << 20);
const TIME_SHIFT: i32 = 20;
const SECOND: Time = (1 << TIME_SHIFT);

#[derive(Clone)]
struct Basics;
impl ::Basics for Basics {
  type Time = Time;
  type Constants = ();
}
struct CollisionBasics {}
impl ::collision_detection::Basics for CollisionBasics {
  type StewardBasics = Basics;
  type DetectorId =();
  fn nearness_column_id() -> ColumnId {
    ColumnId(0x89ebc3ba9a24c286)
  }
}

type Nearness = ::collision_detection::Nearness<CollisionBasics>;

#[derive(Clone)]
struct Circle {
  position: QuadraticTrajectory,
  radius: SpaceCoordinate,
}
impl Column for Circle {
  type FieldType = Self;
  fn column_id() -> ColumnId {
    ColumnId(0x6422505ce8c8ce8e)
  }
}
struct Intersection {
  induced_acceleration: Vector2<SpaceCoordinate>,
}
impl Column for Intersection {
  type FieldType = Self;
  fn column_id() -> ColumnId {
    ColumnId(0x9357f021339198a1)
  }
}

type Steward = s::Steward<Basics>;

fn get_circle_id(index: i32) -> RowId {
  DeterministicRandomId::new(&(0x86ccbeb2c140cc51u64, index))
}

fn collision_predictor <PA: PredictorAccessor <Basics, <s::Steward <Basics> as TimeStewardStaticMethods < Basics>>::EventFn >> (accessor: &mut PA, id: RowId) {
  let ids = Nearness::get_ids(accessor, id).0;
  let time;
  {

    let us = (accessor.data_and_last_change::<Circle>(ids[0])
                      .expect("a nearness exists for a circle that doesn't"),
              accessor.data_and_last_change::<Circle>(ids[1])
                      .expect("a nearness exists for a circle that doesn't"));

    let relationship = accessor.get::<Intersection>(id);
    time = QuadraticTrajectory::approximately_when_distance_passes((us.0).0.radius +
                                                                   (us.1).0.radius,
                                                                   if relationship.is_none() {
                                                                     -1
                                                                   } else {
                                                                     1
                                                                   },
                                                                   ((us.0).1.clone(),
                                                                    &(us.0).0.position),
                                                                   ((us.1).1.clone(),
                                                                    &(us.1).0.position));
    printlnerr!("Planning for {} At {}, {}", id, (us.0).1, (us.1).1);
  }
  if let Some(yes) = time {
  printlnerr!(" planned for {}", & yes);
    accessor.predict_at_time(&yes,
                             Rc::new(move |mutator| {
                               let new_relationship;
                               let mut new;
                               {
                                 let relationship = mutator.get::<Intersection>(id).clone();
                                 let us = (mutator.data_and_last_change::<Circle>(ids[0])
                                                  .expect("a nearness exists for a circle that \
                                                           doesn't (event)"),
                                           mutator.data_and_last_change::<Circle>(ids[1])
                                                  .expect("a nearness exists for a circle that \
                                                           doesn't (event)"));
                                 new = ((us.0).0.clone(), (us.1).0.clone());
                                 new.0.position.update_by(mutator.now() - (us.0).1);
                                 new.1.position.update_by(mutator.now() - (us.1).1);
                                 if let Some(intersection) = relationship {
                                   new.0
                                      .position
                                      .add_acceleration(-intersection.induced_acceleration);
                                   new.1
                                      .position
                                      .add_acceleration(intersection.induced_acceleration);
                                   new_relationship = None;
                                 } else {
                                   let acceleration = (new.0.position.evaluate() - new.1.position.evaluate())*(ARENA_SIZE*4/(new.0.radius + new.1.radius));
                                   new.0.position.add_acceleration(acceleration);
                                   new.1.position.add_acceleration(-acceleration);
                                   new_relationship = Some(Intersection {
                                     induced_acceleration: acceleration,
                                   });

                                 }
                               }
                               mutator.set::<Intersection>(id, new_relationship);
                               mutator.set::<Circle>(ids[0], Some(new.0));
                               mutator.set::<Circle>(ids[1], Some(new.1));
                             }));
  }
}


fn boundary_predictor <PA: PredictorAccessor <Basics, <s::Steward <Basics> as TimeStewardStaticMethods < Basics>>::EventFn >> (accessor: &mut PA, id: RowId) {
  let ids = Nearness::get_ids(accessor, id).0;
  let time;
  {
let arena_center = QuadraticTrajectory::new(TIME_SHIFT,
                                                        [ ARENA_SIZE/2,
                                                          ARENA_SIZE/2,
0, 0, 0,
                                                         0]);
    let me = accessor.data_and_last_change::<Circle>(id)
                      .expect("a prediction was recorded for a circle that doesn't exist");

    let relationship = accessor.get::<Intersection>(id);
    time = QuadraticTrajectory::approximately_when_distance_passes(ARENA_SIZE - me.0.radius,
                                                                   if relationship.is_some() {
                                                                     -1
                                                                   } else {
                                                                     1
                                                                   },
(me.1.clone(),
                                                                    &(me.0.position)),
(0,
                                                                    &(arena_center )));
  }
  if let Some(yes) = time {
  printlnerr!(" planned for {}", & yes);
    accessor.predict_at_time(&yes,
                             Rc::new(move |mutator| {
                               let new_relationship;
                               let mut new;
                               {
                                 let relationship = mutator.get::<Intersection>(id).clone();
                                 let me = mutator.data_and_last_change::<Circle>(id)
                                                  .expect("a an event was recorded for a circle that doesn't exist)"); 
                                                  new = me.0.clone();
                                 new.position.update_by(mutator.now() - me.1);
                                 if let Some(intersection) = relationship {
                                   new
                                      .position
                                      .add_acceleration(-intersection.induced_acceleration);
                                   new_relationship = None;
                                 } else {
                                   let acceleration = - (new.position.evaluate() - Vector2::new (ARENA_SIZE/2,
                                                                                             ARENA_SIZE/2))*(ARENA_SIZE*4/(ARENA_SIZE - me.0.radius));
                                   new.position.add_acceleration(acceleration);
                                   new_relationship = Some(Intersection {
                                     induced_acceleration: acceleration,
                                   });

                                 }
                               }
                               mutator.set::<Intersection>(id, new_relationship);
                               mutator.set::<Circle>(id, Some(new));
                             }));
  }
}


#[derive(Copy, Clone)]

struct Vertex {
  direction: [f32; 2],
  center: [f32; 2],
  radius: f32,
}
implement_vertex!(Vertex, direction, center, radius);

pub fn testfunc() {

  let mut stew: Steward =
    ::TimeStewardStaticMethods::new_empty((),
                                          vec![s::Predictor {
                                                 predictor_id: PredictorId(0x5375592f4da8682c),
                                                 column_id: Nearness::column_id(),
                                                 function: Rc::new(|accessor, id| {
                                                   collision_predictor(accessor, id)
                                                 }),
                                               },
                                               s::Predictor {
                                                 predictor_id: PredictorId(0x87d8a4a095350d30),
                                                 column_id: Circle::column_id(),
                                                 function: Rc::new(|accessor, id| {
boundary_predictor(accessor, id)
                                                 }),
                                               }
                                               ]);

  stew.insert_fiat_event(-1,
                         DeterministicRandomId::new(&0),
                         Rc::new(|mutator| {
                           for i in 0..HOW_MANY_CIRCLES {
                             let thingy = ARENA_SIZE / 20;
                             let radius = mutator.rng().gen_range(ARENA_SIZE / 30, ARENA_SIZE / 15);
                             let id = get_circle_id(i);

                             let position =
                               QuadraticTrajectory::new(TIME_SHIFT,
                                                        [mutator.rng().gen_range(0, ARENA_SIZE),
                                                         mutator.rng().gen_range(0, ARENA_SIZE),
                                                         mutator.rng().gen_range(-thingy, thingy),
                                                         mutator.rng().gen_range(-thingy, thingy),
                                                         0,
                                                         0]);
                             mutator.set::<Circle>(id,
                                                   Some(Circle {
                                                     position: position,
                                                     radius: radius,
                                                   }));
                             collisions::insert::<CollisionBasics, _>(mutator, id, ());
                           }
                         }));
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

if true {
  let display = glium::glutin::WindowBuilder::new()
                  .with_dimensions(600, 600)
                  .build_glium()
                  .expect("failed to create window");
  let program = glium::Program::from_source(&display,
                                            vertex_shader_source,
                                            fragment_shader_source,
                                            None)
                  .expect("glium program generation failed");
  let indices = glium::index::NoIndices(glium::index::PrimitiveType::TrianglesList);
  let start = Instant::now();
  loop {
    for ev in display.poll_events() {
      match ev {
        glium::glutin::Event::Closed => return,
        _ => (),
      }
    }

    let mut target = display.draw();
    target.clear_color(0.0, 0.0, 0.0, 1.0);
    let mut vertices = Vec::<Vertex>::new();

    let snapshot = stew.snapshot_before(&(((start.elapsed().as_secs() as i64*1000000000i64)+start.elapsed().subsec_nanos() as i64) * SECOND /
                                          1000000000i64))
                       .expect("steward failed to provide snapshot");
    for index in 0..HOW_MANY_CIRCLES {
      let (circle, time) = snapshot.data_and_last_change::<Circle>(get_circle_id(index))
                                   .expect("missing circle")
                                   .clone();
      let position = circle.position.updated_by(snapshot.now() - time).evaluate();
      let center = [position[0] as f32 / ARENA_SIZE as f32 - 0.5, position[1] as f32 / ARENA_SIZE as f32 - 0.5];
      let radius = circle.radius as f32 / ARENA_SIZE as f32;
    //printlnerr!("drawing circ at {}, {}", center[0],center[1]);
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
    target.draw(&glium::VertexBuffer::new(&display, &vertices).expect("failed to generate glium Vertex buffer"),
                &indices,
                &program,
                &glium::uniforms::EmptyUniforms,
                &Default::default())
          .expect("failed target.draw");

    target.finish().expect("failed to finish drawing");

}
    sleep(Duration::from_millis(10));
  }


  for increment in 1..21 {
    snapshots.push(stew.snapshot_before(&(increment * SECOND * 2)));
  }
  for snapshot in snapshots.iter_mut().map(|option| {
    option.as_mut().expect("all these snapshots should have been valid")
  }) {
    printlnerr!("snapshot for {}", snapshot.now());
    for index in 0..HOW_MANY_CIRCLES {
      // printlnerr!("{}", snapshot.get::<Circle> (get_circle_id (index)).expect("missing circle").position);
    }
  }
  // panic!("anyway")
}

#[test]
fn actuallytest() {
  testfunc();
}
