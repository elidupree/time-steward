use polynomial::Polynomial;
use nalgebra::Vector2;
use std::cmp::max;
use roots::find_roots_quartic;


use std::io::Write;
macro_rules! printlnerr(
    ($($arg:tt)*) => { {
        let r = writeln!(&mut ::std::io::stderr(), $($arg)*);
        r.expect("failed printing to stderr");
    } }
);

fn multiply_and_shift_avoiding_overflow_by_rounding (stuff: & [Coordinate], shift: i32)->Coordinate {
let mut sign: Coordinate = 1;
let mut product = 1u64;
let mut shift = shift;
for thing in stuff {
let mut factor = thing.clone();
if factor <0 {factor*= -1; sign *= -1;}
let mut factor = factor as u64;
while product >= (1u64) << 32 && shift >0 {product >>= 1; shift -= 1;}
while factor >= (1u64) << 32 && shift >0 {factor >>= 1; shift -= 1;} 
//printlnerr!(" {}, {}, {}", product, factor, shift);
product = product*factor;
}
((product as Coordinate)*sign) >> shift
}

// TODO: polymorphic in number of dimensions, and numeric type
// TODO: optimize away the unnecessary heap-allocation and other inefficiencies of Polynomial, and other pointless inefficiencies I introduced
// TODO replace f64 with a deterministic type (probably mpfr)
// note: the third time is not acceleration, but the coefficient of X squared, which is 2 times acceleration
pub type Coordinate = i64;
#[derive (Clone,)]
pub struct QuadraticTrajectory {
  data: Vector2<Polynomial<Coordinate>>,
  time_scale_shift: i32,
}
fn updated_by(poly: &Polynomial<Coordinate>,
              time: Coordinate,
              time_scale_shift: i32)
              -> Polynomial<Coordinate> {
  &*poly +
  Polynomial::new(vec![multiply_and_shift_avoiding_overflow_by_rounding (& [poly.data().get(1).cloned().unwrap_or(0), time], time_scale_shift) +
                       multiply_and_shift_avoiding_overflow_by_rounding (& [poly.data().get(2).cloned().unwrap_or(0), time, time], time_scale_shift * 2),
                       multiply_and_shift_avoiding_overflow_by_rounding (& [poly.data().get(2).cloned().unwrap_or(0), time * 2], time_scale_shift),
                       0i64])
}
impl QuadraticTrajectory {
  pub fn new(time_scale_shift: i32, coordinates: [Coordinate; 6]) -> QuadraticTrajectory {
    QuadraticTrajectory {
      time_scale_shift: time_scale_shift,
      data: Vector2::new(Polynomial::new(vec![coordinates[0], coordinates[2], coordinates[4]]),
                         Polynomial::new(vec![coordinates[1], coordinates[3], coordinates[5]])),
    }
  }

  pub fn updated_by(&self, time: Coordinate) -> QuadraticTrajectory {
    QuadraticTrajectory {
      data: Vector2::new(updated_by(&self.data[0], time, self.time_scale_shift),
                         updated_by(&self.data[1], time, self.time_scale_shift)),
      ..self.clone()
    }
  }

  pub fn update_by(&mut self, time: Coordinate) {
    *self = self.updated_by(time);
  }

  pub fn add_acceleration(&mut self, acceleration: Vector2<Coordinate>) {
    let changed = &self.data[0] + Polynomial::new(vec![0, 0, acceleration[0]*2]);
    self.data[0].clone_from(&changed);
    let changed = &self.data[1] + Polynomial::new(vec![0, 0, acceleration[1]*2]);
    self.data[1].clone_from(&changed);
  }
  pub fn evaluate(&self) -> Vector2<Coordinate> {
    Vector2::new(self.data[0].data().get(0).cloned().unwrap_or(0),
                 self.data[1].data().get(0).cloned().unwrap_or(0))

  }

  pub fn approximate_starting_distance_squared (first: (Coordinate, &QuadraticTrajectory),
                                            second: (Coordinate, &QuadraticTrajectory))
                                            ->Coordinate {
    assert!(first.1.time_scale_shift == second.1.time_scale_shift,
            "we don't actually support interactions between trajectories with different scales");

    let base = max(first.0, second.0);
    let third = first.1.updated_by(base - first.0);
    let more = second.1.updated_by(base - second.0);
        let checker = third.data.clone() - more.data.clone();

        let real_distance_squared = checker[0].data().get(0).cloned().unwrap_or(0) *
                                    checker[0].data().get(0).cloned().unwrap_or(0) +
                                    checker[1].data().get(0).cloned().unwrap_or(0) *
                                    checker[1].data().get(0).cloned().unwrap_or(0);
real_distance_squared
  }
  

  // direction == -1->"when the distance between the trajectories drops below the distance argument"
  // direction == 1->"when the distance between the trajectories exceeds the distance argument"
  pub fn approximately_when_distance_passes(distance: Coordinate,
                                            direction: Coordinate,
                                            first: (Coordinate, &QuadraticTrajectory),
                                            second: (Coordinate, &QuadraticTrajectory))
                                            -> Option<Coordinate> {
    assert!(first.1.time_scale_shift == second.1.time_scale_shift,
            "we don't actually support interactions between trajectories with different scales");
    let error = distance * 12 + 6;
    let ratio = (1i64 << first.1.time_scale_shift) as f64;

    let base = max(first.0, second.0);
    let third = first.1.updated_by(base - first.0);
    let more = second.1.updated_by(base - second.0);
    let displacement_function: Vector2<Polynomial<Coordinate>> = third.data.clone() - more.data.clone();
    let distance_function = &displacement_function[0] * &displacement_function[0] +
                            &displacement_function[1] * &displacement_function[1];
                            //printlnerr!(" distance function {:?} {:?} {:?} {:?} {:?} {:?} {:?} {:?}", first.1.data, second.1.data, error, ratio, third.data, more.data, displacement_function, distance_function);
    let roots = find_roots_quartic(distance_function.data().get(4).cloned().unwrap_or(0) as f64 /
                                   ratio.powi(4),
                                   distance_function.data().get(3).cloned().unwrap_or(0) as f64 /
                                   ratio.powi(3),
                                   distance_function.data().get(2).cloned().unwrap_or(0) as f64 /
                                   ratio.powi(2),
                                   distance_function.data().get(1).cloned().unwrap_or(0) as f64 /
                                   ratio.powi(1),
                                   (distance_function.data().get(0).cloned().unwrap_or(0) -
                                    (distance * distance +
                                     error * direction)) as f64);

    for root in roots.as_ref() {
    //printlnerr!(" found group {}", root);
      let result = root.ceil() as Coordinate;
      if result > 0 {
    let third = first.1.updated_by(base + result - first.0);
    let more = second.1.updated_by(base + result - second.0);
        let checker = third.data.clone() - more.data.clone();
        //Vector2::new(updated_by(&displacement_function[0],
                                              //result,
                                              //first.1.time_scale_shift),
                                   //updated_by(&displacement_function[1],
                                              //result,
                                              //first.1.time_scale_shift));

        let real_distance_squared = checker[0].data().get(0).cloned().unwrap_or(0) *
                                    checker[0].data().get(0).cloned().unwrap_or(0) +
                                    checker[1].data().get(0).cloned().unwrap_or(0) *
                                    checker[1].data().get(0).cloned().unwrap_or(0);

        if (real_distance_squared - distance * distance) * direction > 0 {
        //printlnerr!(" check past");
          return Some(base + result);
        }
      }
    }
    None
  }
}
