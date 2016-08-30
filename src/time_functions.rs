use nalgebra::{Vector2, Dot};
use std::cmp::max;
use ::rounding_error_tolerant_math::*;


use std::io::Write;
macro_rules! printlnerr(
    ($($arg:tt)*) => { {
        let r = writeln!(&mut ::std::io::stderr(), $($arg)*);
        r.expect("failed printing to stderr");
    } }
);

// TODO: polymorphic in number of dimensions, and numeric type
// TODO: optimize away the pointless inefficiencies I introduced
// note: the third time is not acceleration, but the coefficient of X squared, which is 2 times acceleration
pub type Coordinate = i64;
#[derive (Clone, Debug, Serialize, Deserialize)]
pub struct QuadraticTrajectory {
  data: [[Coordinate; 3]; 2],
  time_scale_shift: u32,
  max_distance_traveled_at_once: i64,
}

fn distance_squared_would_be(first: (Coordinate, &QuadraticTrajectory),
                             second: (Coordinate, &QuadraticTrajectory),
                             when: Coordinate)
                             -> Option<Coordinate> {
  if let Some(third) = first.1.updated_by(when - first.0) {
    if let Some(more) = second.1.updated_by(when - second.0) {
      let displacement = third.evaluate() - more.evaluate();
      return Some(displacement.dot(&displacement));
    }
  }
  None
}

impl QuadraticTrajectory {
  pub fn new(time_scale_shift: u32,
             max_distance_traveled_at_once: i64,
             coordinates: [Coordinate; 6])
             -> QuadraticTrajectory {
    QuadraticTrajectory {
      time_scale_shift: time_scale_shift,
      max_distance_traveled_at_once: max_distance_traveled_at_once,
      data: [[coordinates[0], coordinates[2], coordinates[4]],
             [coordinates[1], coordinates[3], coordinates[5]]],
    }
  }

  pub fn updated_by(&self, time: Coordinate) -> Option<QuadraticTrajectory> {
    let mut result = self.clone();
    for quadratic in result.data.iter_mut() {
      if !quadratic_move_origin_rounding_change_towards_0(quadratic.as_mut(),
                                                              time,
                                                              self.time_scale_shift,max_error_for_distance_traveled (self.max_distance_traveled_at_once)) {return None;}
    }
    Some(result)
  }

  pub fn update_by(&mut self, time: Coordinate) {
    for quadratic in self.data.iter_mut() {
      assert!(quadratic_move_origin_rounding_change_towards_0(quadratic.as_mut(),
                                                              time,
                                                              self.time_scale_shift,max_error_for_distance_traveled (self.max_distance_traveled_at_once)));
    }
  }

  pub fn add_acceleration(&mut self, acceleration: Vector2<Coordinate>) {
    self.data[0][2] += acceleration[0] * 2;
    self.data[1][2] += acceleration[1] * 2;
  }
  pub fn evaluate(&self) -> Vector2<Coordinate> {
    Vector2::new(self.data[0][0], self.data[1][0])

  }

  // Bounds are min and max for the first dimension, then min and max for the second dimension
  pub fn approximately_when_escapes(&self,
                                    base: Coordinate,
                                    now: Coordinate,
                                    bounds: [[Coordinate; 2]; 2])
                                    -> Option<Coordinate> {
    time_until_which_quadratic_trajectory_may_remain_in_bounds (now - base, & self.data, & bounds, self.time_scale_shift, max_error_for_distance_traveled (self.max_distance_traveled_at_once)).map (| time | time + base)
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
    assert!(first.1.max_distance_traveled_at_once == second.1.max_distance_traveled_at_once,
            "we don't actually support interactions between trajectories with different error \
             limits");


    let base = max(first.0, second.0);
    if (distance_squared_would_be(first, second, base)
          .expect("we shouldn't have already maxed out our distance traveled at the \
                   BEGINNING of the test!") - distance * distance) * direction > 0 {
      printlnerr!("rejefoo");
      return Some(base);
    }

    let intervals =
      quadratic_trajectories_possible_distance_crossing_intervals(distance,
                                                                  (first.0,
                                                                   &[first.1.data[0],
                                                                     first.1.data[1]]),
                                                                  (second.0,
                                                                   &[second.1.data[0],
                                                                     second.1.data[1]]),
                                                                  first.1.time_scale_shift,max_error_for_distance_traveled (first.1.max_distance_traveled_at_once));
    for interval in intervals.iter() {
      if interval.max() != i64::max_value() && interval.max() + 1 > base {
        if let Some(future_distance_squared) = distance_squared_would_be(first,
                                                                         second,
                                                                         interval.max() + 1) {
          if (future_distance_squared - distance * distance) * direction > 0 {
            return Some(interval.max() + 1);
          } //else {printlnerr!("rejected interval for not ending in the correct position")}
        }
      } //else {printlnerr!("rejected interval for being in the past")}
    }
    None
  }
}
