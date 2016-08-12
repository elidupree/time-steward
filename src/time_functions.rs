use polynomial::Polynomial;
use nalgebra::{Vector2};
use std::cmp::{max};
use roots::find_roots_quartic;

//TODO: polymorphic in number of dimensions, and numeric type
//TODO: optimize away the unnecessary heap-allocation and other inefficiencies of Polynomial, and other pointless inefficiencies I introduced
//TODO replace f64 with a deterministic type (probably mpfr)
//note: the third time is not acceleration, but the coefficient of X squared, which is 2 times acceleration
pub type Coordinate = i64;
#[derive (Clone,)]
pub struct QuadraticTrajectory {
  data: Vector2 <Polynomial <Coordinate>>,
  time_scale_shift: i32,
  } 
fn update_by (poly: &mut Polynomial <Coordinate> , time: Coordinate, time_scale_shift: i32) {
  let changed =&*poly + Polynomial::new (vec![((poly.data().get (1).cloned().unwrap_or (0)*time) >> time_scale_shift) + ((poly.data().get (2).cloned().unwrap_or (0)*time*time) >> (time_scale_shift*2)), ((poly.data().get (2).cloned().unwrap_or (0)*time*2) >> time_scale_shift), 0i64]);
  poly.clone_from (& changed);
  } 
impl QuadraticTrajectory {
  pub fn new (time_scale_shift: i32, coordinates: [Coordinate; 6])->QuadraticTrajectory {QuadraticTrajectory {
  time_scale_shift:time_scale_shift,
  data: Vector2::new (
  Polynomial::new (
  vec![ coordinates [0], coordinates [2], coordinates [4]])
  , Polynomial::new (vec![coordinates [1], coordinates [3], coordinates [5]]
  ))}}
  
  pub fn update_by (&mut self, time: Coordinate) {
    update_by (&mut self.data [0], time, self.time_scale_shift);
    update_by (&mut self.data [1], time, self.time_scale_shift);
  }
  pub fn add_acceleration (&mut self, acceleration: Vector2 <Coordinate>) {
    let changed =&self.data [0]+ Polynomial::new (vec![0, 0, acceleration [0]]);
    self.data [0].clone_from (& changed);
    let changed =&self.data [1]+ Polynomial::new (vec![0, 0, acceleration [1]]);
    self.data [1].clone_from (& changed);
  }
  
  //direction == -1->"when the distance between the trajectories drops below the distance argument"
  //direction == 1->"when the distance between the trajectories exceeds the distance argument"
  pub fn approximately_when_distance_passes (
distance: Coordinate, direction: Coordinate,
first: (Coordinate, & QuadraticTrajectory), second: (Coordinate, & QuadraticTrajectory),)->Option <Coordinate> {
assert! (first.1.time_scale_shift == second.1.time_scale_shift, "we don't actually support interactions between trajectories with different scales");
let error =//distance can't be off by more than 3, so distance*distance can't be off by more than 3 + distance*6
distance*6 + 3;
let ratio = (1i64 << first.1.time_scale_shift) as f64;

    let base = max (first.0, second.0);
let mut third = first.1.clone(); third.update_by (base - first.0);
let mut more = second.1.clone(); more.update_by (base - second.0);
let displacement_function:Vector2 <Polynomial <Coordinate>> = third.data - more.data;
let distance_function = & displacement_function [0]*& displacement_function [0] + & displacement_function [1]*& displacement_function [1];
let roots = find_roots_quartic (
distance_function .data().get (4).cloned().unwrap_or (0) as f64/ratio.powi(4),
distance_function .data().get (3).cloned().unwrap_or (0) as f64/ratio.powi(3),
distance_function .data().get (2).cloned().unwrap_or (0) as f64/ratio.powi(2),
distance_function .data().get (1).cloned().unwrap_or (0) as f64/ratio.powi(1),
 (distance_function .data().get (0).cloned().unwrap_or (0) - (distance*distance + error*direction)) as f64);

for root in roots.as_ref() {
let result =root.ceil() as Coordinate;
if result >0 {
let mut checker = displacement_function.clone(); update_by (&mut checker [0], result,first.1.time_scale_shift); update_by (&mut checker [1], result,first.1.time_scale_shift);

let real_distance_squared = checker [0].data().get (0).cloned().unwrap_or (0)*checker [0].data().get (0).cloned().unwrap_or (0) + checker [1].data().get (0).cloned().unwrap_or (0)*checker [1].data().get (0).cloned().unwrap_or (0);

if (real_distance_squared - distance*distance)*direction >0 {
return Some (base + result);
}}}
 None
}
}
