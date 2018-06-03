use nalgebra::Vector2;
use std::cmp::max;
use super::rounding_error_tolerant_math::*;
use std::ops::{Add, Sub, Mul, Neg};
use array_ext::*;


pub type Time = i64;

pub trait IntegerVector: Clone + Add <Self, Output = Self> + Sub <Self, Output = Self> + Mul <i64, Output = Self> + Zero + {
  type DistanceProxy: Scalar;
  fn distance_proxy (&self, other: & Self)->Self::DistanceProxy;
}

pub trait ScalarTrajectory: Trajectory <Value: Scalar> {
  /// 
  fn non_strict_bounds (&self, input_range: [Time; 2])->[T; 2];
  fn next_time_ge (&self, start: Time, end: Time, target: Self::Value)->Option <Time> {
    //default impl that works for all locally Lipschitz functions; can optimize more in explicit impls
    let mut current_value = self.evaluate (start) ;
    if current_value >= target {return Some (start)}
    let mut start = start;
    let mut increment = Self::Value::one();
    loop {
      let [max,_] = self.slope_range ([start, start + increment]);
      if max >= target {increment >>= 1; break;}
      if start + increment == end {increment = end - start; break;}
      increment <<= 1;
      if start + increment > end {increment = end - start;}
    }
    loop {
      start += increment;
      current_value = self.evaluate (start);
      if current_value >= target {return Some (start)}
      if start == end {return None;}
      if start + increment > end {increment = end - start;}
      loop {
        let [max,_] = self.slope_range ([start, start + increment]);
        if max >= target {increment >>= 1;}
        else {break;}
      }
    }
  }
  pub fn next_time_gt (&self, start: Time, end: Time, target: Self::Value)->Option <Time> {
    self.next_time_ge (now, value + Vector::one())
  }
  pub fn next_time_(&self, start: Time, end: Time, target: Self::Value)->Option <Time> {
    (-self).next_time_ge (now, -value)
  }
  pub fn next_time_lt (&self, start: Time, end: Time, target: Self::Value)->Option <Time> {
    self.next_time_le (now, value - Vector::one())
  }
}

pub struct DistanceProxyTrajectory <'a, Vector: IntegerVector, T: Trajectory <Vector>> {
  first: & 'a T,
  second: & 'a T,
}
impl <'a, Vector: IntegerVector, T: Trajectory <Vector>> ScalarTrajectory <Vector::DistanceProxy> for DistanceProxyTrajectory <'a, Vector, T> {
  fn non_strict_bounds (&self, input_range: [Time; 2])->[Self::Value; 2] {
    {
      let first_bounds = self.first.component (dimension).non_strict_bounds (input_range);
      let second_bounds = self.second.component (dimension).non_strict_bounds (input_range);
      max_vector [dimension] = max (first_bounds [1] - second_bounds [0], second_bounds [1] - first_bounds [0]);
      min_vector [dimension] = if bounds_intersect (first_bounds, second_bounds) {Self::Value::zero()} else {
    [min_vector.distance_proxy(), max_vector.distance_proxy()]
  }
}

pub trait Trajectory: {
  type Value: IntegerVector;
  fn distance_proxy_trajectory (&self, other: & Self)->impl ScalarTrajectory <T::Component> {
  
  }
}

macro_rules! impl_trajectory_binop {
  ($Trajectory: ident, $Trait: ident, $method: ident) => {

impl <Vector: Clone + $Trait <Vector, Output = Vector>> $Trait for $Trajectory <Vector> {
  type Output = Self;
  fn $method (self, other: Self)->Self {
    $Trajectory {
      origin: self.origin,
      terms: Array::from_fn (| index | self.terms [index].clone().$method (other.terms [index].clone())),
    }
  }
}


impl <'a, Vector: Clone + $Trait <& 'a Vector, Output = Vector>> $Trait <& 'a $Trajectory <Vector>> for $Trajectory <Vector> {
  type Output = Self;
  fn $method (self, other: & 'a $Trajectory <Vector>)->Self {
    $Trajectory {
      origin: self.origin,
      terms: Array::from_fn (| index | self.terms [index].clone().$method (& other.terms [index])),
    }
  }
}

impl <'a, Vector: Clone> $Trait <$Trajectory <Vector>> for & 'a $Trajectory <Vector> where & 'a Vector: $Trait <Vector, Output = Vector> {
  type Output = $Trajectory <Vector>;
  fn $method (self, other: $Trajectory <Vector>)->$Trajectory <Vector> {
    $Trajectory {
      origin: self.origin,
      terms: Array::from_fn (| index | (& self.terms [index]).$method (other.terms [index].clone())),
    }
  }
}

impl <'a, 'b, Vector> $Trait <& 'b $Trajectory <Vector>> for & 'a $Trajectory <Vector> where & 'a Vector: $Trait <& 'b Vector, Output = Vector> {
  type Output = $Trajectory <Vector>;
  fn $method (self, other: & 'b $Trajectory <Vector>)->$Trajectory <Vector> {
    $Trajectory {
      origin: self.origin,
      terms: Array::from_fn (| index | (& self.terms [index]).clone().$method (& other.terms [index])),
    }
  }
}

  }
}

macro_rules! impl_trajectory_add_sub {


impl_binop ($Trait, $method, $Trajectory <Vector>, $Trajectory <Vector>, $Trajectory <Vector>, self, other, {
  //always let [$($first_identifiers),*] = self.terms;
  let [$($second_identifiers),*] = other.terms or & other.terms;
  $Trajectory {
    origin: self.origin,
    terms: [$($first_identifiers.method ($second_identifiers))*]
  }
});

}

impl_binop ($Trait, $method, $Trajectory <Vector>, Scalar, $Trajectory <Vector>, self, other, {
  $Trajectory {
    origin: self.origin,
    terms: [$($first_identifiers.method (other))*]
  }
});






macro_rules! impl_trajectory {
  ($Terms: ident, $Trajectory: ident, $degree: expr) => {

#[derive (Clone, PartialEq, Eq, Hash, Serialize, Deserialize, Debug, Default)]
pub struct $Terms <Vector> ([Vector; $degree + 1]);

#[derive (Clone, PartialEq, Eq, Hash, Serialize, Deserialize, Debug, Default)]
pub struct $Trajectory <Vector> {
  origin: Time,
  terms: $Terms <Vector>,
}

impl <Vector> $Terms <Vector> {
  pub fn constant (value: Vector)->Self {
    let mut array = [Vector::zero()];
    array [0] = value;
    $Terms (array)
  }
  pub fn term (&self, time: Time, which: usize)->Vector {
    let mut factor = 1;
    let mut result = terms [which].clone();
    for index in (which + 1)..($degree + 1) {
      factor *= time;
      result += terms [index]*(factor*binomial_coefficient (index, which));
    }
    result
  }
  pub fn set_origin (&mut self, new_origin: Time) {
    for index in 0.. $degree {
      self.0 [index] = self.taylor_coefficient (index, new_origin) ;
    }
  }
}

impl $Trajectory <Vector> {
  pub fn constant (value: Vector)->Self {
    $Trajectory {origin: Vector::zero(), terms: $Terms::constant (value)}
  }
  pub fn set_origin (&mut self, new_origin: Time) {
    self.terms.set_origin (new_origin - self.origin);
    self.origin = new_origin;
  }
  pub fn term (&self, time: Time, which: usize)->Vector {
    self.terms.taylor_coefficient (time - self.origin, which)
  }
  pub fn set_term (&mut self, time: Time, which: usize, value: Vector) {
    self.set_origin (time) ;
    self.terms.0 [which] = value;
  }
  pub fn add_term (&mut self, time: Time, which: usize, value: Vector) {
    self.set_origin (time);
    self.terms.0 [which] += value;
  }
  pub fn value (&mut self, time: Time)->Vector {self.term (time, 0)}
  pub fn velocity (&mut self, time: Time)->Vector {self.term (time, 1)}
  pub fn set_value (&mut self, time: Time, value: Vector) {self.set_term (time, 0, value)}
  pub fn set_velocity (&mut self, time: Time, value: Vector) {self.set_term (time, 1, value)}
  pub fn add_value (&mut self, time: Time, value: Vector) {self.add_term (time, 0, value)}
  pub fn add_velocity (&mut self, time: Time, value: Vector) {self.add_term (time, 1, value)}
}

impl <Vector: Ord + Div <Vector >> Trajectory <Vector> {
  pub fn next_time_lt (now: Time, value: Vector)->Option <Time> {
    if self.value (now) <value {Some (now)}
    else if $degree >= 2 && self.terms.0 [2] != Vector::zero() {
      unimplemented!()
    }
    else {
      if self.terms.0 [1] <= Vector::zero() {None}
      else {Some (self.origin + div_ceil ((value - self.terms.0 [0]), self.terms.0 [1]))}
    }
  }
  pub fn next_time_le (now: Time, value: Vector)->Option <Time> {
    self.next_time_lt (now, value + Vector::one())
  }
  pub fn next_time_gt (now: Time, value: Vector)->Option <Time> {
    (-self).next_time_lt (now, -value)
  }
  pub fn next_time_ge (now: Time, value: Vector)->Option <Time> {
    self.next_time_gt (now, value - Vector::one())
  }
}

impl_trajectory_arithmetic_trait! ($Trajectory, Add, add);
impl_trajectory_arithmetic_trait! ($Trajectory, Sub, sub);

impl <Vector: Clone + Neg <Output = Vector>> Neg for $Trajectory <Vector> {
  type Output = Self;
  fn neg (self)->Self {
    $Trajectory {
      origin: self.origin,
      terms: Array::from_fn (| index | self.terms [index].clone().neg()),
    }
  }
}
impl <'a, Vector> Neg for & 'a $Trajectory <Vector> where & 'a Vector: Neg <Output = Vector> {
  type Output = $Trajectory <Vector>;
  fn neg (self)->$Trajectory <Vector> {
    $Trajectory {
      origin: self.origin,
      terms: Array::from_fn (| index | (& self.terms [index]).neg()),
    }
  }
}


  };
}

impl_trajectory! (LinearTrajectoryTerms, LinearTrajectory, 1) ;
impl_trajectory! (QuadraticTrajectory, 2) ;


