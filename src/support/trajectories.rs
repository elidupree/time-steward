//use nalgebra::Vector2;
//use std::cmp::max;
use num::FromPrimitive;
use num::traits::{Signed, Bounded};
use super::integer_math::*;
use std::ops::{Add, Sub, Mul, Neg, AddAssign, SubAssign, MulAssign};
use array_ext::*;
use smallvec::SmallVec;


pub type Time = i64;


pub trait Trajectory {
  type Coefficient: Vector;
}

pub trait ScalarTrajectory: Trajectory {
  /*fn next_time_ge (&self, start: Time, end: Time, target: Self::Coefficient)->Option <Time> {

  }
  fn next_time_gt (&self, start: Time, end: Time, target: Self::Coefficient)->Option <Time> {
    self.next_time_ge (now, target + Vector::one())
  }
  fn next_time_le (&self, start: Time, end: Time, target: Self::Coefficient)->Option <Time> {
    (-self).next_time_ge (now, -target)
  }
  fn next_time_lt (&self, start: Time, end: Time, target: Self::Coefficient)->Option <Time> {
    self.next_time_le (now, target - Vector::one())
  }*/
}

macro_rules! impl_binop {
  ([$($generic_parameters: tt)*], $Left:ty, $Right:ty, $Trait: ident, $method: ident, $TraitAssign: ident, $method_assign: ident, $self: ident, $other: ident, $owned_owned: expr, $owned_ref: expr, $ref_owned: expr, $ref_ref: expr, $assign_owned: expr, $assign_ref: expr,) => {

impl <$($generic_parameters)*> $Trait <$Right> for $Left where Time: From <T::Coordinate> {
  type Output = Self;
  fn $method (mut $self, $other: $Right)->Self {
    $owned_owned
  }
}

impl <'a, $($generic_parameters)*> $Trait <& 'a $Right> for $Left where Time: From <T::Coordinate> {
  type Output = Self;
  fn $method (mut $self, $other: & 'a $Right)->Self {
    $owned_ref
  }
}

impl <'a, $($generic_parameters)*> $Trait <$Right> for & 'a $Left where Time: From <T::Coordinate> {
  type Output = $Left;
  fn $method ($self, $other: $Right)->$Left {
    $ref_owned
  }
}

impl <'a, 'b, $($generic_parameters)*> $Trait <& 'b $Right> for & 'a $Left where Time: From <T::Coordinate> {
  type Output = $Left;
  fn $method ($self, $other: & 'b $Right)->$Left {
    $ref_ref
  }
}

impl <$($generic_parameters)*> $TraitAssign <$Right> for $Left where Time: From <T::Coordinate> {
  fn $method_assign (&mut $self, $other: $Right) {
    $assign_owned
  }
}

impl <'a, $($generic_parameters)*> $TraitAssign <& 'a $Right> for $Left where Time: From <T::Coordinate> {
  fn $method_assign (&mut $self, $other: & 'a $Right) {
    $assign_ref
  }
}

  }
}

macro_rules! impl_trajectory_add_sub {
  ($Trajectory:ident, $Trait: ident, $method: ident, $TraitAssign: ident, $method_assign: ident) => {

impl_binop! {
[T: Vector], $Trajectory <T>, $Trajectory <T>, $Trait, $method, $TraitAssign, $method_assign, self, other,
{
  self += other;
  self
},
{
  self += other;
  self
},
{
  self.clone() + other
},
{
  self.clone() + other.clone()
},
{
  let mut other = other;
  if self.origin < other.origin {self.set_origin (other.origin).unwrap();}
  if other.origin < self.origin {other.set_origin (self.origin).unwrap();}
  for (mine, others) in self.coefficients.iter_mut().zip (other.coefficients.into_iter()) {
    mine.$method_assign (others);
  }
},
{
  let mut other_clone;
  let mut other = other;
  if self.origin < other.origin {self.set_origin (other.origin).unwrap();}
  if other.origin < self.origin {
    other_clone = other.clone();
    other_clone.set_origin (self.origin).unwrap();
    other = & other_clone;
  }
  for (mine, others) in self.coefficients.iter_mut().zip (other.coefficients.iter()) {
    mine.$method_assign (others);
  }
},
}


impl_binop! {
[T: Vector], $Trajectory <T>, T, $Trait, $method, $TraitAssign, $method_assign, self, other,
{
  self.coefficients [0].$method_assign (other);
  self
},
{
  self.coefficients [0].$method_assign (other);
  self
},
{
  self.clone() + other
},
{
  self.clone() + other
},
{
  self.coefficients [0].$method_assign (other);
},
{
  self.coefficients [0].$method_assign (other);
},
}


  }
}






macro_rules! impl_trajectory {
  ($Trajectory: ident, $degree: expr) => {

#[derive (Clone, PartialEq, Eq, Hash, Serialize, Deserialize, Debug, Default)]
pub struct $Trajectory <T> {
  origin: Time,
  coefficients: [T; $degree + 1],
}

impl <T: Vector> $Trajectory <T> where Time: From <T::Coordinate> {
  pub fn constant (value: T)->Self {
    let mut coefficients = [T::zero(); $degree + 1];
    coefficients [0] = value;
    $Trajectory {origin: 0, coefficients}
  }
  pub fn set_origin (&mut self, new_origin: Time)->Result <(), polynomial::OverflowError> {
    for dimension in 0..T::DIMENSIONS {
      let coefficients = self.coordinate_coefficients (dimension);
      polynomial::translate_check (&coefficients, new_origin - self.origin, |_| i64::max_value())?;
    }
    for dimension in 0..T::DIMENSIONS {
      let mut coefficients = self.coordinate_coefficients (dimension);
      polynomial::translate_unchecked (&mut coefficients, T::Coordinate::from_i64 (new_origin - self.origin)?);
      for (coefficient, destination) in coefficients.iter().zip (self.coefficients.iter_mut()) {
        destination.set_coordinate (dimension, *coefficient);
      }
    }
    self.origin = new_origin;
    Ok (())
  }
  
  pub fn coordinate_coefficients (&self, dimension: usize)->[T::Coordinate; $degree + 1] {
    Array::from_fn (| which | self.coefficients [which].coordinate (dimension))
  }
  pub fn coordinate_trajectory (&self, dimension: usize)->$Trajectory <T::Coordinate> {
    $Trajectory {
      origin: self.origin,
      coefficients: self.coordinate_coefficients (dimension),
    }
  }
  
  
  pub fn nth_coefficient (&self, which: usize, time_numerator: Time, time_shift: u32)->Result <T, polynomial::OverflowError> {
    let mut result = T::zero();
    for dimension in 0..T::DIMENSIONS {
      result.set_coordinate(dimension, T::Coordinate::from_i64 (polynomial::evaluate_nth_taylor_coefficient_at_fractional_input (& self.coordinate_coefficients (dimension), which, time_numerator - (self.origin << time_shift), time_shift)?)?) ;
    }
    Ok (result)
  }
  pub fn set_nth_coefficient (&mut self, which: usize, time_numerator: Time, time_shift: u32, target_value: T)->Result <(), polynomial::OverflowError> {
    self.set_origin (time_numerator >> time_shift)?;
    let mut transformed_coefficients: SmallVec<[[T::Coordinate; $degree + 1]; 4]> = SmallVec::with_capacity (T::DIMENSIONS);
    for dimension in 0..T::DIMENSIONS {
      let mut coefficients = self.coordinate_coefficients (dimension);
      polynomial::set_nth_taylor_coefficient_at_fractional_input (&mut coefficients, which, time_numerator - (self.origin << time_shift), time_shift, target_value.coordinate (dimension).into())?;
      transformed_coefficients.push (coefficients);
    }
    for (dimension, coefficients) in transformed_coefficients.into_iter().enumerate() {
      for (coefficient, destination) in coefficients.iter().zip (self.coefficients.iter_mut()).take (which + 1) {
        destination.set_coordinate (dimension, *coefficient);
      }
    }
    Ok (())
  }
  pub fn add_nth_coefficient (&mut self, which: usize, time_numerator: Time, time_shift: u32, added_value: T)->Result <(), polynomial::OverflowError> {
    let current_value = self.nth_coefficient (which, time_numerator, time_shift)?;
    self.set_nth_coefficient (which, time_numerator, time_shift, current_value + added_value)
  }
  pub fn value (&mut self, time_numerator: Time, time_shift: u32)->Result <T, polynomial::OverflowError> {self.nth_coefficient (0, time_numerator, time_shift)}
  pub fn velocity (&mut self, time_numerator: Time, time_shift: u32)->Result <T, polynomial::OverflowError> {self.nth_coefficient (1, time_numerator, time_shift)}
  pub fn set_value (&mut self, time_numerator: Time, time_shift: u32, value: T)->Result <(), polynomial::OverflowError> {self.set_nth_coefficient (0, time_numerator, time_shift, value)}
  pub fn set_velocity (&mut self, time_numerator: Time, time_shift: u32, value: T)->Result <(), polynomial::OverflowError> {self.set_nth_coefficient (1, time_numerator, time_shift, value)}
  pub fn add_value (&mut self, value: T) {*self += value}
  pub fn add_velocity (&mut self, time_numerator: Time, time_shift: u32, value: T)->Result <(), polynomial::OverflowError> {self.add_nth_coefficient (1, time_numerator, time_shift, value)}
}

impl <T: Integer + Signed> $Trajectory <T> {
  /*pub fn next_time_lt (&self, now: Time, value: T)->Option <Time> {
    if self.value (now) < value {Some (now)}
    else {
      unimplemented!()
    }
  }
  pub fn next_time_le (&self, now: Time, value: T)->Option <Time> {
    self.next_time_lt (now, value + T::one())
  }
  pub fn next_time_gt (&self, now: Time, value: T)->Option <Time> {
    (-self).next_time_lt (now, -value)
  }
  pub fn next_time_ge (&self, now: Time, value: T)->Option <Time> {
    self.next_time_gt (now, value - T::one())
  }*/
}

impl_trajectory_add_sub! ($Trajectory, Add, add, AddAssign, add_assign);
impl_trajectory_add_sub! ($Trajectory, Sub, sub, SubAssign, sub_assign);

impl<T: Vector, Coordinate: Copy> Mul<Coordinate> for $Trajectory<T> where T: MulAssign <Coordinate> {
  type Output = Self;
  fn mul(mut self, other: Coordinate) -> Self {
    self *= other; self
  }
}

impl<'a, T: Vector, Coordinate: Copy> Mul<Coordinate> for & 'a $Trajectory<T> where T: MulAssign <Coordinate> {
  type Output = $Trajectory <T>;
  fn mul(self, other: Coordinate) ->$Trajectory <T> {
    self.clone()*other
  }
}

impl<T: Vector, Coordinate: Copy> MulAssign <Coordinate> for $Trajectory<T> where T: MulAssign <Coordinate> {
  fn mul_assign (&mut self, other: Coordinate) {
    for coefficient in self.coefficients.iter_mut (){MulAssign::<Coordinate>::mul_assign (coefficient, other);}
  }
}

/*impl_binop! {
[T: Vector], $Trajectory <T>, T::Coordinate, Mul, mul, MulAssign, mul_assign, self, other,
{
  self *= other;
  self
},
{
  self *= other;
  self
},
{
  self.clone()*other
},
{
  self.clone()*other
},
{
  for term in self.terms.iter_mut (){*term *= other;}
},
{
  for term in self.terms.iter_mut (){*term *= other;}
},
}*/

impl <T: Vector + Neg <Output = T>> Neg for $Trajectory <T> {
  type Output = Self;
  fn neg (self)->Self {
    $Trajectory {
      origin: self.origin,
      coefficients: Array::from_fn (| index | self.coefficients [index].clone().neg()),
    }
  }
}
impl <'a, T: Vector> Neg for & 'a $Trajectory <T> where & 'a T: Neg <Output = T> {
  type Output = $Trajectory <T>;
  fn neg (self)->$Trajectory <T> {
    $Trajectory {
      origin: self.origin,
      coefficients: Array::from_fn (| index | (& self.coefficients [index]).neg()),
    }
  }
}


  };
}

//impl_trajectory! (LinearTrajectory, 1) ;
impl_trajectory! (QuadraticTrajectory, 2) ;


