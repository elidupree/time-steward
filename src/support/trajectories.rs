//use nalgebra::Vector2;
//use std::cmp::max;
use num::traits::Signed;
use super::integer_math::*;
use std::ops::{Add, Sub, Mul, Neg, AddAssign, SubAssign, MulAssign};
use array_ext::*;


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

impl <$($generic_parameters)*> $Trait <$Right> for $Left {
  type Output = Self;
  fn $method ($self, $other: $Right)->Self {
    $owned_owned
  }
}

impl <'a, $($generic_parameters)*> $Trait <& 'a $Right> for $Left {
  type Output = Self;
  fn $method ($self, $other: & 'a $Right)->Self {
    $owned_ref
  }
}

impl <'a, $($generic_parameters)*> $Trait <$Right> for & 'a $Left {
  type Output = $Left;
  fn $method ($self, $other: $Right)->$Left {
    $ref_owned
  }
}

impl <'a, 'b, $($generic_parameters)*> $Trait <& 'b $Right> for & 'a $Left {
  type Output = $Left;
  fn $method ($self, $other: & 'b $Right)->$Left {
    $ref_ref
  }
}

impl <$($generic_parameters)*> $TraitAssign <$Right> for $Left {
  fn $method_assign (&mut $self, $other: $Right) {
    $assign_owned
  }
}

impl <'a, $($generic_parameters)*> $TraitAssign <& 'a $Right> for $Left {
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
  if self.origin < other.origin {self.set_origin (other.origin).unwrap();}
  if other.origin < self.origin {other.set_origin (self.origin).unwrap();}
  for (mine, others) in self.terms.iter_mut().zip (other.terms.into_iter()) {
    mine.$method_assign (others);
  }
},
{
  let mut other = other;
  let mut other_clone;
  if self.origin < other.origin {self.set_origin (other.origin).unwrap();}
  if other.origin < self.origin {
    other_clone = other.clone();
    other_clone.set_origin (self.origin).unwrap();
    other = & other_clone;
  }
  for (mine, others) in self.terms.iter_mut().zip (other.terms.iter()) {
    mine.$method_assign (others);
  }
},
}


impl_binop! {
[T: Vector], $Trajectory <T>, T, $Trait, $method, $TraitAssign, $method_assign, self, other,
{
  self.terms[0].$method_assign (other);
  self
},
{
  self.terms[0].$method_assign (other);
  self
},
{
  self.clone() + other
},
{
  self.clone() + other
},
{
  self.terms[0].$method_assign (other);
},
{
  self.terms[0].$method_assign (other);
},
}


  }
}






macro_rules! impl_trajectory {
  ($Trajectory: ident, $degree: expr) => {

#[derive (Clone, PartialEq, Eq, Hash, Serialize, Deserialize, Debug, Default)]
pub struct $Trajectory <T> {
  origin: Time,
  terms: [T; $degree + 1],
}

impl <T: Vector> $Trajectory <T> {
  pub fn constant (value: T)->Self {
    let terms = [T::zero(); $degree + 1];
    terms [0] = value;
    $Trajectory {origin: 0, terms}
  }
  pub fn set_origin (&mut self, new_origin: Time)->Result <(), polynomial::Error <Time>> {
    polynomial::translate (&mut self.terms, new_origin - self.origin)?;
    self.origin = new_origin;
    Ok (())
  }
  pub fn nth_term (&self, which: usize, time_numerator: Time, time_shift: u32)->Result <T, polynomial::OverflowError> {
    polynomial::evaluate_nth_taylor_coefficient_at_fractional_input (& self.terms, time - (self.origin << time_shift)), which)
  }
  pub fn set_nth_term (&mut self, which: usize, time_numerator: Time, time_shift: u32) {
    self.set_origin (time_numerator >> time_shift) ;
    unimplemented!()
  }
  pub fn add_nth_term (&mut self, which: usize, time_numerator: Time, time_shift: u32) {
    self.set_origin (time_numerator >> time_shift);
    unimplemented!()
  }
  pub fn value (&mut self, time_numerator: Time, time_shift: u32)->Result <T, polynomial::OverflowError> {self.term (0, time_numerator, time_shift)}
  pub fn velocity (&mut self, time_numerator: Time, time_shift: u32)->Result <T, polynomial::OverflowError> {self.term (1, time_numerator, time_shift)}
  pub fn set_value (&mut self, time_numerator: Time, time_shift: u32, value: T) {self.set_term (0, time_numerator, time_shift, value)}
  pub fn set_velocity (&mut self, time_numerator: Time, time_shift: u32, value: T) {self.set_term (1, time_numerator, time_shift, value)}
  pub fn add_value (&mut self, value: T) {self += value}
  pub fn add_velocity (&mut self, time_numerator: Time, time_shift: u32, value: T) {self.add_term (1, time_numerator, time_shift, value)}
}

impl <T: Integer + Signed> $Trajectory <T> {
  pub fn next_time_lt (&self, now: Time, value: T)->Option <Time> {
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
  }
}

impl_trajectory_add_sub! ($Trajectory, Add, add, AddAssign, add_assign);
impl_trajectory_add_sub! ($Trajectory, Sub, sub, SubAssign, sub_assign);

impl<T: Vector, Coordinate> Mul<Coordinate> for $Trajectory<T> where T: MulAssign <Coordinate> {
  type Output = Self;
  fn mul(self, other: Coordinate) -> Self {
    self *= other; self
  }
}

impl<'a, T: Vector, Coordinate> Mul<Coordinate> for & 'a $Trajectory<T> where T: MulAssign <Coordinate> {
  type Output = $Trajectory <T>;
  fn mul(self, other: Coordinate) ->$Trajectory <T> {
    self.clone()*other
  }
}

impl<T: Vector, Coordinate> MulAssign <Coordinate> for $Trajectory<T> where T: MulAssign <Coordinate> {
  fn mul_assign (&mut self, other: Coordinate) {
    for term in self.terms.iter_mut (){MulAssign::<Coordinate>::mul_assign (term, other);}
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
      terms: Array::from_fn (| index | self.terms [index].clone().neg()),
    }
  }
}
impl <'a, T: Vector> Neg for & 'a $Trajectory <T> where & 'a T: Neg <Output = T> {
  type Output = $Trajectory <T>;
  fn neg (self)->$Trajectory <T> {
    $Trajectory {
      origin: self.origin,
      terms: Array::from_fn (| index | (& self.terms [index]).neg()),
    }
  }
}


  };
}

//impl_trajectory! (LinearTrajectory, 1) ;
impl_trajectory! (QuadraticTrajectory, 2) ;


