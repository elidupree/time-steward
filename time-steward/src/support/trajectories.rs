//use nalgebra::Vector2;
use std::cmp::min;
use num::{FromPrimitive, Zero, One};
use num::traits::{Signed, Bounded};
use super::integer_math::*;
use self::polynomial::RootSearchResult;
use std::ops::{Add, Sub, Mul, Neg, AddAssign, SubAssign, MulAssign};
use array_ext::*;
use smallvec::SmallVec;


pub type Time = i64;


pub trait Trajectory: Sized {
  type Coefficient: Vector;
}

pub trait ScalarTrajectory: Trajectory where Self::Coefficient: Integer, for <'a> & 'a Self: Neg <Output = Self> {
  /// Find the first time within a range when the trajectory value is "significantly" >= target.
  ///
  /// To be "significant", the output time will obey this condition: The value at the output time is >= target, and it will not dip back to < target due to rounding error (it will only dip back if the ideal slope actually becomes negative).
  ///
  /// In order to guarantee this, we have to be slightly permissive, skipping over some inputs where the value is observed to be >= target. However, it still obeys this guarantee: The output time will always be <= the first time when the value is >= output + 4.
  fn next_time_significantly_ge (&self, range: [Time; 2], input_shift: u32, target: Self::Coefficient)->RootSearchResult<Time>;
  fn next_time_significantly_gt (&self, range: [Time; 2], input_shift: u32, target: Self::Coefficient)->RootSearchResult<Time> {
    self.next_time_significantly_ge (range, input_shift, target + Self::Coefficient::one())
  }
  fn next_time_significantly_le (&self, range: [Time; 2], input_shift: u32, target: Self::Coefficient)->RootSearchResult<Time> {
    (-self).next_time_significantly_ge (range, input_shift, -target)
  }
  fn next_time_significantly_lt (&self, range: [Time; 2], input_shift: u32, target: Self::Coefficient)->RootSearchResult<Time> {
    self.next_time_significantly_le (range, input_shift, target - Self::Coefficient::one())
  }
}

macro_rules! impl_binop {
  ([$($generic_parameters: tt)*], $Left:ty, $Right:ty, $Output:ty, $Trait: ident, $method: ident, $self: ident, $other: ident, $owned_owned: expr, $owned_ref: expr, $ref_owned: expr, $ref_ref: expr,) => {

impl <$($generic_parameters)*> $Trait <$Right> for $Left where Time: From <T::Coordinate> {
  type Output = $Output;
  fn $method (mut $self, $other: $Right)->Self {
    $owned_owned
  }
}

impl <'a, $($generic_parameters)*> $Trait <& 'a $Right> for $Left where Time: From <T::Coordinate> {
  type Output = $Output;
  fn $method (mut $self, $other: & 'a $Right)->Self {
    $owned_ref
  }
}

impl <'a, $($generic_parameters)*> $Trait <$Right> for & 'a $Left where Time: From <T::Coordinate> {
  type Output = $Output;
  fn $method ($self, $other: $Right)->$Left {
    $ref_owned
  }
}

impl <'a, 'b, $($generic_parameters)*> $Trait <& 'b $Right> for & 'a $Left where Time: From <T::Coordinate> {
  type Output = $Output;
  fn $method ($self, $other: & 'b $Right)->$Left {
    $ref_ref
  }
}

  }
}

macro_rules! impl_binop_and_assign {
  ([$($generic_parameters: tt)*], $Left:ty, $Right:ty, $Output:ty, $Trait: ident, $method: ident, $TraitAssign: ident, $method_assign: ident, $self: ident, $other: ident, $owned_owned: expr, $owned_ref: expr, $ref_owned: expr, $ref_ref: expr, $assign_owned: expr, $assign_ref: expr,) => {

impl_binop! ([$($generic_parameters)*], $Left, $Right, $Output, $Trait, $method, $self, $other, $owned_owned, $owned_ref, $ref_owned, $ref_ref,);

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

impl_binop_and_assign! {
[T: Vector], $Trajectory <T>, $Trajectory <T>, $Trajectory <T>, $Trait, $method, $TraitAssign, $method_assign, self, other,
{
  self.$method_assign (other);
  self
},
{
  self.$method_assign (other);
  self
},
{
  other.$method (self)
},
{
  if self.origin < other.origin {self.clone().$method (other)}
  else {other.clone().$method (self)}
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


impl_binop_and_assign! {
[T: Vector], $Trajectory <T>, T, $Trajectory <T>, $Trait, $method, $TraitAssign, $method_assign, self, other,
{
  self.coefficients [0].$method_assign (other);
  self
},
{
  self.coefficients [0].$method_assign (other);
  self
},
{
  self.clone().$method (other)
},
{
  self.clone().$method (other)
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
  ($Trajectory: ident, $degree: expr, $multiplication: tt, $ProductTrajectory: ident) => {

#[derive (Clone, PartialEq, Eq, Hash, Serialize, Deserialize, Debug, Default)]
pub struct $Trajectory <T> {
  origin: Time,
  coefficients: [T; $degree + 1],
}

impl <T: Vector> Trajectory for $Trajectory <T> {
  type Coefficient = T;
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
  pub fn value (&self, time_numerator: Time, time_shift: u32)->Result <T, polynomial::OverflowError> {self.nth_coefficient (0, time_numerator, time_shift)}
  pub fn velocity (&self, time_numerator: Time, time_shift: u32)->Result <T, polynomial::OverflowError> {self.nth_coefficient (1, time_numerator, time_shift)}
  //pub fn acceleration (&self, time_numerator: Time, time_shift: u32)->Result <T, polynomial::OverflowError> {Ok (self.nth_coefficient (1, time_numerator, time_shift)?<<1)}
  pub fn set_value (&mut self, time_numerator: Time, time_shift: u32, value: T)->Result <(), polynomial::OverflowError> {self.set_nth_coefficient (0, time_numerator, time_shift, value)}
  pub fn set_velocity (&mut self, time_numerator: Time, time_shift: u32, value: T)->Result <(), polynomial::OverflowError> {self.set_nth_coefficient (1, time_numerator, time_shift, value)}
  pub fn set_acceleration (&mut self, time_numerator: Time, time_shift: u32, value: T)->Result <(), polynomial::OverflowError> {self.set_nth_coefficient (2, time_numerator, time_shift, value.map_coordinates (| coordinate | shr_round_to_even (coordinate, 1u32)))}
  pub fn add_value (&mut self, value: T) {*self += value}
  pub fn add_velocity (&mut self, time_numerator: Time, time_shift: u32, value: T)->Result <(), polynomial::OverflowError> {self.add_nth_coefficient (1, time_numerator, time_shift, value)}
  pub fn add_acceleration (&mut self, time_numerator: Time, time_shift: u32, value: T)->Result <(), polynomial::OverflowError> {self.add_nth_coefficient (2, time_numerator, time_shift, value.map_coordinates (| coordinate | shr_round_to_even (coordinate, 1u32)))}
  
  pub fn next_time_possibly_outside_bounds (&self, range: [Time; 2], input_shift: u32, bounds: [T; 2])->RootSearchResult<Time> where for <'a> & 'a T::Coordinate: Neg <Output = T::Coordinate> {
    (0..T::DIMENSIONS).map (| dimension | {
      let trajectory = self.coordinate_trajectory (dimension);
      let four = (T::Coordinate::one() + T::Coordinate::one()) + (T::Coordinate::one() + T::Coordinate::one());
      min(
        trajectory.next_time_significantly_le (range, input_shift, bounds [0].coordinate (dimension) + four),
        trajectory.next_time_significantly_ge (range, input_shift, bounds [1].coordinate (dimension) - four),
      )
    }).min().unwrap()
  }
  
  #[cfg $multiplication]
  pub fn magnitude_squared_trajectory (&self)->Result <$ProductTrajectory <T::Coordinate>, polynomial::OverflowError> {
    let mut coefficients = [T::Coordinate::zero(); $degree + $degree + 1];
    for dimension in 0..T::DIMENSIONS {
      let coordinate_coefficients = self.coordinate_coefficients (dimension);
      polynomial::add_product_into (& coordinate_coefficients, & coordinate_coefficients, &mut coefficients)?;
    }
    Ok ($ProductTrajectory {
      origin: self.origin, coefficients
    })
  }
  #[cfg $multiplication]
  pub fn next_time_magnitude_significantly_gt (&self, range: [Time; 2], input_shift: u32, target: T::Coordinate)->Result <RootSearchResult <Time>, polynomial::OverflowError> where for <'a> & 'a T::Coordinate: Neg <Output = T::Coordinate> {
    Ok(self.magnitude_squared_trajectory()?.next_time_significantly_gt (range, input_shift, target*target))
  }
  #[cfg $multiplication]
  pub fn next_time_magnitude_significantly_lt (&self, range: [Time; 2], input_shift: u32, target: T::Coordinate)->Result <RootSearchResult <Time>, polynomial::OverflowError> where for <'a> & 'a T::Coordinate: Neg <Output = T::Coordinate> {
    Ok(self.magnitude_squared_trajectory()?.next_time_significantly_lt (range, input_shift, target*target))
  }
}


/*impl <T: Vector + Integer> $Trajectory <T> where Time: From <T::Coordinate> {
  #[cfg $multiplication]
  fn multiply_same_origin (&self, other: & Self)->$ProductTrajectory <T> {
    let mut coefficients = [T::zero(); $degree + $degree + 1];
    polynomial::add_product_into (& self.coefficients, & other.coefficients, &mut coefficients).unwrap();
    $ProductTrajectory {
      origin: self.origin, coefficients
    }
  }
}*/

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

/*impl_binop_and_assign! {
[T: Vector], $Trajectory <T>, T::Coordinate, $Trajectory <T>, Mul, mul, MulAssign, mul_assign, self, other,
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

/*#[cfg $multiplication]
impl_binop! {
[T: Vector + Integer], $Trajectory <T>, $Trajectory <T>, $ProductTrajectory <T>, Mul, mul, self, other,
{
  if self.origin < other.origin {self.set_origin (other.origin).unwrap(); }
  if other.origin < self.origin {other.set_origin (self.origin).unwrap(); }
  self.multiply_same_origin (&other)
},
{
  if other.origin < self.origin {let mut other = other.clone(); other.set_origin (self.origin).unwrap(); return self.multiply_same_origin (other)}
  if self.origin < other.origin {self.set_origin (other.origin).unwrap(); }
  self.multiply_same_origin (other)
},
{
  other + self
},
{
  if self.origin < other.origin {let mut me = self.clone(); me.set_origin (other.origin).unwrap(); return me.multiply_same_origin (other)}
  if other.origin < self.origin {let mut other = other.clone(); other.set_origin (self.origin).unwrap(); return self.multiply_same_origin (other)}
  self.multiply_same_origin (other)
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

impl <T: Vector + Integer + Signed + HasCoordinates <Coordinate = T>> ScalarTrajectory for $Trajectory <T> where for <'a> & 'a T: Neg <Output = T>, Time: From <T> {
  fn next_time_significantly_ge (&self, range: [Time; 2], input_shift: u32, target: Self::Coefficient)->RootSearchResult<Time> {
    let relative = self - (target + T::one() + T::one());
    let origin = self.origin << input_shift;
    match polynomial::root_search (& relative.coefficients, [range [0] - origin, range [1] - origin], input_shift) {
      RootSearchResult::Root (input) => RootSearchResult::Root (origin + if relative.value (origin + input, input_shift).unwrap() >= T::zero() {input} else {input + 1}),
      RootSearchResult::Overflow (input) => RootSearchResult::Overflow (self.origin + input),
      RootSearchResult::Finished => RootSearchResult::Finished,
    }
  }
}


  };
}

impl_trajectory! (LinearTrajectory, 1, (all()), QuadraticTrajectory) ;
impl_trajectory! (QuadraticTrajectory, 2, (all()), QuarticTrajectory) ;
impl_trajectory! (QuarticTrajectory, 4, (any()), Unused) ;


