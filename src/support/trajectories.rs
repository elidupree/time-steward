use nalgebra::Vector2;
use std::cmp::max;
use super::rounding_error_tolerant_math::*;
use std::ops::{Add, Sub, Mul, Neg};
use array_ext::*;


pub type Time = i64;

macro_rules! impl_trajectory_arithmetic_trait {
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

macro_rules! impl_trajectory {
  ($Trajectory: ident, $degree: expr) => {

#[derive (Clone, PartialEq, Eq, Serialize, Deserialize, Debug)]
pub struct $Trajectory <Vector> {
  origin: Time,
  terms: [Vector; $degree + 1],
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

impl_trajectory! (LinearTrajectory, 1) ;
impl_trajectory! (QuadraticTrajectory, 2) ;
