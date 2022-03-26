use crate::maybe_const::ConstU32;
use crate::polynomial2::{OverflowError, Polynomial};
use crate::{
  mean_round_to_even, shr_round_to_even, DoubleSized, DoubleSizedSignedInteger, Integer, Vector,
};
use derivative::Derivative;
use num::{Signed, Zero};
use serde::{Deserialize, Serialize};
use serde_with::serde_as;
use std::cmp::Ordering;
use std::convert::TryInto;

/**
A polynomial-vector trajectory for TimeSteward simulations.

This is a convenience type, mostly a wrapper around various `Polynomial` functions. It makes the following assumptions:

A Trajectory type has a constant `TIME_SHIFT`; all methods that interact with Time are interpreting it as a *fractional* input (time / 2^TIME_SHIFT) to the underlying polynomials. (TODO: explain the motivation better for a newcomer)

A Trajectory has an _origin_ – the Time of the last change that was made to it. Time only goes forwards, so querying a Trajectory earlier than its origin is an error and may panic or misbehave. (TODO: make it always panic) (TODO: explain how this helps with overflow)
*/
#[serde_as]
#[derive(Copy, Clone, Eq, PartialEq, Hash, Debug, Serialize, Deserialize, Derivative)]
#[derivative(Default(
  bound = "Time: Default, [Polynomial<Coefficient, COEFFICIENTS>; DIMENSIONS]: Default"
))]
#[serde(bound(serialize = "Coefficient: Serialize, Time: Serialize"))]
#[serde(bound(deserialize = "Coefficient: Deserialize<'de>, Time: Deserialize<'de>"))]
pub struct Trajectory<
  Coefficient,
  Time,
  const DIMENSIONS: usize,
  const COEFFICIENTS: usize,
  const TIME_SHIFT: u32,
> {
  // our coordinate polynomials, as functions of self.relative_to_rounded_origin(time)
  #[serde_as(as = "[_; DIMENSIONS]")]
  pub(crate) coordinates: [Polynomial<Coefficient, COEFFICIENTS>; DIMENSIONS],
  pub(crate) unrounded_origin: Time,
}

impl<
    Coefficient: DoubleSizedSignedInteger,
    Time: Integer
      + Signed
      //+ From<Coefficient>
      //+ From<DoubleSized<Coefficient>>
      + TryInto<Coefficient>
      + TryInto<DoubleSized<Coefficient>>,
    const DIMENSIONS: usize,
    const COEFFICIENTS: usize,
    const TIME_SHIFT: u32,
  > Trajectory<Coefficient, Time, DIMENSIONS, COEFFICIENTS, TIME_SHIFT>
{
  #[inline]
  pub fn constant(value: &Vector<Coefficient, DIMENSIONS>) -> Self {
    Trajectory {
      coordinates: crate::vector_to_array(value).map(Polynomial::constant),
      unrounded_origin: Time::min_value(),
    }
  }
  #[inline]
  pub fn is_constant(&self) -> bool {
    self.coordinates.iter().all(Polynomial::is_constant)
  }

  fn time_numerator_to_nearest_integer(time_numerator: Time) -> Time {
    // we don't mind a directional bias here, because time has a directional bias
    // round towards neginf at N.5 (why? shrug, just feels right)
    (time_numerator + (Time::one() << (TIME_SHIFT - 1)) - Time::one()) >> TIME_SHIFT
  }

  // the origin of our polynomials, in time-numerator form
  fn rounded_origin_numerator(&self) -> Time {
    Self::time_numerator_to_nearest_integer(self.unrounded_origin) << TIME_SHIFT
  }

  fn relative_to_rounded_origin(&self, time_numerator: Time) -> Option<DoubleSized<Coefficient>> {
    (time_numerator - self.rounded_origin_numerator())
      .try_into()
      .ok()
  }

  pub fn set_origin(&mut self, new_origin: Time) {
    if !self.is_constant() {
      let integer_duration: DoubleSized<Coefficient> =
        (Self::time_numerator_to_nearest_integer(new_origin)
          - Self::time_numerator_to_nearest_integer(self.unrounded_origin))
        .try_into()
        .ok()
        .expect("Overflow in Trajectory::set_origin");
      for coordinate in &mut self.coordinates {
        *coordinate = coordinate
          .all_taylor_coefficients(integer_duration)
          .expect("Overflow in Trajectory::set_origin");
      }
    }
    self.unrounded_origin = new_origin;
  }

  pub fn try_set_origin(&mut self, new_origin: Time) -> Result<(), OverflowError> {
    if !self.is_constant() {
      let integer_duration: DoubleSized<Coefficient> =
        (Self::time_numerator_to_nearest_integer(new_origin)
          - Self::time_numerator_to_nearest_integer(self.unrounded_origin))
        .try_into()
        .map_err(|_| OverflowError)?;
      for coordinate in &mut self.coordinates {
        *coordinate = coordinate
          .all_taylor_coefficients(integer_duration)
          .ok_or(OverflowError)?;
      }
    }
    self.unrounded_origin = new_origin;
    Ok(())
  }

  #[inline]
  pub fn with_origin(self, new_origin: Time) -> Option<Self> {
    let mut result = self;
    result.try_set_origin(new_origin).ok()?;
    Some(result)
  }

  #[inline]
  pub(crate) fn with_matching_rounded_origins(a: Self, b: Self) -> Option<(Self, Self)> {
    match a
      .rounded_origin_numerator()
      .cmp(&b.rounded_origin_numerator())
    {
      Ordering::Less => Some((a.with_origin(b.unrounded_origin)?, b)),
      Ordering::Equal => Some((a, b.with_origin(a.unrounded_origin)?)),
      Ordering::Greater => Some((a, b)),
    }
  }

  pub fn nth_coefficient(
    &self,
    which: usize,
    time: Time,
  ) -> Option<Vector<Coefficient, DIMENSIONS>> {
    let mut result = Vector::<Coefficient, DIMENSIONS>::zero();
    let relative_time = self.relative_to_rounded_origin(time)?;
    for dimension in 0..DIMENSIONS {
      let bounds = self.coordinates[dimension].all_taylor_coefficients_bounds(
        relative_time,
        ConstU32::<TIME_SHIFT>,
        0,
      )?[which];
      result[dimension] = mean_round_to_even(bounds[0], bounds[1]).try_into().ok()?;
    }
    Some(result)
  }

  pub fn set_nth_coefficient(
    &mut self,
    which: usize,
    time: Time,
    target_value: &Vector<Coefficient, DIMENSIONS>,
  ) -> Result<(), OverflowError> {
    self.try_set_origin(time)?;
    let relative_time = self.relative_to_rounded_origin(time).ok_or(OverflowError)?;
    for (dimension, coordinate) in self.coordinates.iter_mut().enumerate() {
      coordinate.set_nth_taylor_coefficient_at_fractional_input(
        which,
        relative_time,
        ConstU32::<TIME_SHIFT>,
        target_value[dimension],
      )?;
    }
    Ok(())
  }

  pub fn add_nth_coefficient(
    &mut self,
    which: usize,
    time: Time,
    added_value: &Vector<Coefficient, DIMENSIONS>,
  ) -> Result<(), OverflowError> {
    // TODO used proper add function instead, once I implement it
    let current_value = self.nth_coefficient(which, time).ok_or(OverflowError)?;
    self.set_nth_coefficient(which, time, &(current_value + added_value))
  }

  pub fn value(&self, time: Time) -> Option<Vector<Coefficient, DIMENSIONS>> {
    self.nth_coefficient(0, time)
  }

  pub fn velocity(&self, time: Time) -> Option<Vector<Coefficient, DIMENSIONS>> {
    self.nth_coefficient(1, time)
  }

  // pub fn acceleration(
  //   &self,
  //   time: Time,
  //
  // ) -> Option<Vector<Coefficient, DIMENSIONS>> {
  //   Some(
  //     self
  //       .nth_coefficient(1, time, ConstU32::<TIME_SHIFT>)?
  //       .checked_mul(2)?,
  //   )
  // }

  pub fn set_value(
    &mut self,
    time: Time,
    value: &Vector<Coefficient, DIMENSIONS>,
  ) -> Result<(), OverflowError> {
    self.set_nth_coefficient(0, time, value)
  }

  pub fn set_velocity(
    &mut self,
    time: Time,
    value: &Vector<Coefficient, DIMENSIONS>,
  ) -> Result<(), OverflowError> {
    self.set_nth_coefficient(1, time, value)
  }

  // pub fn set_acceleration(
  //   &mut self,
  //   time: Time,
  //   value: &Vector<Coefficient, DIMENSIONS>,
  // ) -> Result<(), OverflowError> {
  //   self.set_nth_coefficient(
  //     2,
  //     time,
  //
  //     // note: we don't have to use shr_nicely_rounded because it's always at 0.5
  //     // TODO: do this with slightly better precision
  //     value.map_coordinates(|coordinate| shr_round_to_even(coordinate, 1u32)),
  //   )
  // }

  pub fn add_value(&mut self, value: Vector<Coefficient, DIMENSIONS>) {
    *self += value
  }
  pub fn add_velocity(
    &mut self,
    time: Time,
    value: &Vector<Coefficient, DIMENSIONS>,
  ) -> Result<(), OverflowError> {
    self.add_nth_coefficient(1, time, value)
  }
  // pub fn add_acceleration(
  //   &mut self,
  //   time: Time,
  //   value: &Vector<Coefficient, DIMENSIONS>,
  // ) -> Result<(), OverflowError> {
  //   self.add_nth_coefficient(
  //     2,
  //     time,
  //
  //     // note: we don't have to use shr_nicely_rounded because it's always at 0.5
  //     // TODO: do this with slightly better precision
  //     value.map_coordinates(|coordinate| shr_round_to_even(coordinate, 1u32)),
  //   )
  // }
}
