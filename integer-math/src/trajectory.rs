use crate::maybe_const::ShiftSize;
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

#[serde_as]
#[derive(Copy, Clone, Eq, PartialEq, Hash, Debug, Serialize, Deserialize, Derivative)]
#[derivative(Default(
  bound = "Time: Default, [Polynomial<Coefficient, COEFFICIENTS>; DIMENSIONS]: Default"
))]
#[serde(bound(serialize = "Coefficient: Serialize, Time: Serialize"))]
#[serde(bound(deserialize = "Coefficient: Deserialize<'de>, Time: Deserialize<'de>"))]
pub struct Trajectory<Coefficient, Time, const DIMENSIONS: usize, const COEFFICIENTS: usize> {
  #[serde_as(as = "[_; DIMENSIONS]")]
  pub(crate) coordinates: [Polynomial<Coefficient, COEFFICIENTS>; DIMENSIONS],
  pub(crate) origin: Time,
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
  > Trajectory<Coefficient, Time, DIMENSIONS, COEFFICIENTS>
{
  #[inline]
  pub fn constant(value: &Vector<Coefficient, DIMENSIONS>) -> Self {
    Trajectory {
      coordinates: crate::vector_to_array(value).map(Polynomial::constant),
      origin: Time::zero(),
    }
  }
  #[inline]
  pub fn is_constant(&self) -> bool {
    self.coordinates.iter().all(Polynomial::is_constant)
  }

  pub fn set_origin(&mut self, new_origin: Time) {
    if !self.is_constant() {
      let duration: DoubleSized<Coefficient> = (new_origin - self.origin)
        .try_into()
        .ok()
        .expect("Overflow in Trajectory::set_origin");
      for coordinate in &mut self.coordinates {
        *coordinate = coordinate
          .all_taylor_coefficients(duration)
          .expect("Overflow in Trajectory::set_origin");
      }
    }
    self.origin = new_origin;
  }

  pub fn try_set_origin(&mut self, new_origin: Time) -> Result<(), OverflowError> {
    if !self.is_constant() {
      let duration: DoubleSized<Coefficient> = (new_origin - self.origin)
        .try_into()
        .map_err(|_| OverflowError)?;
      for coordinate in &mut self.coordinates {
        *coordinate = coordinate
          .all_taylor_coefficients(duration)
          .ok_or(OverflowError)?;
      }
    }
    self.origin = new_origin;
    Ok(())
  }

  #[inline]
  pub fn with_origin(self, new_origin: Time) -> Option<Self> {
    let mut result = self;
    result.try_set_origin(new_origin).ok()?;
    Some(result)
  }

  #[inline]
  pub fn with_later_origin(a: Self, b: Self) -> Option<(Self, Self)> {
    match a.origin.cmp(&b.origin) {
      Ordering::Less => Some((a.with_origin(b.origin)?, b)),
      Ordering::Equal => Some((a, b.with_origin(a.origin)?)),
      Ordering::Greater => Some((a, b)),
    }
  }

  pub fn nth_coefficient(
    &self,
    which: usize,
    time_numerator: Time,
    time_shift: impl ShiftSize,
  ) -> Option<Vector<Coefficient, DIMENSIONS>> {
    let mut result = Vector::<Coefficient, DIMENSIONS>::zero();
    let relative_time: DoubleSized<Coefficient> = (time_numerator
      - (self.origin << time_shift.into()))
    .try_into()
    .ok()?;
    for dimension in 0..DIMENSIONS {
      let bounds =
        self.coordinates[dimension].all_taylor_coefficients_bounds(relative_time, time_shift, 0)?
          [which];
      result[dimension] = mean_round_to_even(bounds[0], bounds[1]).try_into().ok()?;
    }
    Some(result)
  }

  pub fn set_nth_coefficient(
    &mut self,
    which: usize,
    time_numerator: Time,
    time_shift: impl ShiftSize,
    target_value: &Vector<Coefficient, DIMENSIONS>,
  ) -> Result<(), OverflowError> {
    self.try_set_origin(time_numerator >> time_shift.into())?;
    let relative_time: DoubleSized<Coefficient> = (time_numerator
      - (self.origin << time_shift.into()))
    .try_into()
    .map_err(|_| OverflowError)?;
    for (dimension, coordinate) in self.coordinates.iter_mut().enumerate() {
      coordinate.set_nth_taylor_coefficient_at_fractional_input(
        which,
        relative_time,
        time_shift,
        target_value[dimension],
      )?;
    }
    Ok(())
  }

  pub fn add_nth_coefficient(
    &mut self,
    which: usize,
    time_numerator: Time,
    time_shift: impl ShiftSize,
    added_value: &Vector<Coefficient, DIMENSIONS>,
  ) -> Result<(), OverflowError> {
    let current_value = self
      .nth_coefficient(which, time_numerator, time_shift)
      .ok_or(OverflowError)?;
    // TODO used proper add function instead, once I implement it
    self.set_nth_coefficient(
      which,
      time_numerator,
      time_shift,
      &(current_value + added_value),
    )
  }

  pub fn value(
    &self,
    time_numerator: Time,
    time_shift: impl ShiftSize,
  ) -> Option<Vector<Coefficient, DIMENSIONS>> {
    self.nth_coefficient(0, time_numerator, time_shift)
  }

  pub fn velocity(
    &self,
    time_numerator: Time,
    time_shift: impl ShiftSize,
  ) -> Option<Vector<Coefficient, DIMENSIONS>> {
    self.nth_coefficient(1, time_numerator, time_shift)
  }

  // pub fn acceleration(
  //   &self,
  //   time_numerator: Time,
  //   time_shift: impl ShiftSize,
  // ) -> Option<Vector<Coefficient, DIMENSIONS>> {
  //   Some(
  //     self
  //       .nth_coefficient(1, time_numerator, time_shift)?
  //       .checked_mul(2)?,
  //   )
  // }

  pub fn set_value(
    &mut self,
    time_numerator: Time,
    time_shift: impl ShiftSize,
    value: &Vector<Coefficient, DIMENSIONS>,
  ) -> Result<(), OverflowError> {
    self.set_nth_coefficient(0, time_numerator, time_shift, value)
  }

  pub fn set_velocity(
    &mut self,
    time_numerator: Time,
    time_shift: impl ShiftSize,
    value: &Vector<Coefficient, DIMENSIONS>,
  ) -> Result<(), OverflowError> {
    self.set_nth_coefficient(1, time_numerator, time_shift, value)
  }

  // pub fn set_acceleration(
  //   &mut self,
  //   time_numerator: Time,
  //   time_shift: impl ShiftSize,
  //   value: &Vector<Coefficient, DIMENSIONS>,
  // ) -> Result<(), OverflowError> {
  //   self.set_nth_coefficient(
  //     2,
  //     time_numerator,
  //     time_shift,
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
    time_numerator: Time,
    time_shift: impl ShiftSize,
    value: &Vector<Coefficient, DIMENSIONS>,
  ) -> Result<(), OverflowError> {
    self.add_nth_coefficient(1, time_numerator, time_shift, value)
  }
  // pub fn add_acceleration(
  //   &mut self,
  //   time_numerator: Time,
  //   time_shift: impl ShiftSize,
  //   value: &Vector<Coefficient, DIMENSIONS>,
  // ) -> Result<(), OverflowError> {
  //   self.add_nth_coefficient(
  //     2,
  //     time_numerator,
  //     time_shift,
  //     // note: we don't have to use shr_nicely_rounded because it's always at 0.5
  //     // TODO: do this with slightly better precision
  //     value.map_coordinates(|coordinate| shr_round_to_even(coordinate, 1u32)),
  //   )
  // }
}
