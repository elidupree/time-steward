use crate::polynomial2::Polynomial;
use crate::{DoubleSized, DoubleSizedSignedInteger, Integer, ShiftSize};
use live_prop_test::{lpt_assert, lpt_assert_eq};
use num::{BigInt, BigRational, One, Signed, Zero};
use std::convert::TryInto;

fn naive_factorial(value: usize) -> BigInt {
  let mut result = BigInt::one();
  for factor in 2..=value {
    result *= BigInt::from(factor);
  }
  result
}

fn naive_binomial_coefficient(n: usize, k: usize) -> BigInt {
  naive_factorial(n) / (naive_factorial(k) * naive_factorial(n - k))
}

impl<Coefficient: Integer + Signed, const COEFFICIENTS: usize>
  Polynomial<Coefficient, COEFFICIENTS>
{
  pub(crate) fn big_rational_coefficients(&self) -> impl Iterator<Item = BigRational> + '_ {
    self
      .0
      .iter()
      .map(|&coefficient| coefficient.to_big_rational())
  }

  pub(crate) fn naive_perfect_evaluate(&self, input: &BigRational) -> BigRational {
    let mut result = BigRational::zero();
    for (exponent, mut term) in self.big_rational_coefficients().enumerate() {
      for _ in 0..exponent {
        term *= input;
      }
      result += term;
    }
    result
  }

  pub(crate) fn naive_perfect_nth_taylor_coefficient(
    &self,
    input: &BigRational,
    n: usize,
  ) -> BigRational {
    let mut result = BigRational::zero();
    for (exponent, mut term) in self.big_rational_coefficients().enumerate().skip(n) {
      for _ in n..exponent {
        term *= input;
      }
      result += term * naive_binomial_coefficient(exponent, n);
    }
    result
  }

  pub(crate) fn naive_perfect_taylor_coefficients(
    &self,
    input: BigRational,
  ) -> impl Iterator<Item = BigRational> + '_ {
    (0..COEFFICIENTS).map(move |n| self.naive_perfect_nth_taylor_coefficient(&input, n))
  }
}

impl<Coefficient: DoubleSizedSignedInteger, const COEFFICIENTS: usize>
  Polynomial<Coefficient, COEFFICIENTS>
{
  pub(crate) fn all_taylor_coefficients_postconditions(
    &self,
    input: impl Integer + Signed + TryInto<DoubleSized<Coefficient>>,
    result: &Option<Self>,
  ) -> Result<(), String> {
    if let Some(result) = result {
      if let Some(translated_back) = result.all_taylor_coefficients(-input) {
        lpt_assert_eq!(
          *self,
          translated_back,
          "all_taylor_coefficients should be reversible"
        )
      } else {
        return Err("all_taylor_coefficients should be reversible".to_string());
      }

      for (exponent, (result, perfect)) in result
        .big_rational_coefficients()
        .zip(self.naive_perfect_taylor_coefficients(input.to_big_rational()))
        .enumerate()
      {
        lpt_assert_eq!(result, perfect, "coefficient {} was incorrect", exponent);
      }
    } else {
      lpt_assert!(
        self
          .naive_perfect_taylor_coefficients(input.to_big_rational())
          .any(|result| result < Coefficient::min_value().to_big_rational()
            || result > Coefficient::max_value().to_big_rational()),
        "returned None when exact answer was entirely in-bounds"
      );
    }
    Ok(())
  }
}

impl<Coefficient: Integer + Signed, const COEFFICIENTS: usize>
  Polynomial<Coefficient, COEFFICIENTS>
{
  pub(crate) fn all_taylor_coefficients_bounds_postconditions<
    WorkingType: Integer + Signed + From<Coefficient> + TryInto<Coefficient>,
  >(
    &self,
    input: WorkingType,
    input_shift: impl ShiftSize,
    precision_shift: impl ShiftSize,
    result: Option<[[WorkingType; 2]; COEFFICIENTS]>,
  ) -> Result<(), String> {
    if let Some(result) = result {
      let exact_input = input.to_big_rational() / (1 << input_shift.into()).to_big_rational();
      let precision_scale = (1 << precision_shift.into()).to_big_rational();
      let exact_answers = self
        .naive_perfect_taylor_coefficients(exact_input)
        .map(|e| e * &precision_scale);
      let leeway = BigRational::new(BigInt::from(3i32), BigInt::from(2i32));
      for (exponent, (bounds, exact)) in result.iter().zip(exact_answers).enumerate() {
        lpt_assert!(
          bounds[0].to_big_rational() <= exact,
          "Incorrect {}th taylor coefficient lower bound: {} > {}",
          exponent,
          bounds[0],
          exact
        );
        lpt_assert!(
          bounds[1].to_big_rational() >= exact,
          "Incorrect {}th taylor coefficient upper bound: {} > {}",
          exponent,
          bounds[1],
          exact
        );
        lpt_assert!(
          bounds[0].to_big_rational() > &exact - &leeway,
          "Too loose {}th taylor coefficient lower bound: {} <= {} - 1.5",
          exponent,
          bounds[0],
          exact
        );
        lpt_assert!(
          bounds[1].to_big_rational() < &exact + &leeway,
          "Too loose {}th taylor coefficient upper bound: {} >= {} + 1.5",
          exponent,
          bounds[1],
          exact
        );
        lpt_assert!(bounds[1] <= bounds[0].saturating_add(WorkingType::one()+WorkingType::one()), "{}th taylor coefficient bounds are too far from each other (note: this should be impossible if the other conditions are met): {} > {} + 2", exponent, bounds[1], bounds[0]);
      }
    }

    Ok(())
  }
}
