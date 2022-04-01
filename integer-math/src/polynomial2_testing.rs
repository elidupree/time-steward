use crate::polynomial2::{next_time_magnitude_passes, Polynomial, SearchTargetRange};
use crate::{DoubleSized, DoubleSizedSignedInteger, Integer, ShiftSize};
use guard::guard;
use live_prop_test::{lpt_assert, lpt_assert_eq};
use num::{BigInt, BigRational, CheckedAdd, One, Signed, ToPrimitive, Zero};
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

fn perfect_evaluate_magnitude_squared<
  Coefficient: Integer + Signed,
  const DIMENSIONS: usize,
  const COEFFICIENTS: usize,
>(
  coordinates: &[&Polynomial<Coefficient, COEFFICIENTS>; DIMENSIONS],
  input: &BigRational,
) -> BigRational {
  let mut result = BigRational::zero();
  for coefficients in coordinates {
    let notsq = coefficients.perfect_evaluate(input);
    result += notsq.pow(2);
  }
  result
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

  pub(crate) fn perfect_evaluate(&self, input: &BigRational) -> BigRational {
    guard!(let Some((last, rest)) = self.0.split_last() else { return BigRational::zero() });
    let mut result = last.to_big_rational();
    for &coefficient in rest.iter().rev() {
      result *= input;
      result += coefficient.to_bigint().unwrap();
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

pub(crate) fn next_time_magnitude_passes_postconditions<
  Coefficient: DoubleSizedSignedInteger,
  S: ShiftSize,
  const DIMENSIONS: usize,
  const COEFFICIENTS: usize,
>(
  coordinates: &[&Polynomial<Coefficient, COEFFICIENTS>; DIMENSIONS],
  start_input: DoubleSized<Coefficient>,
  input_shift: S,
  target_range: SearchTargetRange<Coefficient>,
  result: Option<DoubleSized<Coefficient>>,
  new_algo: bool,
) -> Result<(), String>
where
  // this bound is and actually needed for this function, but
  // I'm including it for now to maintain parity with the other implementation
  DoubleSized<Coefficient>: DoubleSizedSignedInteger,
  [[[DoubleSized<Coefficient>; 2]; COEFFICIENTS]; DIMENSIONS]: Default,
  [[[DoubleSized<Coefficient>; 2]; 1]; DIMENSIONS]: Default,
  [[[DoubleSized<Coefficient>; 2]; 2]; DIMENSIONS]: Default,
  [[[DoubleSized<Coefficient>; 2]; 3]; DIMENSIONS]: Default,
{
  let input_scale = (DoubleSized::<Coefficient>::one() << input_shift.into()).to_big_rational();
  if let Some(result) = result {
    let result = result.to_big_rational() / &input_scale;
    let exact_magsq_at_result: BigRational =
      perfect_evaluate_magnitude_squared(coordinates, &result);
    lpt_assert!(
      target_range
        .squared_with_precision(0)
        .permits(&exact_magsq_at_result),
      "result value sqrt({}) ~= {:?} at input {} not in permitted range {:?}",
      exact_magsq_at_result,
      ToPrimitive::to_f64(&exact_magsq_at_result).map(f64::sqrt),
      result,
      target_range
    );
  }

  if result != Some(start_input) {
    let mut test_inputs = Vec::new();
    let mut jump_size = DoubleSized::<Coefficient>::one();
    let mut position = start_input;
    while jump_size > Zero::zero() {
      if let Some(next) = position.checked_add(&jump_size) {
        if !matches!(result, Some(r) if next >= r) {
          test_inputs.push(position);
          position = next;
          jump_size <<= 1;
          continue;
        }
      } else {
        break;
      }
      jump_size >>= 1;
    }

    let other_algo_result = if new_algo {
      Polynomial::<Coefficient, COEFFICIENTS>::next_time_magnitude_passes_trust_me(
        coordinates,
        start_input,
        input_shift,
        target_range,
      )
    } else {
      next_time_magnitude_passes(coordinates, start_input, input_shift, target_range)
    };

    if let Some(other_result) = other_algo_result {
      if !matches!(result, Some(r) if other_result >= r) {
        test_inputs.push(other_result);
      }
    }
    test_inputs.sort();

    for test_input in test_inputs {
      let test_input_rational = test_input.to_big_rational() / &input_scale;
      let exact_magsq_at_test_input: BigRational =
        perfect_evaluate_magnitude_squared(coordinates, &test_input_rational);
      lpt_assert!(
        !target_range
          .squared_with_precision(0)
          .requires(&exact_magsq_at_test_input),
        "value sqrt({}) ~= {:?} at {} was in required range {:?}, but search returned {:?}, which is later",
        exact_magsq_at_test_input,
        ToPrimitive::to_f64(&exact_magsq_at_test_input).map(f64::sqrt),
        test_input,
        target_range,
        result
      );
    }
  }

  Ok(())
}
