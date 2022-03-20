use num::{BigInt, BigRational};
use num::{Bounded, FromPrimitive, One, Signed};
use proptest::prelude::*;
use std::cmp::max;

use super::polynomial2::*;
use super::range_search::*;
use super::*;

fn naive_perfect_evaluate<Coefficient: Integer>(
  coefficients: &[Coefficient],
  input: BigRational,
) -> BigRational
where
  BigInt: From<Coefficient>,
{
  let mut result = BigRational::zero();
  for (exponent, coefficient) in coefficients.iter().enumerate() {
    let mut term = BigRational::from(BigInt::from(*coefficient));
    for _ in 0..exponent {
      term = term * &input;
    }
    result = result + term;
  }
  result
}

fn naive_perfect_evaluate_magnitude_squared<Coefficient: Integer>(
  coordinates: &[&[Coefficient]],
  input: BigRational,
) -> BigRational
where
  BigInt: From<Coefficient>,
{
  let mut result = BigRational::zero();
  for coefficients in coordinates {
    let notsq = naive_perfect_evaluate(coefficients, input.clone());
    result = result + &notsq * &notsq;
  }
  result
}

fn naive_factorial(value: usize) -> BigInt {
  let mut result = BigInt::one();
  for factor in 2..=value {
    result = result * BigInt::from(factor);
  }
  result
}

fn naive_binomial_coefficient(n: usize, k: usize) -> BigInt {
  naive_factorial(n) / (naive_factorial(k) * naive_factorial(n - k))
}

fn naive_perfect_nth_taylor_coefficient<Coefficient: Integer>(
  coefficients: &[Coefficient],
  input: BigRational,
  n: usize,
) -> BigRational
where
  BigInt: From<Coefficient>,
{
  let mut result = BigRational::zero();
  for (exponent, coefficient) in coefficients.iter().enumerate().skip(n) {
    let mut term = BigRational::from(BigInt::from(*coefficient));
    for _ in n..exponent {
      term = term * &input;
    }
    result = result + term * naive_binomial_coefficient(exponent, n);
  }
  result
}

fn precision_scale(precision_shift: u32) -> BigRational {
  BigRational::new(
    BigInt::from_i64(1i64 << precision_shift).unwrap(),
    BigInt::one(),
  )
}

fn rational_input<T>(input: FractionalInput<T>) -> BigRational
where
  BigInt: From<T>,
{
  BigRational::new(
    BigInt::from(input.numerator),
    BigInt::from_i64(1i64 << input.shift).unwrap(),
  )
}

fn arbitrary_fractional_input() -> BoxedStrategy<FractionalInput<i64>> {
  (0u32..16)
    .prop_flat_map(|shift| ((-16i64 << shift..16i64 << shift), Just(shift)))
    .prop_map(|(numerator, shift)| FractionalInput { numerator, shift })
    .boxed()
}

fn probe_max_total_shift<
  Coefficient: Integer + Signed,
  WorkingType: Integer + Signed + From<Coefficient> + TryInto<Coefficient>,
  const COEFFICIENTS: usize,
>(
  overflow: u32,
) {
  let max_total_shift = <[Coefficient; COEFFICIENTS] as AllTaylorCoefficientsBoundsWithinHalf<
    WorkingType,
  >>::max_total_shift()
    + overflow;
  let all_pos = [Coefficient::max_value(); COEFFICIENTS];
  let all_neg = [Coefficient::min_value(); COEFFICIENTS];
  let mut one_neg = [Coefficient::max_value(); COEFFICIENTS];
  one_neg[COEFFICIENTS - 1] = Coefficient::min_value();
  let mut one_pos = [Coefficient::min_value(); COEFFICIENTS];
  one_pos[COEFFICIENTS - 1] = Coefficient::max_value();
  for input_shift in 1..max_total_shift {
    for input in [WorkingType::one(), -WorkingType::one()] {
      for polynomial in &[all_pos, all_neg, one_pos, one_neg] {
        all_taylor_coefficients_bounds_within_half_unchecked(
          polynomial,
          input << (input_shift - 1),
          input_shift,
          max_total_shift - input_shift,
        );
      }
    }
  }
}

macro_rules! test_nontrivial_polynomial {
  ($coefficients: expr, $integer: ident, $double: ident, $uniform: ident, $name: ident $(,)*) => {
    mod $name {
      use super::*;

      test_polynomial!($coefficients, $integer, $double, $uniform, $name);

      #[test]
      fn max_total_shift_works() {
        probe_max_total_shift::<$integer, $double, $coefficients>(0);
      }

      #[test]
      #[should_panic(expected = "overflow")]
      fn max_total_shift_tight() {
        probe_max_total_shift::<$integer, $double, $coefficients>(1);
      }
    }
  };
}

macro_rules! test_polynomial {
  ($coefficients: expr, $integer: ident, $double: ident, $uniform: ident, $name: ident $(,)*) => {

  mod $name {
    test_polynomial!($coefficients, $integer, $double, $uniform);
    use super::*;
    //use super::super::*;
    //use proptest::prelude::*;
  }
  };
  ($coefficients: expr, $integer: ident, $double: ident, $uniform: ident $(,)*) => {

    proptest! {
      #[test]
      fn randomly_test_polynomial_translation_inverts (coefficients in prop::array::$uniform(-16 as $integer..16), input in -16 as $integer..16) {
        let translated = coefficients.all_taylor_coefficients (input);
        prop_assume! (translated.is_some());
        let translated = translated.unwrap();
        let translated_back = translated.all_taylor_coefficients (-input);
        prop_assert! (translated_back.is_some(), "we know that the original value was in bounds, so translating back should return some");
        prop_assert_eq! (coefficients, translated_back.unwrap());

      }

      #[test]
      fn randomly_test_taylor_coefficients_evaluates (coefficients in prop::array::$uniform(-16 as $integer..16), input in -16 as $integer..16) {
        let translated = coefficients.all_taylor_coefficients (input);
        prop_assume! (translated.is_some());
        let translated = translated.unwrap();
        let evaluated = naive_perfect_evaluate (&coefficients, BigRational::from(BigInt::from(input)));
        prop_assert_eq! (BigRational::from(BigInt::from(translated[0])), evaluated);
        for which in 0..$coefficients {
          prop_assert_eq! (BigRational::from(BigInt::from(translated[which])), naive_perfect_nth_taylor_coefficient(&coefficients, BigRational::from(BigInt::from(input)), which), "Incorrect {}th taylor coefficient ", which);
        }
      }

      #[test]
      fn randomly_test_taylor_coefficients_bounds_correct (coefficients in prop::array::$uniform(-16 as $integer..16), input in arbitrary_fractional_input(), precision_shift in 0u32..10) {
        let bounds = coefficients.all_taylor_coefficients_bounds (input.numerator, input.shift, precision_shift);
        prop_assume! (bounds.is_some());
        let bounds = bounds.unwrap();
        for which in 0..$coefficients {
          let exact = naive_perfect_nth_taylor_coefficient(&coefficients, rational_input(input), which) * precision_scale(precision_shift);

          prop_assert! (BigRational::from(BigInt::from(bounds[which][0])) <= exact, "Incorrect {}th taylor coefficient lower bound: {} > {}", which, bounds[which][0], exact);
          prop_assert! (BigRational::from(BigInt::from(bounds[which][1])) >= exact, "Incorrect {}th taylor coefficient upper bound: {} < {}", which, bounds[which][1], exact);
        }
      }

      #[test]
      fn randomly_test_taylor_coefficients_bounds_close(coefficients in prop::array::$uniform(-16 as $integer..16), input in arbitrary_fractional_input(), precision_shift in 0u32..10) {
        let bounds = coefficients.all_taylor_coefficients_bounds (input.numerator, input.shift, precision_shift);
        prop_assume! (bounds.is_some());
        let bounds = bounds.unwrap();
        let leeway = BigRational::new(BigInt::from(3i32), BigInt::from(2i32));
        for which in 0..$coefficients {
          let exact = naive_perfect_nth_taylor_coefficient(&coefficients, rational_input(input), which) * precision_scale(precision_shift);
          prop_assert! (BigRational::from(BigInt::from(bounds[which][0])) > &exact - &leeway, "Too loose {}th taylor coefficient lower bound: {} + 1.5 <= {}", which, bounds[which][0], exact);
          prop_assert! (BigRational::from(BigInt::from(bounds[which][1])) < &exact + &leeway, "Too loose {}th taylor coefficient upper bound: {} - 1.5 >= {}", which, bounds[which][1], exact);
          prop_assert! (bounds[which][1] <= bounds[which][0].saturating_add(2), "{}th taylor coefficient bounds are too far from each other (note: this should be impossible if the other conditions are met): {} > {} + 2", which, bounds[which][1], bounds[which][0]);
        }
      }


      #[test]
      fn randomly_test_next_time_definitely_lt_is_lt (coefficients in prop::array::$uniform(-16 as $integer..16), input in arbitrary_fractional_input(), permit_threshold in -16 as $integer..16, threshold_difference in 3..16) {
        let require_threshold = permit_threshold - threshold_difference;
        let time = coefficients.next_time_value_passes (input.numerator, input.shift, LessThanFilter::new(permit_threshold, require_threshold));
        prop_assume! (time .is_some());
        let time = time.unwrap();

        let exact = naive_perfect_nth_taylor_coefficient(&coefficients, rational_input(FractionalInput::new(time, input.shift)), 0);
        //if let Some(coefficients) = coefficients.all_taylor_coefficients_bounds (time, input.shift, 0u32) {
        prop_assert!(exact < BigRational::from(BigInt::from(permit_threshold)), "value {} at time {} was not less than permit_threshold {}", exact, time, permit_threshold);
      }

      #[test]
      fn randomly_test_next_time_definitely_lt_is_next (coefficients in prop::array::$uniform(-16 as $integer..16), input in arbitrary_fractional_input(), permit_threshold in -16 as $integer..16, threshold_difference in 3..16, test_frac in 0f64..1f64) {
        let require_threshold = permit_threshold - threshold_difference;
        let time = coefficients.next_time_value_passes (input.numerator, input.shift, LessThanFilter::new(permit_threshold, require_threshold));
        let last_not_lt = match time {
          None => $double::max_value(),
          Some(k) => {
            prop_assert!(k >= input.numerator);
            k-1
          },
        };
        prop_assume!(last_not_lt >= input.numerator);
        let test_time = input.numerator + ((last_not_lt.saturating_sub(input.numerator)) as f64 * test_frac).floor() as $double;

        let exact_require_threshold = BigRational::from(BigInt::from(require_threshold));
        let exact = naive_perfect_nth_taylor_coefficient(&coefficients, rational_input(input), 0);
        prop_assert!(exact >= exact_require_threshold);
        let exact = naive_perfect_nth_taylor_coefficient(&coefficients, rational_input(FractionalInput::new(last_not_lt, input.shift)), 0);
        prop_assert!(exact >= exact_require_threshold);
        let exact = naive_perfect_nth_taylor_coefficient(&coefficients, rational_input(FractionalInput::new(test_time, input.shift)), 0);
        prop_assert!(exact >= exact_require_threshold);
      }

      #[test]
      fn randomly_test_next_time_definitely_ge_is_ge(coefficients in prop::array::$uniform(-16 as $integer..16), input in arbitrary_fractional_input(), permit_threshold in -16 as $integer..16, threshold_difference in 3..16) {
        let require_threshold = permit_threshold + threshold_difference;
        let time = coefficients.next_time_value_passes (input.numerator, input.shift, GreaterThanEqualToFilter::new(permit_threshold, require_threshold));
        prop_assume! (time .is_some());
        let time = time.unwrap();

        let exact = naive_perfect_nth_taylor_coefficient(&coefficients, rational_input(FractionalInput::new(time, input.shift)), 0);
        //if let Some(coefficients) = coefficients.all_taylor_coefficients_bounds (time, input.shift, 0u32) {
        prop_assert!(exact >= BigRational::from(BigInt::from(permit_threshold)));
      }

      #[test]
      fn randomly_test_next_time_definitely_ge_is_next (coefficients in prop::array::$uniform(-16 as $integer..16), input in arbitrary_fractional_input(), permit_threshold in -16 as $integer..16, threshold_difference in 3..16, test_frac in 0f64..1f64) {
        let require_threshold = permit_threshold + threshold_difference;
        let time = coefficients.next_time_value_passes (input.numerator, input.shift, GreaterThanEqualToFilter::new(permit_threshold, require_threshold));
        let last_not_ge = match time {
          None => $double::max_value(),
          Some(k) => {
            prop_assert!(k >= input.numerator);
            k-1
          },
        };
        prop_assume!(last_not_ge >= input.numerator);
        let test_time = input.numerator + ((last_not_ge.saturating_sub(input.numerator)) as f64 * test_frac).floor() as $double;

        let exact_require_threshold = BigRational::from(BigInt::from(require_threshold));
        let exact = naive_perfect_nth_taylor_coefficient(&coefficients, rational_input(input), 0);
        prop_assert!(exact < exact_require_threshold);
        let exact = naive_perfect_nth_taylor_coefficient(&coefficients, rational_input(FractionalInput::new(last_not_ge, input.shift)), 0);
        prop_assert!(exact < exact_require_threshold);
        let exact = naive_perfect_nth_taylor_coefficient(&coefficients, rational_input(FractionalInput::new(test_time, input.shift)), 0);
        prop_assert!(exact < exact_require_threshold);
      }

      #[test]
      fn randomly_test_coefficient_bounds_on_integer_interval (coefficients in prop::array::$uniform(-16 as $integer..16), start in -16 as $double..16, duration in 0 as $double..16, test_frac in 0f64..1f64) {
        let first = coefficients.all_taylor_coefficients (start);
        prop_assume! (first.is_some());
        let first = first.unwrap();
        let second = coefficients.all_taylor_coefficients (start+duration);
        prop_assume! (second.is_some());
        let second = second.unwrap();

        let computed = coefficient_bounds_on_integer_interval([&first, &second], duration);
        let test_time = start + ((duration as f64+0.999) * test_frac).floor() as $double;
        for (exponent, bounds) in computed.iter().enumerate() {
          let exact = naive_perfect_nth_taylor_coefficient(&coefficients, BigRational::from(BigInt::from(test_time)), exponent);
          prop_assert!(bounds [0] == Bounded::min_value() || BigRational::from(BigInt::from(bounds [0])) <= exact);
          prop_assert!(bounds [1] == Bounded::max_value() || BigRational::from(BigInt::from(bounds [1])) >= exact);
        }
      }

      #[test]
      fn randomly_test_coefficient_bounds_on_tail (coefficients in prop::array::$uniform(-16 as $integer..16), start in -16 as $double..16, test_frac in 0f64..1f64) {
        let first = coefficients.all_taylor_coefficients (start);
        prop_assume! (first.is_some());
        let first = first.unwrap();

        let computed = coefficient_bounds_on_tail (&first);
        let test_time = start + max(0, (1.0/test_frac).floor() as $double);
        for (exponent, bounds) in computed.iter().enumerate() {
          let exact = naive_perfect_nth_taylor_coefficient(&coefficients, BigRational::from(BigInt::from(test_time)), exponent);
          prop_assert!(bounds [0] == Bounded::min_value() || BigRational::from(BigInt::from(bounds [0])) <= exact);
          prop_assert!(bounds [1] == Bounded::max_value() || BigRational::from(BigInt::from(bounds [1])) >= exact);
        }
      }

      #[test]
      fn randomly_test_value_bounds_on_negative_power_of_2_interval (coefficients in prop::array::$uniform(-16 as $integer..16), (start, duration_shift) in arbitrary_fractional_input().prop_flat_map(|input| (Just(input), 0..input.shift+1)), test_frac in 0f64..1f64) {
        let first = coefficients.all_taylor_coefficients_bounds (start.numerator, start.shift, STANDARD_PRECISION_SHIFT);
        prop_assume! (first.is_some());
        let first = first.unwrap();
        let duration = 1<<(start.shift - duration_shift);
        let second = coefficients.all_taylor_coefficients_bounds (start.numerator+duration, start.shift, STANDARD_PRECISION_SHIFT);
        prop_assume! (second.is_some());
        let second = second.unwrap();

        let bounds: [$integer; 2] = value_bounds_on_negative_power_of_2_interval([&first, &second], duration_shift);
        let test_time = start.numerator + ((duration as f64+0.999) * test_frac).floor() as $double;
        let test_time = rational_input(FractionalInput::new(test_time, start.shift));

        let exact = naive_perfect_nth_taylor_coefficient(&coefficients, test_time.clone(), 0);
        prop_assert!(BigRational::from(BigInt::from(bounds [0])) <= exact);
        prop_assert!(BigRational::from(BigInt::from(bounds [1])) >= exact);
      }
  }
}}
macro_rules! test_squarable_polynomials {
  ($($coefficients: expr, $integer: ident, $double: ident, $uniform: ident, $name: ident,)*) => {
$(
  mod $name {
  use super::*;

  proptest! {

      #[test]
      fn randomly_test_next_time_magnitude_squared_definitely_gt_is_gt(coefficients in prop::array::uniform2(prop::array::$uniform(-16 as $integer..16)), input in arbitrary_fractional_input(), permit_threshold in 16 as $integer..1024, threshold_difference in 3..16) {
        let require_threshold = permit_threshold + threshold_difference;
        let coefficients_slices: Vec<_> = coefficients.iter().map (| polynomial | polynomial.as_slice()).collect();
        let time = <[$integer; $coefficients] as PolynomialMagnitudeSquaredRangeSearch<$double>>::next_time_magnitude_squared_passes (coefficients.as_slice(), input.numerator, input.shift, GreaterThanFilter::new(permit_threshold, require_threshold));
        prop_assume! (time .is_some());
        let time = time.unwrap();

        let exact = naive_perfect_evaluate_magnitude_squared(coefficients_slices.as_slice(), rational_input(FractionalInput::new(time, input.shift)));
        //if let Some(coefficients) = coefficients.all_taylor_coefficients_bounds (time, input.shift, 0u32) {
        prop_assert!(exact > BigRational::from(BigInt::from(permit_threshold)), "expected above {} but was {}", permit_threshold, exact);
      }

      #[test]
      fn randomly_test_next_time_magnitude_squared_definitely_gt_is_next (coefficients in prop::array::uniform2(prop::array::$uniform(-16 as $integer..16)), input in arbitrary_fractional_input(), permit_threshold in 16 as $integer..100024, threshold_difference in 3..16, test_frac in 0f64..1f64) {
        let require_threshold = permit_threshold + threshold_difference;
        let coefficients_slices: Vec<_> = coefficients.iter().map (| polynomial | polynomial.as_slice()).collect();
        let time = <[$integer; $coefficients] as PolynomialMagnitudeSquaredRangeSearch<$double>>::next_time_magnitude_squared_passes (coefficients.as_slice(), input.numerator, input.shift, GreaterThanFilter::new(permit_threshold, require_threshold));

        let last_not_lt = match time {
          None => $double::max_value(),
          Some(k) => {
            prop_assert!(k >= input.numerator);
            k-1
          },
        };
        prop_assume!(last_not_lt >= input.numerator);
        let test_time = input.numerator + ((last_not_lt.saturating_sub(input.numerator)) as f64 * test_frac).floor() as $double;

        let exact_require_threshold = BigRational::from(BigInt::from(require_threshold));
        let exact = naive_perfect_evaluate_magnitude_squared(coefficients_slices.as_slice(), rational_input(input));
        prop_assert!(exact <= exact_require_threshold, "at time {}, earlier than {:?}, was {} but should have been <= {}", input.numerator, time, exact, exact_require_threshold);
        let exact = naive_perfect_evaluate_magnitude_squared(coefficients_slices.as_slice(), rational_input(FractionalInput::new(last_not_lt, input.shift)));
        prop_assert!(exact <= exact_require_threshold, "at time {}, earlier than {:?}, was {} but should have been <= {}", last_not_lt, time, exact, exact_require_threshold);
        let exact = naive_perfect_evaluate_magnitude_squared(coefficients_slices.as_slice(), rational_input(FractionalInput::new(test_time, input.shift)));
        prop_assert!(exact <= exact_require_threshold, "at time {}, earlier than {:?}, was {} but should have been <= {}", test_time, time, exact, exact_require_threshold);
      }

    }
  }
)*

  }
}

test_polynomial!(1, i32, i64, uniform1, polynomial_tests_1,);
test_nontrivial_polynomial!(2, i32, i64, uniform2, polynomial_tests_2,);
test_nontrivial_polynomial!(3, i32, i64, uniform3, polynomial_tests_3,);
test_nontrivial_polynomial!(4, i32, i64, uniform4, polynomial_tests_4,);
test_nontrivial_polynomial!(5, i32, i64, uniform5, polynomial_tests_5,);

test_squarable_polynomials!(
  /*1,
  i32,
  i64,
  uniform1,
  polynomial_tests_12,*/
  2,
  i32,
  i64,
  uniform2,
  polynomial_tests_22,
  3,
  i32,
  i64,
  uniform3,
  polynomial_tests_32,
);

/*#[test]
fn print_record_data() {
   use std::io::Write;
   let mut data: Vec<Vec<RangeSearchRecordHack>> = serde_json::from_reader (::std::fs::File::open("/n/pfft/range_search_data.json").unwrap()).unwrap();
   data.sort_by_key(|datum|datum.len());
   let percentiles:Vec<usize> = (0..=100).map(|percent| data[percent*(data.len()-1)/100].len()).collect();
   write!(::std::io::stderr(), "Number of searches: {:?}", data.len());
   write!(::std::io::stderr(), "Sizes: {:?}", percentiles);
   write!(::std::io::stderr(), "Worst: {}", serde_json::to_string_pretty(data.last().unwrap()).unwrap());
}*/
