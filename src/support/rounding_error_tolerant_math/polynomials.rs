use std::cmp::{max, min};

use super::range::*;

macro_rules! printlnerr(
    ($($arg:tt)*) => { {use std::io::Write;
        let r = writeln!(&mut ::std::io::stderr(), $($arg)*);
        r.expect("failed printing to stderr");
    } }
);


/**

A polynomial pseudo-solver, using Range.

Returns a collection of ranges that include the exact roots. False-positives are possible.

TODO: instead of Vec<Range>, these should return a stack-allocated type.

*/

pub fn roots_linear(coefficients: [Range; 2], min_input: i64, max_input: i64) -> Vec<Range> {
  if coefficients[1] == Range::exactly(0) && !coefficients[0].includes_0() {
    return Vec::new();
  }
  if let Some(result) = ((-coefficients[0]) / coefficients[1]).clamp_to_0_exponent() {
    if result.max() >= min_input && result.min() <= max_input {
      return vec![result];
    }
  }
  Vec::new()
}
pub fn roots_quadratic(terms: [Range; 3], min_input: i64, max_input: i64) -> Vec<Range> {
  let a = terms[2];
  let b = terms[1];
  let c = terms[0];
  let discriminant = b.squared() - a * c * Range::exactly(4);
  // printlnerr!(" discriminant {:?}", discriminant);
  // printlnerr!("confirm results: {:?}", roots_derivative_based (& terms));

  if discriminant < 0 {
    return Vec::new();
  }
  let sqrt = discriminant.sqrt()
    .expect("I thought we just ruled out the case where the square root would be nonexistent");
  // printlnerr!(" sqrt {:?}", sqrt);
  let result_0 = (-b - sqrt) / (a * 2);
  let result_1 = (-b + sqrt) / (a * 2);
  // printlnerr!(" result 0 {:?}", result_0);
  // printlnerr!(" result 1 {:?}", result_1);
  let mut results = Vec::new();
  if let Some(result) = result_0.clamp_to_0_exponent() {
    if result.max() >= min_input && result.min() <= max_input {
      results.push(result);
    }
  }
  if let Some(result) = result_1.clamp_to_0_exponent() {
    if result.max() >= min_input && result.min() <= max_input {
      if results.last().map_or(false, |whatever| result.min() < whatever.min()) {
        results.insert(0, result);
      } else {
        results.push(result);
      }
    }
  }

  // printlnerr!("My results: {:?}", results);
  return results;
  // if result_0.max >= result_1.min {
  // vec![Range {
  // min: result_0.min,
  // max: result_1.max,
  // exponent: 0,
  // }]
  // } else {
  // vec![result_0, result_1]
  // }
}


fn find_root_search <Metadata: Copy, InputStrategy: Fn (i64, i64, Range, Range, Metadata)->i64, MetadataGenerator: Fn (Range)->Metadata, MetadataTransformer: Fn (Metadata, Range, Range)->Metadata> (terms: &[Range],
                    min_only: bool,
                    max_only: bool,
                    input_1: i64,
                    input_2: i64,
                    value_1: Range,
                    value_2: Range,
                    value_1_metadata: Metadata,
                    input_strategy: &InputStrategy,
                    metadata_generator: &MetadataGenerator,
                    metadata_transformer: &MetadataTransformer)
                    -> (i64, i64) {
  assert!(!(value_1.includes_0() && value_2.includes_0()));
  if !min_only {
    assert!((value_1 < 0) != (value_2 < 0));
  }
  if !max_only {
    assert!((value_1 > 0) != (value_2 > 0));
  }

  let mut input_1: i64 = input_1;
  let mut input_2: i64 = input_2;
  let mut value_1: Range = value_1;
  let mut value_1_metadata = value_1_metadata;
  let mut value_2: Range = value_2;
  let mut result_for_other: i64 = 0;
  let mut min_only = min_only;
  loop {
    let input = input_strategy (input_1, input_2, value_1, value_2, value_1_metadata);
    if input == input_1 || input == input_2 {
      break;
    }

    let value = evaluate(terms, input);

    let closer_to_1;
    if min_only {
      closer_to_1 = (value > 0) != (value_2 > 0);
    } else if max_only {
      closer_to_1 = (value < 0) != (value_2 < 0);
    } else {
      closer_to_1 = (value > 0) != (value_2 > 0);
      let other_closer_to_1 = (value < 0) != (value_2 < 0);

      if closer_to_1 != other_closer_to_1 {
        min_only = true;
        if other_closer_to_1 {
          result_for_other =
            find_root_search(terms, false, true, input_2, input, value_2, value, metadata_generator (value_2), input_strategy, metadata_generator, metadata_transformer).0;
        } else {
          // possible optimization: use a better factor, referring to "A Family of Regula Falsi Methods", Galdino
          result_for_other = find_root_search(terms,
                                              false,
                                              true,
                                              input_1,
                                              input,
                                              value_1,
                                              value,
                                              metadata_transformer (value_1_metadata, value_2, value),
                                              input_strategy, metadata_generator, metadata_transformer)
            .0;
        }
      }
    }
    if closer_to_1 {
      input_1 = input_2;
      value_1 = value_2;
      value_1_metadata = metadata_generator (value_2);
    } else {
      value_1_metadata = metadata_transformer (value_1_metadata, value_2, value);
    }
    input_2 = input;
    value_2 = value;
  }
  if max_only {
    assert!((value_1 < 0) != (value_2 < 0));
    (if value_1 < 0 { input_1 } else { input_2 }, result_for_other)
  } else {
    assert!((value_1 > 0) != (value_2 > 0));
    (if value_1 > 0 { input_1 } else { input_2 }, result_for_other)
  }
}

fn find_root_search_default (terms: &[Range], min_only: bool,
                    max_only: bool,
                    input_1: i64,
                    input_2: i64,
                    value_1: Range,
                    value_2: Range)
                    -> (i64, i64) {
  
  let floating = | whatever: Range | (whatever.internal_min() as f64)*(2f64.powi (whatever.exponent() as i32));
  let relaxed_result = find_root_search (terms, min_only, max_only, input_1, input_2, value_1, value_2, floating (value_1),
    & |input_1, input_2, _, value_2, value_1_metadata| {
    let mut input;
    let value_2 = floating (value_2);
    let denominator = value_2 - value_1_metadata;
    input = ((input_2 as f64) -
             value_2 * ((input_2 as f64) - (input_1 as f64)) / denominator) as i64;
    if input.cmp(&input_2) != input.cmp(&input_1).reverse() {
      input = average_round_towards_neginf(input_1, input_2);
    }
    input
    }, & floating, &| fa,_,_| {
      // refer to "A Family of Regula Falsi Methods", Galdino
      // this method is written down as generally slightly the best in number of function evaluations,
      // but it profiled slightly worse, probably just because it uses more operations.
      // let mut m = 1f64 - floating (fx)/floating (fb);
      // if m <= 0f64 {m = 0.5;}
      // fa*m
      fa * 0.5
    });
  if cfg! (debug_assertions) {
  let strict_result = find_root_search (terms, min_only, max_only, input_1, input_2, value_1, value_2, value_1,
    & |input_1, input_2, _, value_2, value_1_metadata| {
    let mut input;
    let denominator = (value_2 - value_1_metadata).rounded_to_middle_towards_neginf();
    if denominator.includes_0() {
      input = average_round_towards_neginf(input_1, input_2);
    } else {
      input = (Range::exactly(input_2) -
               value_2 * (Range::exactly(input_2) - Range::exactly(input_1)) / denominator)
        .clamp_to_0_exponent()
        .unwrap()
        .min();
      if input.cmp(&input_2) != input.cmp(&input_1).reverse() {
        input = average_round_towards_neginf(input_1, input_2);
      }
    }
    input
    }, & | whatever: Range | whatever, & | value,_,_ | value >> 1);
  assert! (relaxed_result == strict_result);
  }
  relaxed_result
}

fn find_root(terms: &[Range], min: i64, max: i64) -> Option<Range> {
  if min >= max {
    return None;
  }
  let min_value = evaluate(terms, min);
  let max_value = evaluate(terms, max);
  // printlnerr!(" Values {:?}:{:?}, {:?}:{:?}", min, min_value, max, max_value);

  if min_value.includes_0() {
    if max_value.includes_0() {
      Some(Range::new(min, max))
    } else {
      let search_by_min = max_value > 0;
      Some(Range::new(min,
                      find_root_search_default(terms,
                                       search_by_min,
                                       !search_by_min,
                                       min,
                                       max,
                                       min_value,
                                       max_value)
                        .0))
    }
  } else if max_value.includes_0() {
    let search_by_min = min_value > 0;
    Some(Range::new(find_root_search_default(terms,
                                     search_by_min,
                                     !search_by_min,
                                     min,
                                     max,
                                     min_value,
                                     max_value)
                      .0,
                    max))
  } else if max_value.min_signum() == min_value.min_signum() {
    None
  } else {
    let (result_for_min, result_for_max) = find_root_search_default(terms,
                                                            false,
                                                            false,
                                                            min,
                                                            max,
                                                            min_value,
                                                            max_value);
    Some(Range::new_either_order(result_for_min, result_for_max))
  }
  // return find_root_search (terms, false, min, max,
  // let mut lower_bound = min;
  // let mut upper_bound = max;
  // hack: use a negative number for move_size so that it can store a slightly larger value
  // let mut move_size = -1i64 << 63;
  // while min.checked_sub(max).is_some() && move_size < min - max {
  // move_size /= 2;
  // }
  // while move_size < 0 {
  // printlnerr!(" Next values {:?}:{:?}, {:?}:{:?}", lower_bound, evaluate (terms, lower_bound ), upper_bound, evaluate (terms, upper_bound ));
  //
  // if lower_bound - move_size <= max &&
  // (evaluate(terms, lower_bound - move_size) * direction).max <= 0 {
  // lower_bound -= move_size;
  // }
  // if upper_bound + move_size >= min &&
  // (evaluate(terms, upper_bound + move_size) * direction).min >= 0 {
  // upper_bound += move_size;
  // }
  // move_size /= 2;
  // }
  // Some(Range::new(lower_bound, upper_bound))

}
fn collect_root(terms: &[Range], min: i64, max: i64, bucket: &mut Vec<Option<Range>>) {
  bucket.push(find_root(terms, min, max));
}


fn roots_derivative_based(terms: &[Range], min_input: i64, max_input: i64) -> Vec<Range> {

  let derivative: Vec<Range> = terms[1..]
    .iter()
    .enumerate()
    .map(|(which, term)| term * (which as i64 + 1))
    .collect();
  // printlnerr!(" Derivative {:?}", derivative);
  let extrema = roots(derivative.as_slice(), min_input, max_input);
  // printlnerr!("extrema {:?}", extrema);
  let mut bucket = Vec::new();
  let mut results = Vec::new();
  if extrema.is_empty() {
    collect_root(terms, min_input, max_input, &mut bucket);
  } else {
    collect_root(terms, min_input, extrema[0].min(), &mut bucket);
    for which in 0..(extrema.len() - 1) {
      collect_root(terms,
                   max(extrema[which].max(), min_input),
                   min(extrema[which + 1].min(), max_input),
                   &mut bucket);
    }
    collect_root(terms, extrema.last().unwrap().max(), max_input, &mut bucket);
  }
  // if we found a root on both sides of a derivative-root, we know that the derivative-root is bounded away from 0
  for which in 0..extrema.len() {
    let me = extrema[which];
    if let Some(lower) = bucket[which] {
      results.push(lower);
      if let Some(higher) = bucket[which + 1] {
        if lower < me && me < higher {
          continue;
        }
      }
    }
    results.push(me);
  }
  if let Some(lower) = bucket[extrema.len()] {
    results.push(lower);
  }

  results

}
pub fn roots(terms: &[Range], min: i64, max: i64) -> Vec<Range> {
  let mut terms = terms;
  while terms.last().map_or(false, |term| term == &Range::exactly(0)) {
    terms = &terms[..terms.len() - 1]
  }
  match terms.len() {
    0 => vec![Range::new(min, max)],
    1 => {
      if terms[0].internal_min() <= 0 && terms[0].internal_max() >= 0 {
        vec![Range::new(min, max)]
      } else {
        Vec::new()
      }
    }
    2 => roots_linear([terms[0], terms[1]], min, max),
    3 => roots_quadratic([terms[0], terms[1], terms[2]], min, max),
    _ => roots_derivative_based(terms, min, max),
  }
}

pub fn evaluate(terms: &[Range], input: i64) -> Range {
  let mut factor = Range::exactly(1);
  let mut result = Range::exactly(0);
  for term in terms.iter() {
    result = result + (term * factor);
    factor = factor * input;
  }
  result
}

pub fn multiply_polynomials(terms_0: &[Range], terms_1: &[Range]) -> Vec<Range> {
  (0..terms_0.len() + terms_1.len() - 1)
    .map(|new_index| {
      (max(terms_1.len(), new_index + 1) - terms_1.len()..min(terms_0.len(), new_index + 1))
        .map(|view| terms_0[view] * terms_1[new_index - view])
        .sum()
    })
    .collect()
}
use rand::Rng;
use rand;
// when coercing an update to land on an integer value, we obviously have a possible rounding error of up to 2 units (one from dividing the velocity, one from dividing the acceleration).
// But that's not all. The multiplications also have rounding error if they have to prevent overflows.
// we are only guaranteed to keep the top 31 bits of each factor, so that's a possible error factor of just below 1+2^{-30} for each of them.
// Square that error because there are 2 inputs in each multiplication,
// and square it again because we do 2 multiplications in a row for the acceleration.
// (1+2^{-30})^4 is a little bit above 1+2^{-28}.
// That error factor is multiplied specifically with the *distance traveled*
// or rather, it's multiplied with the absolute values of the quadratic term of the distance traveled,
// and then added to the error of the linear term, which is a little bit above 1+2^{-29}.
// The relationship between this error and the ACTUAL distance traveled is a little more complicated,
// since the 2 terms can point in opposite directions. In the worst case, the error can get up to
// more than 8 times the 1+2^{-28} figure for the same actual distance. Less than 16, though.
// So chopping off another 4 bits will be enough: 1+2^{-24}.
// So any constant error term is associated with a maximum distance traveled that will have no more than that much error.
pub fn max_error_for_distance_traveled(distance: i64) -> i64 {
  right_shift_round_up(distance, 24)
}

const DO_TESTS: bool = cfg!(debug_assertions);

// We require the user to pass in a max error value – specifically, the one that they use with
// quadratic_trajectories_possible_distance_crossing_intervals –
// so that we can check to make sure they didn't go beyond the bounds of what they tested for.
pub fn quadratic_move_origin_rounding_change_towards_0(terms: &mut [i64],
                                                       origin: i64,
                                                       input_scale_shift: u32,
                                                       max_error: i64)
                                                       -> bool {
  let distance_traveled = ((Range::exactly(terms[1]) * origin) >> input_scale_shift) +
                          ((Range::exactly(terms[2]) * origin * origin) >> (input_scale_shift * 2));

  if distance_traveled.max() - distance_traveled.min() > max_error * 2 {
    printlnerr!("overflow-ish in quadratic_move_origin_rounding_change_towards_0; error size \
                 exceeded the given max error");
    return false;
  }
  let mut between_time = 0;
  let mut confirm = [Range::exactly(0);3];
  if DO_TESTS {
    between_time = rand::thread_rng().gen_range(0, origin + 1);
    confirm = quadratic_future_proxy_minimizing_error(terms, between_time, input_scale_shift, max_error);
  }
  terms[0] += distance_traveled.rounded_towards_0();
  terms[1] += ((Range::exactly(terms[2]) * origin) >> (input_scale_shift - 1)).rounded_towards_0();
  if DO_TESTS {
    let experimented = evaluate(&confirm, origin - between_time) >> (input_scale_shift * 2);
    // printlnerr!("experimented {}, actually {}", experimented, terms [0]);
    assert!(experimented.includes(&Range::exactly(terms[0])));
  }
  true
}

pub fn quadratic_future_proxy_minimizing_error(terms: &[i64],
                                               origin: i64,
                                               input_scale_shift: u32,
                                               max_error: i64)
                                               -> [Range; 3] {
  // in the constant term, preserve the error of 2 units noted above.
  // Multiplication error term is about (term 1*time since original origin) >> 30+shift + (term 2*time since original origin squared) >> 29+shift*2
  // but time since original origin is actually "origin" + the input of the quadratic we're creating,
  // this error is actually quadratic.
  [(Range::new(terms[0] - 2 - max_error, terms[0] + 2 + max_error) << (input_scale_shift * 2)) +
   ((Range::exactly(terms[1]) * origin) << input_scale_shift) +
   (Range::exactly(terms[2]) * origin * origin),

   (Range::exactly(terms[1]) << input_scale_shift) + ((Range::exactly(terms[2]) * origin) << 1),

   Range::exactly(terms[2])]
}


pub fn time_until_which_quadratic_trajectory_may_remain_in_bounds(start_time: i64,
                                                                  trajectory: &[[i64; 3]],
                                                                  bounds: &[[i64; 2]],
                                                                  input_scale_shift: u32,
                                                                  max_error: i64)
                                                                  -> Option<i64> {
  assert!(trajectory.len() == bounds.len());
  assert!(trajectory.len() > 0);
  let mut min_input = start_time;
  let mut max_input = i64::max_value() - max(0, start_time);
  // printlnerr!("begin {:?} {:?} {:?}", start_time, trajectory, bounds);
  for (third, more) in trajectory.iter().zip(bounds.iter()) {
    let mut rubble =
      quadratic_future_proxy_minimizing_error(third, 0, input_scale_shift, max_error);
    rubble[0] = rubble[0] - (Range::new(more[0], more[1]) << (input_scale_shift * 2));
    let possible_overlap_times = roots(&rubble, min_input, max_input);
    // printlnerr!("roots {:?} {:?}", rubble, possible_overlap_times);
    if let Some((this_min, this_max)) = if possible_overlap_times.is_empty() {
      None
    } else if possible_overlap_times.len() == 2 &&
                                                  possible_overlap_times[0].max() >=
                                                  possible_overlap_times[1].min() - 1 {
      if possible_overlap_times[0].min() <= start_time &&
         possible_overlap_times[1].max() >= start_time {
        Some((possible_overlap_times[0].min(), possible_overlap_times[1].max()))
      } else {
        None
      }
    } else {
      possible_overlap_times.iter()
        .find(|root| root.min() <= start_time && root.max() >= start_time)
        .map(|root| (root.min(), root.max()))
    } {
      min_input = max(min_input, this_min);
      max_input = min(max_input, this_max);
      assert!(min_input <= max_input,
              "an interval containing start_time should never exclude it");
    } else {
      return None;
    }

  }
  // printlnerr!("end {} {}", min_input, max_input);
  Some(max_input)
}


pub fn quadratic_trajectories_possible_distance_crossing_intervals(distance: i64,
                                                                   first: (i64, &[[i64; 3]]),
                                                                   second: (i64, &[[i64; 3]]),
                                                                   input_scale_shift: u32,
                                                                   max_error: i64)
                                                                   -> Vec<Range> {
  assert!(first.1.len() == second.1.len());
  assert!(first.1.len() > 0);
  let base = max(first.0, second.0);
  let mut proxy =
    [Range::exactly(0), Range::exactly(0), Range::exactly(0), Range::exactly(0), Range::exactly(0)];
  let mut min_input = 0;
  let mut max_input = i64::max_value() - max(0, base);
  for (third, more) in first.1.iter().zip(second.1.iter()) {
    let mut rubble = quadratic_future_proxy_minimizing_error(third.as_ref(),
                                                             base - first.0,
                                                             input_scale_shift,
                                                             max_error);
    let bravo = quadratic_future_proxy_minimizing_error(more.as_ref(),
                                                        base - second.0,
                                                        input_scale_shift,
                                                        max_error);
    for index in 0..3 {
      rubble[index] = rubble[index] - bravo[index];
    }
    let this_dimension_tester =
      [rubble[0] + (Range::error_sized(distance) << (input_scale_shift * 2)), rubble[1], rubble[2]];
    let possible_overlap_times = roots(&this_dimension_tester, min_input, max_input);
    // printlnerr!("one-dimensional proxy: {:?} {:?} {:?} {:?}", min_input, max_input, this_dimension_tester, possible_overlap_times );

    if possible_overlap_times.is_empty() {

      return Vec::new();
    } else {
      min_input = max(min_input, possible_overlap_times[0].min());
      max_input = min(max_input, possible_overlap_times.last().unwrap().max());
      if min_input > max_input {
        return Vec::new();
      }
    }
    for (which, value) in multiply_polynomials(&rubble, &rubble).into_iter().enumerate() {
      proxy[which] = proxy[which] + value
    }
  }
  proxy[0] = proxy[0] - (Range::exactly(distance).squared() << (input_scale_shift * 4));
  let real_distance_squared = |input| {
    let mut result = 0i64;
    for (third, more) in first.1.iter().zip(second.1.iter()) {
      let mut rubble = third.clone();
      if !quadratic_move_origin_rounding_change_towards_0(&mut rubble,
                                                          input - first.0,
                                                          input_scale_shift,
                                                          max_error) {
        return None;
      }
      let mut bravo = more.clone();
      if !quadratic_move_origin_rounding_change_towards_0(&mut bravo,
                                                          input - second.0,
                                                          input_scale_shift,
                                                          max_error) {
        return None;
      }
      for index in 0..3 {
        rubble[index] = rubble[index] - bravo[index];
      }
      if let Some(term) = rubble[0].checked_mul(rubble[0]) {
        if let Some(res) = result.checked_add(term) {
          result = res;
        } else {
          return None;
        }
      } else {
        return None;
      }
    }
    Some(result)

  };
  let test = |input| {
    let evaluated = evaluate(&proxy, input);
    // printlnerr!("input: {}, base: {}, evaluated: {}", input, base, evaluated);
    if input < 0 || input > 1i64 << 32 {
      return evaluated;
    }
    if let Some(distance_squared) = real_distance_squared(input + base) {
      let real = distance_squared - distance * distance;
      // printlnerr!("real: {}", real);
      assert!((evaluated >> (input_scale_shift * 4)).includes(&Range::exactly(real)));
    }
    evaluated
  };
  let test_empty_interval = |start, stop| {
    // Currently, evaluate() is more permissive than it theoretically needs to be.
    // It could include 0 even if the polynomial couldn't actually emit 0 from that input.
    // roots_derivative_based() uses evaluate() directly, so it's fine to assume that evaluate() is correct.
    // However, roots_quadratic() might return a slightly tighter result.
    // So we can't test quadratics in quite the same way.
    if proxy[3] == Range::exactly(0) && proxy[4] == Range::exactly(0) {
      return;
    }
    if start >= stop {
      return;
    }
    let sample_points: Vec<i64> = vec![start,
                                       stop,
                                       rand::thread_rng().gen_range(start, stop),
                                       rand::thread_rng().gen_range(start, stop),
                                       rand::thread_rng().gen_range(start, stop)];
    let sample_values: Vec<Range> = sample_points.iter().map(|input| test(input.clone())).collect();
    let signum = sample_values[0].internal_min().signum();
    for value in sample_values.iter() {
      if value.includes_0_strictly() || value.internal_min().signum() == -signum {
        printlnerr!(" Proxy: {:?}", proxy);
        printlnerr!("fail points: {:?}\n values: {:?}",
                    sample_points,
                    sample_values);
        panic!()
      }
    }

  };

  // printlnerr!(" Proxy: {:?}", proxy);

  let mut result = roots(proxy.as_ref(), min_input, max_input);
  // printlnerr!(" Proxy: {:?}\n Roots: {:?}", proxy, result);
  if DO_TESTS {
    test(0);
    test(1000);
    test(base);
    for (which, root) in result.iter().enumerate() {
      test((root.max() - root.min()) / 2);
      if which == 0 {
        test_empty_interval(min_input, root.min() - 1);
      }
      if which < result.len() - 1 {
        test_empty_interval(root.max() + 1, result[which + 1].min() - 1);
      } else {
        test_empty_interval(root.max() + 1, max_input);
      }
      // printlnerr!("root check: {}: {} and then {} and then {}", root, evaluate (& proxy, root.max - 1),  evaluate (& proxy, root.max()), evaluate (& proxy, root.max() + 1));
    }
  }
  for root in result.iter_mut() {
    *root = *root + Range::exactly(base);
  }
  result
}

#[cfg (test)]
mod tests {
  use super::super::*;

  fn test_roots(given_roots: Vec<Range>) {
    let mut polynomial = vec![Range::exactly(1)];
    for root in given_roots.iter() {
      polynomial = multiply_polynomials(polynomial.as_slice(), &[-root, Range::exactly(1)])
    }
    let computed = roots(polynomial.as_slice(), -i64::max_value(), i64::max_value());
    println!("\nFor roots {:?}\n  Computed polynomial {:?}\n  And roots {:?}\n  Evaluated root \
              minima: {:?}",
             given_roots,
             polynomial,
             computed,
             given_roots.iter()
                        .map(|root| evaluate(polynomial.as_slice(), root.min()))
                        .collect::<Vec<Range>>());
  }

  // quickcheck! {
  // fn automatic_roots(given_roots: Vec<Range>)
  // }

  #[test]
  fn explicit_roots() {
    test_roots(vec![Range::exactly(0)]);
    test_roots(vec![Range::exactly(55)]);
    test_roots(vec![Range::exactly(0), Range::exactly(55)]);
    test_roots(vec![Range::exactly(-8), Range::exactly(55)]);
    test_roots(vec![Range::exactly(-8), Range::exactly(55), Range::exactly(999)]);
    test_roots(vec![Range::exactly(-8),
                    Range::exactly(55),
                    Range::exactly(999),
                    Range::exactly(-84)]);
    test_roots(vec![Range::exactly(-8),
                    Range::exactly(55),
                    Range::exactly(999),
                    Range::exactly(-84),
                    Range::exactly(-1967)]);

    test_roots(vec![Range::new(-1, 1), Range::new(54, 56)]);
    test_roots(vec![Range::new(-9, -7), Range::new(50, 60)]);
    test_roots(vec![Range::new(-9, -7), Range::new(54, 56), Range::exactly(999)]);
    test_roots(vec![Range::new(-9, -7),
                    Range::new(54, 56),
                    Range::new(950, 1050),
                    Range::new(-90, -80)]);
    test_roots(vec![Range::new(-9, -7),
                    Range::new(54, 56),
                    Range::new(950, 1050),
                    Range::new(-90, -80),
                    Range::new(-1967, -1940)]);





    println!(" {:?}",
             roots(&[Range::new(-900, -800), Range::new(500, 501), Range::exactly(50)],
                   -i64::max_value(),
                   i64::max_value()));
    println!(" {:?}",
             roots(&[Range::new(-900, -800),
                     Range::new(500, 501),
                     Range::exactly(50),
                     Range::exactly(1)],
                   -i64::max_value(),
                   i64::max_value()));
  }

}
