// In this example, we're going to make a simple diffusion simulation:
// a two-dimensional grid of cells, each containing some amount of ink.
// Ink diffuses from cells with more ink the cells with less ink.

// All field data types must implement serde::Serialize and serde::Deserialize,
// so we include the features for deriving them.
extern crate serde;
#[macro_use]
extern crate serde_derive;

#[macro_use]
extern crate time_steward;

// Imports for the UI
#[macro_use]
extern crate glium;
extern crate docopt;
#[path = "../dev-shared/emscripten_compatibility.rs"] mod emscripten_compatibility;
pub use emscripten_compatibility::canvas_click;

macro_rules! printlnerr(
    ($($arg:tt)*) => { {use std::io::Write;
        let r = writeln!(&mut ::std::io::stderr(), $($arg)*);
        r.expect("failed printing to stderr");
    } }
);


use std::cmp::{min, max};
use time_steward::{DeterministicRandomId};
use time_steward::rowless::api::{self, PersistentTypeId, PersistentlyIdentifiedType, ListedType, StewardData, QueryOffset, DataHandleTrait, DataTimelineCellTrait, ExtendedTime, Basics as BasicsTrait};
use time_steward::rowless::stewards::{simple_flat as steward_module};
use steward_module::{TimeSteward, ConstructibleTimeSteward, IncrementalTimeSteward, Event, DataHandle, DataTimelineCell, EventHandle, Accessor, EventAccessor, FutureCleanupAccessor, SnapshotAccessor, simple_timeline};
use simple_timeline::{SimpleTimeline, GetVarying, IterateUniquelyOwnedPredictions, tracking_query, modify_simple_timeline, unmodify_simple_timeline};


use time_steward::support::rounding_error_tolerant_math::Range;

/// i64 makes a good time type:
/// It's big enough to subdivide down to the nanosecond, allowing a smooth simulation,
/// while still representing any reasonable amount of time the simulation could take.
type Time = i64;
const SECOND: Time = 1i64 << 20;

type Steward = steward_module::Steward <Basics>;


/// A type defining simulation constants.
/// This obviously isn't needed for compile-time constants, which can be done normally,
/// but it's useful for things like game settings or commandline parameters.
#[derive (Clone, PartialEq, Eq, Serialize, Deserialize, Debug)]
struct Globals {
  size: [i32; 2],
  
  /// Maximum inaccuracy of transfers in ink-per-update
  max_inaccuracy: i64,
  
  cells: Vec<Cell>,
}
impl StewardData for Globals {}

// Derive all the traits required for field data types.
#[derive (Clone, PartialEq, Eq, Serialize, Deserialize, Debug)]
struct Cell {
  varying: DataTimelineCell <SimpleTimeline <CellVarying, Steward>>,
  transfers: [DataTimelineCell <SimpleTimeline <TransferVarying, Steward>>; 2],
}
#[derive (Clone, PartialEq, Eq, Serialize, Deserialize, Debug)]
struct CellVarying {
  /// The exact amount of ink present in this cell at the last time we updated it.
  last_change: Time,
  ink_at_last_change: i64,
}
#[derive (Clone, PartialEq, Eq, Serialize, Deserialize, Debug)]
struct TransferVarying {
  /// The rate at which this cell is currently transferring ink to other cells.
  /// 
  /// The mathematically accurate transfer function between just 2 cells would be
  /// exponential decay, but it would be quickly complicated by other cells anyway. 
  /// Instead, we use a linear approximation, and just update it whenever
  /// it becomes too inaccurate.
  /// 
  /// The values here represent the transfer TOWARDS the cells in the POSITIVE
  /// x and y directions. Transfers from the cells in the negative x and y directions
  /// are stored in the data of *those* cells.
  rate: i64,
  last_change: Time,
  accumulated_error: i64,
  next_change: Option <EventHandle <Basics>>,
}
impl StewardData for Cell {}
impl StewardData for CellVarying {}
impl StewardData for TransferVarying {}
impl IterateUniquelyOwnedPredictions <Steward> for CellVarying {}
impl IterateUniquelyOwnedPredictions <Steward> for TransferVarying {
  fn iterate_predictions <F: FnMut (& <Steward as TimeSteward>::EventHandle)> (&self, callback: &mut F) {
    if let Some (prediction) = self.next_change.as_ref() {
      callback (prediction);
    }
  }
}

fn ink_at (cell: &CellVarying, accumulation_rate: i64, time: Time)->i64 {
  cell.ink_at_last_change + accumulation_rate*(time - cell.last_change)
}

fn more_stable_rate_and_cumulative_error_coefficients (cells: [&CellVarying; 2], accumulation_rates: [i64; 2], transfer: & TransferVarying, start: Time)->(i64, (i64,i64)) {
    
    // We choose the target transfer rate to be the amount that would
    // equalize the two cells in one second.
    // That is, the target transfer rate is (my ink - other ink)/(SECOND*2).
    //
    // Cumulative error = integral_0^t (actual transfer rate - ideal transfer rate)
    // = integral_0^t (actual transfer rate) - integral_0^t (ideal transfer rate)
    // = t*(actual transfer rate) - integral_0^t (ideal transfer rate)
    //
    // ideal transfer rate(t) = (my ink - other ink(t))/(SECOND*2)
    // = (current_difference + t*current_difference_change_rate)/(SECOND*2)
    // (well… Sort of. current_difference_change_rate values can change when other transfers change,
    //   making this formula behave somewhat weirdly)
    // 
    // so Cumulative error =
    // = t*(actual transfer rate) - integral_0^t (ideal transfer rate)
    // = t*(actual transfer rate) - integral_0^t ((current_difference + t*current_difference_change_rate)/(SECOND*2))
    // = t*(actual transfer rate - current_difference/(SECOND*2)) - integral_0^t ((t*current_difference_change_rate)/(SECOND*2))
    // = t*(actual transfer rate - current_difference/(SECOND*2)) - (0.5t^2)*current_difference_change_rate/(SECOND*2)
  
  assert!(cells [0].last_change <= start) ;
  assert!(cells [1].last_change <= start) ;
  assert!(transfer.last_change <= start) ;
  let ink_original = [
    ink_at (cells [0], accumulation_rates [0], start),
    ink_at (cells [1], accumulation_rates [1], start),
  ];
  let original_difference = ink_original [0] - ink_original [1];
  let difference_change_rate = accumulation_rates [0] - accumulation_rates [1];
  
  // the absolute difference between the current rate and a hypothetical more stable rate, tolerating some rounding error
  let rate_change_stability_thing = (difference_change_rate.abs()/8 + 2);
  // we trust the system not to that the ideal rate past more_stable_rate without doing an update, so error can increase no faster than abs(current_rate - more_stable_rate)
  
  (rate_change_stability_thing, (rate_change_stability_thing, 0))
}

fn desired_transfer_change_time (cells: [&CellVarying; 2], accumulation_rates: [i64; 2], transfer: & TransferVarying, start: Time)->Option <Time> {
  let (rate_change_stability_thing, coefficients) = more_stable_rate_and_cumulative_error_coefficients (cells, accumulation_rates, transfer, start);
  
  let original_difference = ink_at (cells [0], accumulation_rates [0], start) - ink_at (cells [1], accumulation_rates [1], start);
  let difference_change_rate = accumulation_rates [0] - accumulation_rates [1];
  
  
    // We need to notice when the target transfer rate goes outside of the range
    // [current transfer rate - rate_change_stability_thing, current transfer rate + rate_change_stability_thing].
    // After a little algebra...
    let (min_difference, max_difference) = (
      (transfer.rate - rate_change_stability_thing)*(2*SECOND),
      (transfer.rate + rate_change_stability_thing)*(2*SECOND)
    );
    if original_difference < min_difference || original_difference > max_difference {
      //printlnerr!("predict wow! {:?}", (start, min_difference,original_difference, max_difference, rate_change_stability_thing));
      return Some(start);
    }
    //printlnerr!("{:?}", (start, min_difference,original_difference, max_difference));
  
  let mut previous_duration = i64::max_value();
  if !(difference_change_rate == 0) {
  
  previous_duration = 1;
  loop {
    let duration = previous_duration*2;
    let later_difference = ink_at (cells [0], accumulation_rates [0], start + duration) - ink_at (cells [1], accumulation_rates [1], start + duration);
    let minimum_difference = if original_difference.signum() != later_difference.signum() {0} else {min(original_difference.abs(), later_difference.abs())};
    let permissible_cumulative_error = 160000000 + (minimum_difference >> 4);
    //printlnerr!("{:?}", (duration, coefficients, permissible_cumulative_error ));
    let error = transfer.accumulated_error + duration*duration*coefficients.1 + duration*coefficients.0;
    //printlnerr!("{:?}", (error ));
    assert!(duration >0 );
    if error.abs() > permissible_cumulative_error {break}
    previous_duration = duration;
  }
  
  }
  
  
    if difference_change_rate > 0 {
      previous_duration = min (previous_duration, (max_difference-original_difference)/difference_change_rate);
    }
    else if difference_change_rate < 0 {
      previous_duration = min (previous_duration, (min_difference-original_difference)/difference_change_rate);
    }
  
  if previous_duration == i64::max_value() {return None;}
  //printlnerr!("{:?}", (cells, accumulation_rates, start, previous_duration));
  Some(start + previous_duration)
}

fn update_cell <A: EventAccessor <Steward = Steward>> (accessor: &A, coordinates: [i32; 2]) {
  let me = get_cell (accessor, coordinates).unwrap();
  let mut my_varying = accessor.query (&me.varying, & GetVarying, QueryOffset::After).unwrap().1;
  let my_accumulation_rate: i64 = get_accumulation_rate (accessor, coordinates);
  my_varying.ink_at_last_change = ink_at (& my_varying, my_accumulation_rate, accessor.now().clone());
  my_varying.last_change = accessor.now().clone();
  modify_simple_timeline (accessor, & me.varying, Some(my_varying));
}

fn update_accumulated_error <A: EventAccessor <Steward = Steward>> (accessor: &A, coordinates: [i32; 2], dimension: usize) {
  if !in_bounds (accessor.globals(), coordinates) {return;}
  let mut neighbor_coordinates = coordinates;
  neighbor_coordinates [dimension] += 1;
  if neighbor_coordinates [dimension] >= accessor.globals().size [dimension] {return;}
  
  let cells = [
    get_cell (accessor, coordinates).unwrap(),
    get_cell (accessor, neighbor_coordinates).unwrap(),
  ];
  let varying = [
    accessor.query (&cells[0].varying, & GetVarying, QueryOffset::After).unwrap().1,
    accessor.query (&cells[1].varying, & GetVarying, QueryOffset::After).unwrap().1,
  ];
  let accumulation_rates = [
    get_accumulation_rate (accessor, coordinates),
    get_accumulation_rate (accessor, neighbor_coordinates),
  ];
  
  let mut transfer = accessor.query (&cells[0].transfers [dimension], & GetVarying, QueryOffset::After).unwrap().1;
  
  let start = max (transfer.last_change, max (varying[0].last_change, varying[1].last_change));
  let (_, coefficients) = more_stable_rate_and_cumulative_error_coefficients ([&varying[0], &varying[1]], accumulation_rates, &transfer, start);
  
  let duration = accessor.now() - start;
  
  transfer.accumulated_error += duration*duration*coefficients.1 + duration*coefficients.0;
  modify_simple_timeline (accessor, & cells[0].transfers [dimension], Some(transfer));
}

fn update_transfer_change_prediction <A: EventAccessor <Steward = Steward>> (accessor: &A, coordinates: [i32; 2], dimension: usize) {
  if !in_bounds (accessor.globals(), coordinates) {return;}
  // This update is only in charge of the transfer to the cell
  // in the positive x and y directions. The other transfers will be handled by
  // the updates for the cells in the negative x and y directions.
  let mut neighbor_coordinates = coordinates;
  neighbor_coordinates [dimension] += 1;
  if neighbor_coordinates [dimension] >= accessor.globals().size [dimension] {return;}
  
  let cells = [
    get_cell (accessor, coordinates).unwrap(),
    get_cell (accessor, neighbor_coordinates).unwrap(),
  ];
  let varying = [
    accessor.query (&cells[0].varying, & GetVarying, QueryOffset::After).unwrap().1,
    accessor.query (&cells[1].varying, & GetVarying, QueryOffset::After).unwrap().1,
  ];
  let accumulation_rates = [
    get_accumulation_rate (accessor, coordinates),
    get_accumulation_rate (accessor, neighbor_coordinates),
  ];
  
  let mut transfer = accessor.query (&cells[0].transfers [dimension], & GetVarying, QueryOffset::After).unwrap().1;
  
  let time = desired_transfer_change_time ([&varying[0], &varying[1]], accumulation_rates, & transfer, *accessor.now());
  
  
  /*
  let (neighbor_last_change, neighbor) = query_cell (accessor, neighbor_coordinates).unwrap();
    
    let last_change = me.transfer_change_times [dimension];
    let current_difference =
      (me.ink_at_last_change + my_accumulation_rate*(last_change - my_last_change)) -
      (neighbor.ink_at_last_change + neighbor_accumulation_rate*(last_change - neighbor_last_change));
    let current_difference_change_rate = my_accumulation_rate - neighbor_accumulation_rate;
    let current_transfer_rate = me.ink_transfers [dimension];
    
    //let permissible_cumulative_error = accessor.globals().max_inaccuracy;
    //let permissible_cumulative_error = 8 + Range::exactly (current_difference.abs()).sqrt().unwrap().max();
    // if we're already fairly stable, require smaller error to avoid drift
    // let permissible_cumulative_error = 8 + Range::exactly (current_difference_change_rate.abs()).sqrt().unwrap().max()<<20;
    let permissible_cumulative_error = 16 + (current_difference.abs() >> 4);
    //
    // set = to max_inaccuracy, and
    // multiply everything by (SECOND*2*2) to reduce rounding:
    // 0 = t^2(current_difference_change_rate) + t*2(actual transfer rate*SECOND*2 - current_difference) +/- max_inaccuracy*SECOND*2*2
    
    let a = current_difference_change_rate;
    let b = current_transfer_rate*(SECOND*4) - current_difference*2;
    let c = permissible_cumulative_error*(SECOND*4);
    // quadratic formula: t = (-b +/- \sqrt(b^2-4ac)) / 2a
    // if it's currently going up, we want the first result for positive inaccuracy and the second for negative inaccuracy, and vice versa
    let time;
    if a == 0 {
      if b == 0 {
        time = None;
      }
      else {
        time = Some ((c/b).abs());
      }
    }
    else {
      let sign_a = if a <0 {-1} else {1};
      if b == 0 {
        let discriminant = Range::exactly (4)*a*c*sign_a;
        time = Some ((discriminant.sqrt().unwrap()*sign_a).min() / (2*a));
      }
      else {
        let sign_b = if b <0 {-1} else {1};
        let direct_discriminant_sqrt = if sign_a == sign_b {None} else {(Range::exactly (b)*b-Range::exactly (4)*a*c*sign_a).sqrt()};
        if let Some(square_root) = direct_discriminant_sqrt {
          time = Some (max(0, (-b - (square_root*sign_a).max()) / (2*a)));
        }
        else {
          let later_discriminant = Range::exactly (b)*b-Range::exactly (-4)*a*c*sign_a;
          time = Some ((-b + (later_discriminant.sqrt().unwrap()*sign_a).max()) / (2*a));
        }
      }
    }
    //printlnerr!( "predict {} {} {} {:?}",a,b,c,time);
    
    // 
    // We need to notice when the target transfer rate goes outside of the range
    // [current transfer rate - maximum inaccuracy, current transfer rate + maximum inaccuracy].
    // After a little algebra...
    /*let (min_difference, target_difference, max_difference) = (
      (current_transfer_rate - accessor.globals().max_inaccuracy)*(2*SECOND),
      (current_transfer_rate                         )*(2*SECOND),
      (current_transfer_rate + accessor.globals().max_inaccuracy)*(2*SECOND)
    );
    if current_difference < min_difference || current_difference > max_difference {
      printlnerr!( "predict wow! {:?} ! {:?}", me, neighbor);
      best = Some((last_change, dimension));
    }
    else if current_difference_change_rate > 0 {
      //printlnerr!( "predict {}/{}",max_difference-current_difference, current_difference_change_rate);
      let time = min (
        me.transfer_change_times [dimension] + SECOND/4,
        last_change + (max_difference-current_difference)/current_difference_change_rate
      );
      if best.map_or (true, | previous | time <previous.0) {best = Some((time, dimension));}
    }
    else if current_difference_change_rate < 0 {
      //printlnerr!( "predict {}/{}",min_difference-current_difference, current_difference_change_rate);
      let time = min (
        me.transfer_change_times [dimension] + SECOND/4,
        last_change + (min_difference-current_difference)/current_difference_change_rate
      );
      if best.map_or (true, | previous | time <previous.0) {best = Some((time, dimension));}
    }*/
    if let Some (time) = time {
      assert!(time >= 0);
      let time = last_change + time;
      if best.map_or (true, | previous | time <previous.0) {best = Some((max (*accessor.now(),time) , dimension));}
    }
  }
  
  let now = accessor.extended_now().clone();
  
  
  // Hack: we only need to modify this because overwriting the cell in order to overwrite it prediction
  // also updates the last change time for the cell as a whole
  me.ink_at_last_change += my_accumulation_rate*(now.base - my_last_change);
  */
  
  if let Some (discarded) = transfer.next_change.take() {accessor.destroy_prediction (&discarded);}
  transfer.next_change = time.map (|time| {
    accessor.create_prediction (
        time,
        DeterministicRandomId::new (&(accessor.id(), coordinates, dimension)),
        TransferChange {coordinates: coordinates, dimension: dimension}
      )
  });
  
  modify_simple_timeline (accessor, & cells[0].transfers [dimension], Some(transfer));
}

/// A utility function used above. Gets the current rate of change of ink in a cell,
/// by summing up the current transfer rates.
/// 
/// Since this function doesn't make predictions, it only needs to require trait Accessor,
/// which is a supertrait of EventAccessor. Thus, it could also be used in Events,
/// and with Snapshots, if needed.
fn get_accumulation_rate <A: Accessor <Steward = Steward >> (accessor: &A, coordinates: [i32; 2])->i64 {
  let mut result = 0;
  let me = get_cell (accessor, coordinates).unwrap();
  for dimension in 0..2 {
    result -= accessor.query (&me.transfers[dimension], & GetVarying, QueryOffset::After).unwrap().1.rate;
    
    let mut neighbor_coordinates = coordinates;
    neighbor_coordinates [dimension] -= 1;
    
    // Adjacent cells might NOT exist (they could be out of bounds).
    // We could also have just done a bounds check on the coordinates, like above.
    if let Some (neighbor) = get_cell (accessor, neighbor_coordinates) {
      result += accessor.query (&neighbor.transfers[dimension], & GetVarying, QueryOffset::After).unwrap().1.rate;
    }
  }
  result
}

fn cell_index (globals: & Globals, coordinates: [i32; 2])->usize {
  (coordinates [0]*globals.size[0] + coordinates [1]) as usize
}
fn in_bounds (globals: & Globals, coordinates: [i32; 2])->bool {
  coordinates [0] >= 0 && coordinates [0] < globals.size [0] &&
  coordinates [1] >= 0 && coordinates [1] < globals.size [1]
}
fn get_cell <A: Accessor <Steward = Steward >> (accessor: &A, coordinates: [i32; 2])->Option <&Cell> {
  if in_bounds (accessor.globals(), coordinates) {
    Some(& accessor.globals().cells[cell_index (accessor.globals(), coordinates)])
  }
  else { None }
}

/// The TransferChange event, as used above.
#[derive (Clone, PartialEq, Eq, Serialize, Deserialize, Debug)]
struct TransferChange {coordinates: [i32; 2], dimension: usize}
impl StewardData for TransferChange {}
impl PersistentlyIdentifiedType for TransferChange {
  const ID: PersistentTypeId = PersistentTypeId(0xd6621e9cfad1c765);
}
impl Event for TransferChange {
  type Steward = Steward;
  type ExecutionData = ();
  fn execute <Accessor: EventAccessor <Steward = Self::Steward>> (&self, accessor: &mut Accessor) {
    //let (my_last_change, mut me) = query_cell (accessor, self.coordinates).expect("cell doesn't exist for TransferChange?");
    let other_dimension = (self.dimension + 1) & 1;
    let mut neighbor_coordinates = self.coordinates;
    neighbor_coordinates [self.dimension] += 1;
    
    // if the other algorithms are right, we only need to update exactly the
    // seven transfers immediately adjacent to the two updated cells.
    let mut walking_coordinates = self.coordinates.clone();
    for offset in -1..2 {
      walking_coordinates[self.dimension] = self.coordinates[self.dimension] + offset;
      update_accumulated_error (accessor, walking_coordinates, self.dimension);
    }
    for offsa in 0..2 {
      for offsb in -1..1 {
        walking_coordinates[self.dimension] = self.coordinates[self.dimension] + offsa;
        walking_coordinates[other_dimension] = self.coordinates[other_dimension] + offsb;
        update_accumulated_error (accessor, walking_coordinates, other_dimension);
      }
    }
    
    update_cell (accessor, self.coordinates);
    update_cell (accessor, neighbor_coordinates);
    
    let me = get_cell (accessor, self.coordinates).unwrap();
    let my_varying = accessor.query (&me.varying, & GetVarying, QueryOffset::After).unwrap().1;
    let neighbor = get_cell (accessor, neighbor_coordinates).unwrap();
    let neighbor_varying = accessor.query (&neighbor.varying, & GetVarying, QueryOffset::After).unwrap().1;
    let mut transfer = accessor.query (&me.transfers [self.dimension], & GetVarying, QueryOffset::After).unwrap().1;
    let mut accumulation_rates = [
      get_accumulation_rate (accessor, self.coordinates),
      get_accumulation_rate (accessor, neighbor_coordinates),
    ];

    // Plain old physics wants to equalize the AMOUNT OF INK between the 2 cells.
    // We want to skew that a little bit, because we also want the simulation to become more stable –
    // – which means that we want to equalize the ACCUMULATION RATE as well.
    let physics_transfer_rate = (my_varying.ink_at_last_change - neighbor_varying.ink_at_last_change) / (2*SECOND);
    accumulation_rates [0] -= physics_transfer_rate - transfer.rate;
    accumulation_rates [1] += physics_transfer_rate - transfer.rate;
    let difference_change_rate = accumulation_rates [0] - accumulation_rates [1];
    let stability_adjustment = difference_change_rate.signum()*(difference_change_rate.abs()/8 + 2);
    transfer.rate = physics_transfer_rate + stability_adjustment/2;
        
    transfer.last_change = accessor.now().clone();
    transfer.accumulated_error = 0;

    modify_simple_timeline (accessor, & me.transfers [self.dimension], Some(transfer));
    
    /*let (neighbor_last_change, mut neighbor) = query_cell (accessor, neighbor_coordinates).expect("neighbor doesn't exist for TransferChange?");
    
    let my_current_ink = me.ink_at_last_change + get_accumulation_rate (accessor, self.coordinates)*(accessor.now() - my_last_change);
    let neighbor_current_ink = neighbor.ink_at_last_change + get_accumulation_rate (accessor, neighbor_coordinates)*(accessor.now() - neighbor_last_change);
    let current_difference = my_current_ink - neighbor_current_ink;
 
    me.ink_at_last_change = my_current_ink;
    neighbor.ink_at_last_change = neighbor_current_ink;
    me.ink_transfers [self.dimension] = current_difference/(2*SECOND);
    me.transfer_change_times [self.dimension] = *accessor.now() ;
    
    modify_cell (accessor, self.coordinates, me) ;
    modify_cell (accessor, neighbor_coordinates, neighbor) ;*/

    // if the other algorithms are right, we only need to update exactly the
    // seven transfers immediately adjacent to the two updated cells.
    let mut walking_coordinates = self.coordinates.clone();
    for offset in -1..2 {
      walking_coordinates[self.dimension] = self.coordinates[self.dimension] + offset;
      update_transfer_change_prediction (accessor, walking_coordinates, self.dimension);
    }
    for offsa in 0..2 {
      for offsb in -1..1 {
        walking_coordinates[self.dimension] = self.coordinates[self.dimension] + offsa;
        walking_coordinates[other_dimension] = self.coordinates[other_dimension] + offsb;
        update_transfer_change_prediction (accessor, walking_coordinates, other_dimension);
      }
    }
    
    // TODO: invalidation
  }

  fn undo <Accessor: FutureCleanupAccessor <Steward = Self::Steward>> (&self, accessor: &mut Accessor, _: ()) {
    unimplemented!()
  }
}


/// In order to get the simulation going, we need two more events:
/// One to be the FiatEvent that initializes the cells to an initial empty state,
/// and one to be the FiatEvent that adds or removes ink in some way at real-time.
///
/// For these simple events, we lazily use the pseudo-closure syntax.
#[derive (Clone, PartialEq, Eq, Serialize, Deserialize, Debug)]
struct Initialize {}
impl StewardData for Initialize {}
impl PersistentlyIdentifiedType for Initialize {
  const ID: PersistentTypeId = PersistentTypeId(0xf0d2d9134cfe9b49);
}
impl Event for Initialize {
  type Steward = Steward;
  type ExecutionData = ();
  fn execute <Accessor: EventAccessor <Steward = Self::Steward>> (&self, accessor: &mut Accessor) {
    let globals = accessor.globals();
    for x in 0..globals.size [0] {
      for y in 0..globals.size [1] {
        let cell = get_cell (accessor, [x,y]).unwrap();
        modify_simple_timeline (accessor, & cell.varying, Some(CellVarying {
          last_change: 0,
          ink_at_last_change: 0,
        }));
        for dimension in 0..2 {
          modify_simple_timeline (accessor, & cell.transfers [dimension], Some(TransferVarying {
            rate: 0,
            last_change: 0,
            accumulated_error: 0,
            next_change: None,
          }));
        }
      }
    }
  }
  fn undo <Accessor: FutureCleanupAccessor <Steward = Self::Steward>> (&self, accessor: &mut Accessor, _: ()) {
    unimplemented!()
  }
}
#[derive (Clone, PartialEq, Eq, Serialize, Deserialize, Debug)]
struct AddInk {coordinates: [i32; 2], amount: i64}
impl StewardData for AddInk {}
impl PersistentlyIdentifiedType for AddInk {
  const ID: PersistentTypeId = PersistentTypeId(0x3e6d029c3da8b9a2);
}
impl Event for AddInk {
  type Steward = Steward;
  type ExecutionData = ();
  fn execute <Accessor: EventAccessor <Steward = Self::Steward>> (&self, accessor: &mut Accessor) {
    /*let (my_last_change, mut me) = query_cell (accessor, self.coordinates).expect("cell doesn't exist for AddInk?");
    let my_current_ink = me.ink_at_last_change + get_accumulation_rate (accessor, self.coordinates)*(accessor.now() - my_last_change);
    me.ink_at_last_change = my_current_ink + self.amount;
    modify_cell (accessor, self.coordinates, me) ;*/
    for offsx in -1..1 {
      for offsy in -1..1 {
        let coordinates = [self.coordinates [0] + offsx, self.coordinates [1] + offsy];
        update_accumulated_error (accessor, coordinates, 0) ;
        update_accumulated_error (accessor, coordinates, 1) ;
      }
    }
    update_cell (accessor, self.coordinates);
    let me = get_cell (accessor, self.coordinates).unwrap();
    let mut my_varying = accessor.query (&me.varying, & GetVarying, QueryOffset::After).unwrap().1;
    my_varying.ink_at_last_change += self.amount;
    modify_simple_timeline (accessor, & me.varying, Some(my_varying));
    // TODO: some of the ones at the corners don't need to be updated
    for offsx in -1..1 {
      for offsy in -1..1 {
        let coordinates = [self.coordinates [0] + offsx, self.coordinates [1] + offsy];
        update_transfer_change_prediction (accessor, coordinates, 0) ;
        update_transfer_change_prediction (accessor, coordinates, 1) ;
      }
    }
  }
  fn undo <Accessor: FutureCleanupAccessor <Steward = Self::Steward>> (&self, accessor: &mut Accessor, _: ()) {
    unimplemented!()
  }
}

fn make_globals()-> Globals {
  let mut cells = Vec::new();
  for index in 0..60*60 {
    cells.push (Cell{
      varying: DataTimelineCell::new (SimpleTimeline::new ()),
      transfers: [
        DataTimelineCell::new (SimpleTimeline::new ()),
        DataTimelineCell::new (SimpleTimeline::new ()),
      ],
    });
  }
  Globals {
    size: [60, 60],
    max_inaccuracy: 1 << 30,
    cells: cells,
  }
}

/// Finally, define the Basics type.
#[derive (Copy, Clone, Hash, PartialEq, Eq, PartialOrd, Ord, Serialize, Deserialize, Debug, Default)]
struct Basics {}
impl BasicsTrait for Basics {
  type Time = Time;
  type Globals = Globals;
  type Types = (ListedType <TransferChange>, ListedType <Initialize>, ListedType <AddInk>);
}


// User interface details.
use docopt::Docopt;

const USAGE: &'static str = "
Simple diffusion, a TimeSteward usage example.

Usage:
  simple_diffusion
  simple_diffusion (-l | --listen) <host> <port>
  simple_diffusion (-c | --connect) <host> <port>
  
Options:
  -l, --listen   Start a synchronized simulation by listening for TCP connections.
  -c, --connect  Start a synchronized simulation by making a TCP connection.
";

#[derive(Debug, Deserialize)]
struct Args {
  flag_listen: bool,
  flag_connect: bool,
    
  arg_host: Option <String>,
  arg_port: Option <u16>,
}

use std::time::{Instant, Duration};
use glium::{DisplayBuild, Surface};

#[derive(Copy, Clone)]
struct Vertex {
  location: [f32; 2],
  ink: f32,
}
implement_vertex!(Vertex, location, ink);

use std::net::{TcpListener, TcpStream};
use std::io::{BufReader, BufWriter};

fn main() {
  let globals = make_globals();

  // For some reason, docopt checking the arguments caused build_glium() to fail in emscripten.
  /*if !cfg!(target_os = "emscripten") {
    let arguments: Args = Docopt::new(USAGE)
                            .and_then(|d| d.deserialize())
                            .unwrap_or_else(|e| e.exit());
    
    if arguments.flag_listen {
      let listener = TcpListener::bind ((arguments.arg_host.as_ref().map_or("localhost", | string | string as & str), arguments.arg_port.unwrap())).unwrap();
      let stream = listener.accept().unwrap().0;
      let mut steward: simply_synchronized::Steward<Basics, DefaultSteward <Basics>> = simply_synchronized::Steward::new(DeterministicRandomId::new (& 0u32), 0, SECOND>>3, constants, BufReader::new (stream.try_clone().unwrap()), BufWriter::new (stream));
      steward.insert_fiat_event(0, DeterministicRandomId::new(&0), Initialize::new()).unwrap();
      run (steward, |a,b| (a.settle_before (b)));
      return;
    }
    else if arguments.flag_connect {
      let stream = TcpStream::connect ((arguments.arg_host.as_ref().map_or("localhost", | string | string as & str), arguments.arg_port.unwrap())).unwrap();
      let steward: simply_synchronized::Steward<Basics, DefaultSteward <Basics>> = simply_synchronized::Steward::new(DeterministicRandomId::new (& 1u32), 0, SECOND>>3, constants, BufReader::new (stream.try_clone().unwrap()), BufWriter::new (stream));
      run (steward, |a,b| (a.settle_before (b)));
      return;
    }
  }*/
  {
    let mut steward: Steward = Steward::from_globals(globals);
    steward.insert_fiat_event(0, DeterministicRandomId::new(&0), Initialize{}).unwrap();
    run (steward, |_,_|());
  }
}


fn run <F: Fn (&mut Steward, Time)>(mut stew: Steward, settle:F) {


  let vertex_shader_source = r#"
#version 100
attribute lowp vec2 location;
attribute lowp float ink;
varying lowp float ink_transfer;

void main() {
gl_Position = vec4 (location, 0.0, 1.0);
ink_transfer = ink;
}

"#;

  let fragment_shader_source = r#"
#version 100
varying lowp float ink_transfer;

void main() {
gl_FragColor = vec4 (vec3(0.5 - ink_transfer/100000000000.0), 1.0);
}

"#;

  let mut event_index = 0u64;
  let mut mouse_coordinates = [0,0];

  let display = glium::glutin::WindowBuilder::new()
    .with_dimensions(600, 600)
    .build_glium()
    .expect("failed to create window");
  let program =
    glium::Program::from_source(&display, vertex_shader_source, fragment_shader_source, None)
      .expect("glium program generation failed");
  let parameters = glium::DrawParameters {
    blend: glium::draw_parameters::Blend::alpha_blending(),
    ..Default::default()
  };
  let indices = glium::index::NoIndices(glium::index::PrimitiveType::TrianglesList);

  // take care of the expensive predictions before starting the timer
  stew.snapshot_before(&1);
  let start = Instant::now();
  let mut previous_time = 0;
  let mut display_state = 0;

  let frame = || {
    let frame_begin = Instant::now();
    let time = min (previous_time + SECOND/10, 1 + ((start.elapsed().as_secs() as i64 * 1000000000i64) +
                          start.elapsed().subsec_nanos() as i64) *
                         SECOND / 1000000000i64);
    previous_time = time;
    
    let accessor = stew.snapshot_before(& time)
      .expect("steward failed to provide snapshot");
    stew.forget_before (& time);
    settle (&mut stew, time);
    let globals = accessor.globals();
    
    for ev in display.poll_events() {
      match ev {
        glium::glutin::Event::Closed => return true,
        glium::glutin::Event::MouseMoved (x,y) => {
          mouse_coordinates [0] = (x as i32) * 60 / display.get_window().unwrap().get_inner_size_pixels().unwrap().0 as i32;
          mouse_coordinates [1] = (display.get_window().unwrap().get_inner_size_pixels().unwrap().1 as i32-(y as i32)) * 60 / display.get_window().unwrap().get_inner_size_pixels().unwrap().1 as i32;
        },
        glium::glutin::Event::MouseInput (_,_) => {
          if in_bounds (globals, mouse_coordinates) {
            event_index += 1;
            stew.insert_fiat_event (time, DeterministicRandomId::new (& event_index), AddInk {
              coordinates: [mouse_coordinates [0], mouse_coordinates [1]],
              amount: (DeterministicRandomId::new (& event_index).data() [0] & ((1u64<<40)-1)) as i64 - (1<<38)
            }).unwrap();
          }
        },
        glium::glutin::Event::KeyboardInput (state,_,_) => {
          if state == glium::glutin::ElementState::Pressed {
            display_state = (display_state + 1) % 3;
          }
        }
        _ => (),
      }
    }
    while let Some ((x,y)) = emscripten_compatibility::pop_click() {
      // TODO duplicate code
      mouse_coordinates [0] = (x*60.0) as i32;
      mouse_coordinates [1] = ((1.0-y)*60.0) as i32;
      if in_bounds (globals, mouse_coordinates) {
        event_index += 1;
        stew.insert_fiat_event (time, DeterministicRandomId::new (& event_index), AddInk {
            coordinates: [mouse_coordinates [0], mouse_coordinates [1]],
            amount: (DeterministicRandomId::new (& event_index).data() [0] & ((1u64<<40)-1)) as i64 - (1<<39)
          }).unwrap();
      }
      else {
        display_state = (display_state + 1) % 3;
      }
    }

    let mut target = display.draw();
    target.clear_color(1.0, 1.0, 1.0, 1.0);
    let mut vertices = Vec::<Vertex>::new();
    
    for x in 0.. globals.size [0] {
      for y in 0.. globals.size [1] {
        let me = get_cell (& accessor, [x,y]).unwrap();
        let my_varying = accessor.query (&me.varying, & GetVarying, QueryOffset::After).unwrap().1;
        let my_current_ink = match display_state {
          0 => ink_at (&my_varying, get_accumulation_rate (& accessor, [x,y]), *accessor.now()) as f32,
          1 => (get_accumulation_rate (&accessor, [x,y]) * SECOND) as f32,
          _ => ((accessor.now() - my_varying.last_change)*50000000000 / SECOND) as f32,
        };
        
        vertices.extend(&[Vertex {
                            location: [((x) as f32)/30.0 -1.0,((y) as f32)/30.0 -1.0],
                            ink: my_current_ink,
                          },
                          Vertex {
                            location: [((x + 1) as f32)/30.0 -1.0,((y) as f32)/30.0 -1.0],
                            ink: my_current_ink,
                          },
                          Vertex {
                            location: [((x) as f32)/30.0 -1.0,((y + 1) as f32)/30.0 -1.0],
                            ink: my_current_ink,
                          },
                          Vertex {
                            location: [((x + 1) as f32)/30.0 -1.0,((y + 1) as f32)/30.0 -1.0],
                            ink: my_current_ink,
                          },
                          Vertex {
                            location: [((x + 1) as f32)/30.0 -1.0,((y) as f32)/30.0 -1.0],
                            ink: my_current_ink,
                          },
                          Vertex {
                            location: [((x) as f32)/30.0 -1.0,((y + 1) as f32)/30.0 -1.0],
                            ink: my_current_ink,
                          }]);

      }
    }
    
    target.draw(&glium::VertexBuffer::new(&display, &vertices)
                .expect("failed to generate glium Vertex buffer"),
              &indices,
              &program,
              &glium::uniforms::EmptyUniforms,
              &parameters)
        .expect("failed target.draw");

    target.finish().expect("failed to finish drawing");
      
    /*while frame_begin.elapsed() < Duration::from_millis (10) && stew.updated_until_before().map_or (false, | limitation | limitation < time + SECOND) {
        for _ in 0..8 {stew.step();}
    }*/
    false
  };
  
  emscripten_compatibility::main_loop(frame);
}
