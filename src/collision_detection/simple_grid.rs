use std::collections::HashSet;
use super::{Nearness};
use {RowId, ColumnId, Column, Mutator};
use std::marker::PhantomData;



pub trait Basics: super::Basics {
  type SpaceCoordinate;
  fn grid_size()->Self::SpaceCoordinate;
  fn get_center()->
}



struct Cell <B: Basics> {
  _marker: PhantomData<B>,
}
impl<B: Basics> Column for Cell <B> {
  type FieldType = HashSet <RowId>;
  fn column_id() -> ColumnId {
    ColumnId(B::nearness_column_id().0 ^ 0x030fc8868af34f2c)
  }
}

fn cell_row (x: i64, y: i64, me: B::DetectorId) {RowId::new (& [x, y, me])}
fn get_center<B: Basics, M: Mutator<B::StewardBasics>>(mutator: &mut M,
                                                       who: RowId,
                                                       me: B::DetectorId) {
  B::get_center(mutator, who, me)
}
pub fn insert<B: Basics, M: Mutator<B::StewardBasics>>(mutator: &mut M,
                                                       who: RowId,
                                                       me: B::DetectorId) {
  let (x, y) = get_center:: <B, M> (mutator, who, me);
  for next in x - 1..x + 2 {for what in y - 1..y + 2 {
    let row = cell_row (next, what, me);
    let mut members = mutator.get::<Cell <B>>(row).cloned().unwrap_or(HashSet::new());
    for member in members.iter() {
      let (id, contents) = Nearness::new(who, member.clone(), me);
      mutator.set::<Nearness<B>>(id, Some(contents));
    }
    if next == x && what == y {
      members.insert(who);
      mutator.set::<Cell <B>>(row, Some(members));
    }
  }}
}

pub fn erase<B: Basics, M: Mutator<B::StewardBasics>>(mutator: &mut M,
                                                      who: RowId,
                                                      me: B::DetectorId) {
  let (x, y) = get_center:: <B, M> (mutator, who, me);
  for next in x - 1..x + 2 {for what in y - 1..y + 2 {
    let row = cell_row (next, what, me);
    let mut members = mutator.get::<Cell <B>>(row).cloned().unwrap_or(HashSet::new());
    if next == x && what == y {
      members.remove (&who);
      mutator.set::<Cell <B>>(row, if members.is_empty() {
                               None
                             } else {
                               Some(members)
                             });
    }
    for member in members.iter() {
      let (id,_) = Nearness::new(who, member.clone(), me);
      mutator.set::<Nearness<B>>(id, None);
    }
  }}
}

macro_rules! push_predictors {
($module: ident, $predictors: ident) => {{
$predictors.push ($module::Predictor {
predictor_id: PredictorId (),
column_id:,
function: Rc::new (| accessor, id | {
let center = B::get_center (accessor, id: me);
if let Some (time) = B::when_escapes (accessor, id, center) {
accessor.predict_at_time (& time, Rc::new (move | mutator | {

});
}
}),
});
}}
}
