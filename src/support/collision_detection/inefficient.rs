use std::collections::BTreeSet;
use super::{Nearness, Basics};
use {RowId, ColumnId, Column, Mutator};
use std::marker::PhantomData;

struct Detector<B: Basics> {
  _marker: PhantomData<B>,
}
impl<B: Basics> Column for Detector<B> {
  type FieldType = BTreeSet <RowId>;
  fn column_id() -> ColumnId {
    ColumnId(B::nearness_column_id().0 ^ 0xdaa7e18546759b65)
  }
}


pub fn insert<B: Basics, M: Mutator<Basics = B::StewardBasics>>(mutator: &mut M,
                                                                who: RowId,
                                                                me: B::DetectorId) {
  let my_row = RowId::new(&me);
  let mut members = mutator.get::<Detector<B>>(my_row).cloned().unwrap_or(BTreeSet::new());
  for member in members.iter() {
    let (id, contents) = Nearness::new(who, member.clone(), me);
    mutator.set::<Nearness<B>>(id, Some(contents));
  }
  members.insert(who);
  mutator.set::<Detector<B>>(my_row, Some(members));
}

pub fn remove<B: Basics, M: Mutator<Basics = B::StewardBasics>>(mutator: &mut M,
                                                                who: RowId,
                                                                me: B::DetectorId) {
  let my_row = RowId::new(&me);
  let mut members = mutator.get::<Detector<B>>(my_row)
    .expect("erasing an element when there are no elements")
    .clone();
  members.remove(&who);
  for member in members.iter() {
    let (id, _) = Nearness::<B>::new(who, member.clone(), me);
    mutator.set::<Nearness<B>>(id, None);
  }
  mutator.set::<Detector<B>>(my_row,
                             if members.is_empty() {
                               None
                             } else {
                               Some(members)
                             });
}
