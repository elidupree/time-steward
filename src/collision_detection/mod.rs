use ::{RowId, ColumnId, Column, Mutator};
use std::any::Any;
use std::hash::Hash;

pub mod inefficient;

pub trait Basics: 'static {
  type StewardBasics: ::Basics;
  type DetectorId: Copy + Any + Hash;//=()
  fn nearness_column_id()->ColumnId;
}

pub struct Nearness <B: Basics> {
  detector_id: B::DetectorId,
  ids: [RowId; 2],

}

impl<B: Basics> Nearness <B> {
  pub fn new (first: RowId, second: RowId, detector_id: B::DetectorId)->(RowId, Nearness <B>) {
let ids = if first <second {[first, second]} else {[second, first]};
(RowId::new (& (ids, detector_id)), Nearness {ids: ids, detector_id: detector_id})
}
  pub fn get_ids <M: Mutator <B::StewardBasics>>  (mutator: &mut M, my_id: RowId)->([RowId; 2], B::DetectorId) {
    let me = mutator.get:: <Nearness <B>> (my_id).unwrap();
    (me.ids, me.detector_id)
  }
}
impl<B: Basics> Column for Nearness <B> {
  type FieldType = Self;
  fn column_id()->ColumnId {B::nearness_column_id()}
}
