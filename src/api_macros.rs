
#[macro_export]
macro_rules! time_steward_predictor {
  ([$($privacy:tt)*] struct $Struct: ident
    <$([$Parameter: ident $($bounds:tt)*]),*>,
    $B: ty, $predictor_id: expr, watching $Column: ty,
    | $accessor_name: ident, $row_name: ident | $contents: expr) => {
    
    #[derive (Clone, PartialEq, Eq, Serialize, Deserialize, Debug)]
    $($privacy)* struct $Struct<$($Parameter $($bounds)*),*>(::std::marker::PhantomData <($($Parameter),*)>);
    impl<$($Parameter $($bounds)*),*> $crate::Predictor for $Struct <$($Parameter),*> {
      type Basics = $B;
      fn call <P: $crate::PredictorAccessor <Basics = $B>> ($accessor_name: &mut P, $row_name: RowId) {
        $contents
      }
      #[inline (always)]
      fn predictor_id()->$crate::PredictorId {$predictor_id}
      type WatchedColumn = $Column;
    }
  };
  ([$($privacy:tt)*] struct $Struct: ident
    <$([$Parameter: ident $($bounds:tt)*]),*>,
    $B: ty, $predictor_id: expr, watching $Column: ty,
    fn $generic_function: ident) => {
    time_steward_predictor! ([$($privacy)*] struct $Struct <$([$Parameter $($bounds)*]),*>, $B, $predictor_id, watching $Column, | accessor, id | $generic_function::<$($Parameter),*>(accessor, id));
  };
  ([$($privacy:tt)*] struct $Struct: ident,
    $B: ty, $predictor_id: expr, watching $Column: ty,
    $($rest:tt)*) => {
    time_steward_predictor! ([$($privacy)*] struct $Struct <>, $B, $predictor_id, watching $Column, $($rest)*);
  };
  (pub struct $($rest:tt)*) => {
    time_steward_predictor! ([pub] struct $($rest)*);
  };
  (struct $($rest:tt)*) => {
    time_steward_predictor! ([] struct $($rest)*);
  };
}

#[macro_export]
macro_rules! time_steward_event {
  ([$($privacy:tt)*] struct $Struct: ident <$([$Parameter: ident $($bounds:tt)*]),*>{$($field_name: ident: $field_type: ty),*}, $B: ty, $event_id: expr, | &$self_name: ident, $mutator_name: ident | $contents: expr) => {
    #[derive (Clone, PartialEq, Eq, Serialize, Deserialize, Debug)]
    $($privacy)* struct $Struct<$($Parameter $($bounds)*),*>{
      // Hacky workaround for https://github.com/rust-lang/rust/issues/41617 (see https://github.com/serde-rs/serde/issues/943)
      $(#[serde(deserialize_with = "::serde::Deserialize::deserialize")] $field_name: $field_type),*
    }
    impl<$($Parameter $($bounds)*),*> $crate::Event for $Struct <$($Parameter),*> {
      type Basics = $B;
      fn call <M: $crate::Mutator <Basics = $B>> (&$self_name, $mutator_name: &mut M) {
        $contents
      }
      #[inline (always)]
      fn event_id()->$crate::EventId {$event_id}
    }
    impl<$($Parameter $($bounds)*),*> $Struct <$($Parameter),*> {
      #[allow (dead_code)]
      $($privacy)* fn new($($field_name: $field_type),*)->Self {$Struct {$($field_name: $field_name),*}}
    }
  };
  ([$($privacy:tt)*] struct $Struct: ident <$([$Parameter: ident $($bounds:tt)*]),*>{$($field_name: ident: $field_type: ty),*}, $B: ty, $event_id: expr, fn $generic_function: ident) => {
    time_steward_event! ([$($privacy)*] struct $Struct <$([$Parameter $($bounds)*]),*>{$($field_name: $field_type),*}, $B, $event_id, | &self, mutator | $generic_function::<$($Parameter),*>(mutator, self));
  };
  ([$($privacy:tt)*] struct $Struct: ident{$($field_name: ident: $field_type: ty),*}, $B: ty, $event_id: expr, $($rest:tt)*) => {
    time_steward_event! ([$($privacy)*] struct $Struct <>{$($field_name: $field_type),*}, $B, $event_id, $($rest)*);
  };
  (pub struct $($rest:tt)*) => {
    time_steward_event! ([pub] struct $($rest)*);
  };
  (struct $($rest:tt)*) => {
    time_steward_event! ([] struct $($rest)*);
  };
}


#[macro_export]
macro_rules! time_steward_basics {
  ([$($privacy:tt)*] struct $Basics: ident {$($contents:tt)*}) => {
    #[derive (Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize, Debug, Default)]
    $($privacy)* struct $Basics;
    impl $crate::Basics for $Basics {
      $($contents)*
    }
  };
  (pub struct $($rest:tt)*) => {
    time_steward_basics! ([pub] struct $($rest)*);
  };
  (struct $($rest:tt)*) => {
    time_steward_basics! ([] struct $($rest)*);
  };
}
