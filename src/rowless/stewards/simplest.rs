use ::{Basics, TimeSteward, Accessor};


pub struct Mutator<'a, B: Basics> {
  generic: common::GenericMutator<B>,
  steward: &'a mut StewardImpl<B>,
}
pub struct PredictorAccessor<'a, B: Basics> {
  generic: common::GenericPredictorAccessor<B, DynamicEvent<B>>,
  steward: &'a StewardImpl<B>,
}

struct DataTimelineHandle <B: Basics, Timeline: DataTimeline> {
  data: TypedArc::<B::DataTimelinesList, DataTimelineId, T>>;
}

struct PredictorHandle <B: Basics> {
  data: DynamicArc::<B::PredictorsList, ()>>;
}
struct EventHandle <B: Basics> {
  data: DynamicArc <B::EventsList, ()>;
}

impl <B: Basics> ::Mutator for Mutator {
  fn create <T: DataTimeline> (&mut self, constants: D::Constants)->DataTimelineHandle <B, T> {
    let id = self.next_id();
    let result = DataTimelineHandle {
      data: TypedArc::<StewardsTimesDataTimelines, DataTimelineId, Times <<Self as Accessor>::Steward, T>>::new (id, T::from_constants (constants))
    }
    //self.existent_timelines.insert (result.data.clone().erase_type());
  }
  fn do_operation <T: DataTimeline> (&mut self, timeline: &DataTimelineHandle <B, T>, operation: T::Operation) {
    // stewards::simplest always uhh...  reverts all later changes before uh...?
    let result = timeline.differentiated_mut().insert_operation (self.extended_now(), operation, snapshots????);
    for predictor in result.created_predictors {
      self.steward.existent_predictors.insert(predictor);
    }
    for predictor in result.destroyed_predictors {
      self.steward.existent_predictors.remove(predictor);
    }
  }
}

struct Steward <B: Basics> {
  constants: B::Constants,
  invalid_before: ValidSince <B::Time>,
  last_event: Option <ExtendedTime <B>>,
  upcoming_fiat_events: BTreeMap<ExtendedTime <B>, EventHandle<B>>,
  existent_predictors: HashMap<PredictorHandle <B>>,
  snapshots: SnapshotTree <B>,
}


impl<B: Basics> Steward<B> {
  fn next_event(&self) -> Option<(ExtendedTime<B>, EventHandle<B>)> {
    let first_fiat_event_iter = self.state
      .upcoming_fiat_events
      .iter()
      .map(|ev| (ev.0.clone(), ev.1.clone()));
    let predicted_events_iter = self.existent_predictors.iter().map(|predictor| {
      let generic;
      {
        let mut pa = PredictorAccessor {
          generic: common::GenericPredictorAccessor::new(),
          steward: self,
        };
        (predictor.function)(&mut pa, field_id.row_id);
        generic = pa.generic;
      }
      generic.soonest_prediction.into_inner().map(|(event_base_time, event)| {
        let extended =
          common::next_extended_time_of_predicted_event(predictor.predictor_id,
                                                            field_id.row_id,
                                                            event_base_time,
                                                            &self.last_event
                                                              .as_ref()
                                                              .expect("how can we be calling a \
                                                                       predictor when there are \
                                                                       no fields yet?"))
                .expect("this should only fail if the time was in the past, a case that was \
                         already ruled out");
            (extended, event)
          })
        })
    });
    let events_iter = first_fiat_event_iter.chain(predicted_events_iter);
    events_iter.min_by_key(|ev| ev.0.clone())
  }

  fn execute_event(&mut self, event_time: ExtendedTime<B>, event: DynamicEvent<B>) {
    event(&mut Mutator {
      generic: common::GenericMutator::new(event_time.clone()),
      steward: &mut *self,
    });
    // if it was a fiat event, clean it up:
    self.upcoming_fiat_events.remove(&event_time);
    self.last_event = Some(event_time);
  }
}

impl<B: Basics> TimeSteward for Steward<B> {
  type Basics = B;
  type Snapshot = Snapshot<B>;

  fn valid_since(&self) -> ValidSince<B::Time> {
    max(self.state.invalid_before.clone(),
        match self.state.last_event {
          None => ValidSince::TheBeginning,
          Some(ref time) => ValidSince::After(time.base.clone()),
        })
  }
  
  fn insert_fiat_event<E: ::Event<Basics = B>>(&mut self,
                                               time: B::Time,
                                               id: DeterministicRandomId,
                                               event: E)
                                               -> Result<(), FiatEventOperationError> {
    if self.valid_since() > *time {
      return Err(FiatEventOperationError::InvalidTime);
    }
    match self.state.fiat_events.insert(common::extended_time_of_fiat_event(time, id),
                                        StewardRc::new(DynamicEventFn::new(event))) {
      None => Ok(()),
      Some(_) => Err(FiatEventOperationError::InvalidInput),
    }
  }

  fn remove_fiat_event(&mut self,
                       time: &B::Time,
                       id: DeterministicRandomId)
                       -> Result<(), FiatEventOperationError> {
    if self.valid_since() > *time {
      return Err(FiatEventOperationError::InvalidTime);
    }
    match self.state.fiat_events.remove(&common::extended_time_of_fiat_event(time.clone(), id)) {
      None => Err(FiatEventOperationError::InvalidInput),
      Some(_) => Ok(()),
    }
  }
  
  fn snapshot_before (&mut self, time: & Basics::Time)->Option <Self::Snapshot> {
    if self.valid_since() > *time { return None; }
    while let Some (updated) = self.updated_until_before () {
      if updated >= time {break;}
      self.step();
    }
    Some (snapshots.make_new (time))
  }
  
  fn forget_before (&mut self, time: & Basics::Time) {}
}

impl<B: Basics> ::TimeStewardFromConstants for Steward<B> {
  fn from_constants(constants: B::Constants) -> Self {
    Steward {
      constants: constants,
      invalid_before: ValidSince::TheBeginning,
      last_event: None,
      upcoming_fiat_events: BTreeMap::new(),
      snapshots: SnapshotTree::new(),
    }
  }
}
impl<B: Basics> ::TimeStewardFromSnapshot for Steward<B> {
  fn from_snapshot<'a, S: ::Snapshot<Basics = B>>(snapshot: &'a S) -> Self
    where &'a S: IntoIterator<Item = ::SnapshotEntry<'a, B>>
  {
    let mut result = Steward {
      constants: ????,
      invalid_before: ValidSince::Before(snapshot.now().clone()),
      last_event: None,
      upcoming_fiat_events: BTreeMap::new(),
      snapshots: SnapshotTree::new(),
    };
    ????
    result
  }
}

impl<B: Basics> ::IncrementalTimeSteward for Steward<B> {
  fn step(&mut self) {
    if let Some(ev) = self.next_event() {
      let (event_time, event) = ev;
      self.execute_event(event_time, event);
    }
  }
  fn updated_until_before(&self) -> Option<B::Time> {
    self.next_event().map(|(time, _)| time.base)
  }
}
impl<B: Basics> ::CanonicalTimeSteward for Steward<B> {}
