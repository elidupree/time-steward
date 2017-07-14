// pub trait SimpleSynchronizableTimeSteward<B: Basics>: TimeSteward<B>
// where B::Time: Sub + Mul<i64, Output = B::Time> + Div<B::Time, Output = i64> {
// fn begin_checks (&mut self, start: B::Time, stride: B::Time);
// fn checksum(&self, which: i64)->u64;
// fn debug_dump(&self, which: i64) ->BTreeMap<ExtendedTime <B>, u64>;
// fn event_details (&self, time: & ExtendedTime <B>)->String;
// }
//

use std::collections::BTreeMap;
use std::io::{Read, Write};
use std::any::Any;
use std::ops::{Sub, Mul, Div};
use {ExtendedTime, Basics, TimeSteward, SimpleSynchronizableTimeSteward, DeterministicRandomId,
     EventId, Event, FiatEventOperationError, ValidSince};
use std::sync::mpsc::{channel, Sender, Receiver};
use bincode;
use serde::Deserialize;


// serde(deserialize_with is a hacky workaround for https://github.com/rust-lang/rust/issues/41617 (see https://github.com/serde-rs/serde/issues/943)
#[derive (Clone, Serialize, Deserialize)]
enum Message<B: Basics> {
  InsertFiatEvent(#[serde(deserialize_with = "Deserialize::deserialize")] B::Time, #[serde(deserialize_with = "Deserialize::deserialize")] DeterministicRandomId, #[serde(deserialize_with = "Deserialize::deserialize")] EventId, #[serde(deserialize_with = "Deserialize::deserialize")] Vec<u8>),
  RemoveFiatEvent(#[serde(deserialize_with = "Deserialize::deserialize")] B::Time, #[serde(deserialize_with = "Deserialize::deserialize")] DeterministicRandomId),
  Settled(#[serde(deserialize_with = "Deserialize::deserialize")] i64),
  Checksum(#[serde(deserialize_with = "Deserialize::deserialize")] i64, #[serde(deserialize_with = "Deserialize::deserialize")] u64),
  DebugDump(#[serde(deserialize_with = "Deserialize::deserialize")] BTreeMap<ExtendedTime<B>, u64>),
  EventDetails(#[serde(deserialize_with = "Deserialize::deserialize")] String),
  Finished(#[serde(deserialize_with = "Deserialize::deserialize")] u32),
}

pub struct Steward<B: Basics, Steward0: SimpleSynchronizableTimeSteward<Basics = B>> {
  steward: Steward0,
  id: DeterministicRandomId,
  sender: Sender<Message<B>>,
  receiver: Receiver<Message<B>>,
  start: B::Time,
  stride: B::Time,
  valid_since: ValidSince <B::Time>,
  settled_through: i64,
  other_settled_through: i64,
  checksums: Vec<u64>,
  dump: Option<BTreeMap<ExtendedTime<B>, u64>>,
  finishes_received: u32,
}

time_steward_dynamic_fn! (fn do_fiat_event_message <B: Basics, [Steward0: Any + SimpleSynchronizableTimeSteward<Basics = B>]> (event_id: EventId of <E: Event <Basics = B>>, steward: &mut Steward <B, Steward0>, time: B::Time, qualified_id: DeterministicRandomId, data: Vec <u8>)->() {
  steward.steward.insert_fiat_event (time, qualified_id, bincode::serde::deserialize:: <E> (data.as_slice()).unwrap()).unwrap();
});

impl <B: Basics, Steward0: SimpleSynchronizableTimeSteward<Basics = B>> Steward <B, Steward0>
where << Steward0 as TimeSteward>::Basics as Basics>::Time: Sub <Output = << Steward0 as TimeSteward>::Basics as Basics>::Time> + Mul<i64, Output = << Steward0 as TimeSteward>::Basics as Basics>::Time> + Div<<<Steward0 as TimeSteward>::Basics as Basics>::Time, Output = i64>
{
  pub fn new <Reader: Any + Read + Send, Writer: Any + Write + Send> (id: DeterministicRandomId, start: B::Time, stride: B::Time, constants: B::Constants, mut reader: Reader, mut writer: Writer)->Self {
    let (send_away, receive_away) = channel();
    let (send_back, receive_back) = channel();
    ::std::thread::spawn (move | | {
      loop {
        let message: Message <B> = match bincode::serde::deserialize_from (&mut reader, bincode::SizeLimit::Infinite) {
          Err (_) => return,
          Ok (message) => message,
        };
        send_back.send (message.clone()).unwrap();
        if let Message::Finished (9) = message {return;}
      }
    });
    ::std::thread::spawn (move | | {
      loop {
        match receive_away.recv() {
          Err (_) => return,
          Ok (message) => {match bincode::serde::serialize_into (&mut writer, &message, bincode::SizeLimit::Infinite) {
            Err (_) => return,
            Ok (_) => {writer.flush().unwrap()},
          }
          if let Message::Finished (9) = message {return;}
          }
        };
      }
    });
    let mut steward: Steward0 = Steward0::from_constants (constants);
    steward.begin_checks (start.clone(), stride.clone());
    Steward {
      steward: steward,
      id: id,
      sender: send_away,
      receiver: receive_back,
      start: start, stride: stride,
      valid_since: ValidSince::TheBeginning,
      settled_through: -1, other_settled_through: -1,
      checksums: Vec::new(),
      dump: None,
      finishes_received: 0,
    }
  }

  fn receive_once (&mut self)->bool {
    match self.receiver.try_recv() {
      Err (_) => false,
      Ok (message) => {self.received (message); true},
    }
  }
  fn receive_event_details (&self)->String {
    while let Ok (message) = self.receiver.recv() {
      if let Message::EventDetails (event_details) = message {
        return event_details;
      }
    }
    panic!("did not receive expected event details");
  }

  fn received (&mut self, message: Message <B>) {
        match message {
          Message::InsertFiatEvent (time, id, event_id, data) => do_fiat_event_message (event_id, self, time, id, data),
          Message::RemoveFiatEvent (time, id) => self.remove_fiat_event (&time, id).unwrap(),
          Message::Settled (chunk) => {
            self.other_settled_through = chunk;
            self.do_checksums();
          },
          Message::Checksum (chunk, checksum) => {
            if self.checksums [chunk as usize] != checksum && self.dump.is_none() {
              println!("detected desynchronization in chunk {}", chunk);
              self.dump = Some (self.steward.debug_dump (chunk));
              self.sender.send (Message::DebugDump (self.dump.clone().unwrap())).unwrap();
            }
          },
          Message::DebugDump (events) => {
            let mut my_iter = self.dump.as_ref().unwrap().iter();
            let mut other_iter = events.iter();
            loop {
              match (my_iter.next(), other_iter.next()) {
                (None, None) => panic!("both debug dumps are the same, even though the checksums were different?"),
                (Some ((my_time, my_checksum)), Some ((other_time, other_checksum))) => {
                  if my_time < other_time {
                    let event_details = self.steward.event_details (my_time);
                    self.sender.send (Message::EventDetails (event_details.clone())).unwrap();
                    panic!("event only occurred locally:\n {}", event_details);
                  }
                  if my_time > other_time {
                    panic!("event only occurred remotely:\n {}", self.receive_event_details());
                  }
                  if my_checksum != other_checksum {
                    let event_details = self.steward.event_details (my_time);
                    self.sender.send (Message::EventDetails (event_details.clone())).unwrap();
                    panic!("event occurred this way locally:\n {}\n\nbut this way remotely: {}", event_details, self.receive_event_details());
                  }
                },
                (Some ((my_time, _)), None) => {
                  let event_details = self.steward.event_details (my_time);
                  self.sender.send (Message::EventDetails (event_details.clone())).unwrap();
                  panic!("event only occurred locally:\n {}", event_details);
                },
                (None, Some ((_, _))) => {
                  panic!("event only occurred remotely:\n {}", self.receive_event_details());
                },
              }
            }
          },
          Message::EventDetails (_) => panic!("We should not receive an event details except where specifically expecting it"),
          Message::Finished (_) => {self.finishes_received += 1;},
        };
  }

  pub fn settle_before (&mut self, time: B::Time) {
    if self.valid_since() > time {
      return;
    }
    self.valid_since = ValidSince::Before (time.clone());
    let settled_chunk: i64 = (time - self. start.clone())/self.stride.clone() - 1;
    self.settled_through =::std::cmp::max (settled_chunk, self.settled_through);
    self.sender.send (Message::Settled (self.settled_through)).unwrap();
    self.do_checksums();
  }

  fn do_checksums (&mut self) {
    while (self.checksums.len() as i64) <= ::std::cmp::min (self.settled_through, self.other_settled_through) {
      let checksum: u64 = self.steward.checksum (self.checksums.len() as i64);
      self.sender.send (Message::Checksum (self.checksums.len() as i64, checksum)).unwrap();
      self.checksums.push (checksum);
    }
  }

  pub fn finish (&mut self) {
    for round in 0..10 {
      self.sender.send (Message::Finished (round)).unwrap();
      loop {
        let message = self.receiver.recv().unwrap();
        self.received (message);
        if self.finishes_received > round {break;}
      }
    }
  }
}

impl<B: Basics, Steward0: SimpleSynchronizableTimeSteward<Basics = B>> TimeSteward for Steward<B, Steward0>
where  B::Time: Sub <Output = B::Time> + Mul<i64, Output = B::Time> + Div<B::Time, Output = i64> {
  type Basics = B;
  type Snapshot = Steward0::Snapshot;
  
  fn valid_since(&self) -> ValidSince<B::Time> {
    self.valid_since.clone()
  }
    
  fn insert_fiat_event<E: ::Event<Basics = B>>(&mut self,
                                        time: B::Time,
                                        id: DeterministicRandomId,
                                        event: E)
                                        -> Result<(), FiatEventOperationError> {
    time_steward_common_insert_fiat_event_prefix!(B, self, time, E);
    let qualified_id = DeterministicRandomId::new (& (id, self .id));
    let result = self.steward.insert_fiat_event (time.clone(), qualified_id, event.clone());
    match result {
      Ok (_) => self.sender.send (Message::InsertFiatEvent (time, qualified_id, E::event_id(), bincode::serde::serialize (&event, bincode::SizeLimit::Infinite).unwrap())).unwrap(),
      Err (FiatEventOperationError::InvalidInput) => (),
      Err (FiatEventOperationError::InvalidTime) => (),
    }
    result
  }

  fn remove_fiat_event(&mut self,
                      time: &B::Time,
                      id: DeterministicRandomId)
                      -> Result<(), FiatEventOperationError> {
    if self.valid_since() > *time {
      return Err(FiatEventOperationError::InvalidTime);
    }
    let qualified_id = DeterministicRandomId::new (& (id, self .id));
    let result = self.steward.remove_fiat_event (time, qualified_id);
    match result {
      Ok (_) => self.sender.send (Message::RemoveFiatEvent (time.clone(), qualified_id)).unwrap(),
      Err (FiatEventOperationError::InvalidInput) => (),
      Err (FiatEventOperationError::InvalidTime) => (),
    }
    result
  }

  fn snapshot_before<'b>(&'b mut self, time: &'b B::Time) -> Option<Steward0::Snapshot> {
    while self.receive_once() {}
    self.steward.snapshot_before (time)
  }
}


impl<B: Basics, Steward0: ::IncrementalTimeSteward + SimpleSynchronizableTimeSteward<Basics = B>> ::IncrementalTimeSteward for Steward<B, Steward0>
where << Steward0 as TimeSteward>::Basics as Basics>::Time: Sub <Output = << Steward0 as TimeSteward>::Basics as Basics>::Time> + Mul<i64, Output = << Steward0 as TimeSteward>::Basics as Basics>::Time> + Div<<<Steward0 as TimeSteward>::Basics as Basics>::Time, Output = i64>
{
  fn step(&mut self) {
    if !self.receive_once() { self.steward.step(); }
  }
  fn updated_until_before (&self)->Option <B::Time> {
    self.steward.updated_until_before()
  }
}
