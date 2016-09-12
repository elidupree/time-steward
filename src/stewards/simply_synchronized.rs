/*
pub trait SimpleSynchronizableTimeSteward<B: Basics>: TimeSteward<B>
where B::Time: Sub + Mul<i64, Output = B::Time> + Div<B::Time, Output = i64> {
  fn begin_checks (&mut self, start: B::Time, stride: B::Time);
  fn checksum(&self, which: i64)->u64;
  fn debug_dump(&self, which: i64) ->BTreeMap<ExtendedTime <B>, u64>;
  fn event_details (&self, time: & ExtendedTime <B>)->String;
}
*/

use std::collections::{BTreeMap};
use std::io::{Read, Write};
use std::any::Any;
use std::ops::{Sub, Mul, Div};
use {ExtendedTime, Basics, TimeSteward, SimpleSynchronizableTimeSteward, DeterministicRandomId, EventId, Event, FiatEventOperationError, StewardRc,};
use std::sync::mpsc::{channel, Sender, Receiver};
use bincode;


#[derive (Serialize, Deserialize)]
enum Message <B: Basics> {
  InsertFiatEvent (B::Time, DeterministicRandomId, EventId, Vec<u8>),
  RemoveFiatEvent (B::Time, DeterministicRandomId),
  Settled (i64),
  Checksum (i64, u64),
  DebugDump (BTreeMap<ExtendedTime <B>, u64>),
  EventDetails (String),
}

pub struct Steward <B: Basics, Steward0: SimpleSynchronizableTimeSteward<Basics = B>> {
  steward: Steward0,
  id: DeterministicRandomId,
  sender: Sender <Message <B>>,
  receiver: Receiver <Message <B>>,
  start: B::Time, stride: B::Time,
  settled_through: i64, other_settled_through: i64,
  checksums: Vec<u64>,
  dump: BTreeMap<ExtendedTime <B>, u64>,
}

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
        send_back.send (message);
      }
    });
    ::std::thread::spawn (move | | {
      loop {
        match receive_away.recv() {
          Err (_) => return,
          Ok (message) => match bincode::serde::serialize_into (&mut writer, &message, bincode::SizeLimit::Infinite) {
            Err (_) => return,
            Ok (_) => (),
          }
        };
      }
    });
    let mut steward: Steward0 = TimeSteward::new_empty (constants);
    steward.begin_checks (start.clone(), stride.clone());
    Steward {
      steward: steward,
      id: id,
      sender: send_away,
      receiver: receive_back,
      start: start, stride: stride,
      settled_through: -1, other_settled_through: -1,
      checksums: Vec::new(),
      dump: BTreeMap::new(),
    }
  }
  
  fn receive_once (&mut self)->bool {
    match self.receiver.try_recv() {
      Err (_) => false,
      Ok (message) => {
        match message {
          Message::InsertFiatEvent (time, id, event_id, data) => unimplemented!(),
          Message::RemoveFiatEvent (time, id) => self.remove_fiat_event (&time, id).unwrap(),
          Message::Settled (chunk) => {
            self.other_settled_through = chunk;
            self.do_checksums();
          },
          Message::Checksum (chunk, checksum) => {
            if self.checksums [chunk as usize] != checksum {
              self.dump = self.steward.debug_dump (chunk);
              self.sender.send (Message::DebugDump (self.dump.clone())).unwrap();
            }
          },
          Message::DebugDump (events) => {
            let mut my_iter = self.dump.iter();
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
                    if let Message::EventDetails (event_details) = self.receiver.recv().unwrap() {
                      panic!("event only occurred remotely:\n {}", event_details);
                    }
                    panic!("did not receive expected event details");
                  }
                  if my_checksum != other_checksum {
                    let event_details = self.steward.event_details (my_time);
                    self.sender.send (Message::EventDetails (event_details.clone())).unwrap();                 if let Message::EventDetails (other_details) = self.receiver.recv().unwrap() {
                      panic!("event occurred this way locally:\n {}\n\nbut this way remotely: {}", event_details, other_details);
                    }
                    panic!("did not receive expected event details");
                  }
                },
                (Some ((my_time, my_checksum)), None) => {
                  let event_details = self.steward.event_details (my_time);
                  self.sender.send (Message::EventDetails (event_details.clone())).unwrap();
                  panic!("event only occurred locally:\n {}", event_details);
                },
                (None, Some ((other_time, other_checksum))) => {
                  if let Message::EventDetails (event_details) = self.receiver.recv().unwrap() {
                    panic!("event only occurred remotely:\n {}", event_details);
                  }
                },
              }
            }
          },
          Message::EventDetails (string) => panic!("We should not receive an event details except where specifically expecting it"),
        };
        true
      }
    }
  }
  
  pub fn settle_before (&mut self, time: B::Time) {
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
  
  pub fn insert_fiat_event<E: ::Event<Basics = B>>(&mut self,
                                        time: B::Time,
                                        id: DeterministicRandomId,
                                        event: E)
                                        -> Result<(), FiatEventOperationError> {
    let id = DeterministicRandomId::new (& (id, self .id));
    let result = self.steward.insert_fiat_event (time.clone(), id, event.clone());
    match result {
      Ok (_) => self.sender.send (Message::InsertFiatEvent (time, id, E::event_id(), bincode::serde::serialize (&event, bincode::SizeLimit::Infinite).unwrap())).unwrap(),
      Err (FiatEventOperationError::InvalidInput) => (),
      Err (FiatEventOperationError::InvalidTime) => (),
    }
    result
  }

  pub fn remove_fiat_event(&mut self,
                      time: &B::Time,
                      id: DeterministicRandomId)
                      -> Result<(), FiatEventOperationError> {
    let id = DeterministicRandomId::new (& (id, self .id));
    let result = self.steward.remove_fiat_event (time, id);
    match result {
      Ok (_) => self.sender.send (Message::RemoveFiatEvent (time.clone(), id)).unwrap(),
      Err (FiatEventOperationError::InvalidInput) => (),
      Err (FiatEventOperationError::InvalidTime) => (),
    }
    result
  }

  pub fn snapshot_before<'b>(&'b mut self, time: &'b B::Time) -> Option<Steward0::Snapshot> {
    while self.receive_once() {}
    self.steward.snapshot_before (time)
  }

}


impl<B: Basics, Steward0: ::IncrementalTimeSteward + SimpleSynchronizableTimeSteward<Basics = B>> Steward<B, Steward0>
where << Steward0 as TimeSteward>::Basics as Basics>::Time: Sub <Output = << Steward0 as TimeSteward>::Basics as Basics>::Time> + Mul<i64, Output = << Steward0 as TimeSteward>::Basics as Basics>::Time> + Div<<<Steward0 as TimeSteward>::Basics as Basics>::Time, Output = i64>
{
  pub fn step(&mut self) {
    if !self.receive_once() { self.steward.step(); }
  }
  pub fn updated_until_before (&self)->Option <B::Time> {
    self.steward.updated_until_before()
  }
}

