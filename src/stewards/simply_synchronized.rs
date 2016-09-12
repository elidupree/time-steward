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
use {ExtendedTime, Basics, TimeSteward, FullTimeSteward, DeterministicRandomId, EventId, Event, FiatEventOperationError, StewardRc,};
use std::sync::mpsc::{channel, Sender, Receiver};
use bincode;


#[derive (Serialize, Deserialize)]
enum Message <B: Basics> {
  InsertFiatEvent (B::Time, DeterministicRandomId, EventId, Vec<u8>),
  RemoveFiatEvent (B::Time, DeterministicRandomId),
  SettledWithChecksum (i64, u64),
  DebugDump (BTreeMap<ExtendedTime <B>, u64>),
  EventDetails (String),
}

struct Steward <B: Basics, Steward0: FullTimeSteward <Basics = B>, Writer: Write> {
  steward: Steward0,
  id: DeterministicRandomId,
  writer: Writer,
  receiver: Receiver <Message <B>>,
}

impl <B: Basics, Steward0: FullTimeSteward <Basics = B>, Writer: Write> Steward <B, Steward0, Writer> {
  fn new <Reader: Read + Send + 'static> (id: DeterministicRandomId, constants: B::Constants, mut reader: Reader, writer: Writer)->Self {
    let (send, receive) = channel();
    ::std::thread::spawn (move | | {
      loop {
        let message: Message <B> = match bincode::serde::deserialize_from (&mut reader, bincode::SizeLimit::Infinite) {
          Ok (message) => message,
          Err (_) => return,
        };
        send.send (message);
      }
    });
    Steward {
      steward: TimeSteward::new_empty (constants),
      id: id,
      writer: writer,
      receiver: receive,
    }
  }
  
  fn receive_once (&mut self)->bool {
    match self.receiver.try_recv() {
      Err (_) => false,
      Ok (message) => {
        match message {
          Message::InsertFiatEvent (time, id, event_id, data) => unimplemented!(),
          Message::RemoveFiatEvent (time, id) => self.remove_fiat_event (&time, id).unwrap(),
          Message::SettledWithChecksum (which, checksum) => unimplemented!(),
          Message::DebugDump (events) => unimplemented!(),
          Message::EventDetails (string) => unimplemented!(),
        };
        true
      }
    }
  }
  
  fn insert_fiat_event<E: ::Event<Basics = B>>(&mut self,
                                        time: B::Time,
                                        id: DeterministicRandomId,
                                        event: E)
                                        -> Result<(), FiatEventOperationError> {
    unimplemented!()
  }

  fn remove_fiat_event(&mut self,
                      time: &B::Time,
                      id: DeterministicRandomId)
                      -> Result<(), FiatEventOperationError> {
    unimplemented!()
  }

  fn snapshot_before<'b>(&'b mut self, time: &'b B::Time) -> Option<Steward0::Snapshot> {
    unimplemented!()
  }

}
//, 