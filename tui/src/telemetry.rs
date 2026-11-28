use serde::Deserialize;
use std::collections::BTreeMap;
use std::net::UdpSocket;
use std::sync::mpsc::{self, Receiver};
use std::thread;

#[derive(Debug, Clone, Deserialize)]
#[serde(tag = "type")]
pub enum TelemetryPacket {
    #[serde(rename = "state")]
    State {
        clock: ClockState,
        instruments: Vec<InstrumentState>,
    },
    #[serde(rename = "note")]
    Note(NoteEvent),
}

#[derive(Debug, Clone, Deserialize, Default)]
pub struct ClockState {
    pub bar: i64,
    pub beat_in_bar: f64,
    pub bpm: f64,
    pub beats_per_bar: f64,
    pub mode: String,
}

#[derive(Debug, Clone, Deserialize)]
pub struct InstrumentState {
    pub name: String,
    pub ins_n: i32,
    pub status: String,
    pub time_f: String,
    pub tpat: Vec<f64>,
    #[allow(dead_code)]
    pub to_play: Option<f64>,
    #[serde(default)]
    pub cycle_origin_bar: Option<i64>,
    pub pfields: BTreeMap<String, serde_json::Value>,
    pub patterned_pfs: Vec<String>,
}

#[derive(Debug, Clone, Deserialize)]
pub struct NoteEvent {
    pub instr: String,
    pub beat: f64,
    pub pfields: BTreeMap<String, serde_json::Value>,
}

pub fn spawn_udp_listener(port: u16) -> std::io::Result<Receiver<TelemetryPacket>> {
    let socket = UdpSocket::bind(("127.0.0.1", port))?;
    let (tx, rx) = mpsc::channel();

    thread::spawn(move || {
        let mut buf = [0u8; 65535];
        loop {
            match socket.recv(&mut buf) {
                Ok(n) => {
                    if let Ok(pkt) = serde_json::from_slice::<TelemetryPacket>(&buf[..n]) {
                        if tx.send(pkt).is_err() {
                            break;
                        }
                    }
                }
                Err(_) => break,
            }
        }
    });

    Ok(rx)
}
