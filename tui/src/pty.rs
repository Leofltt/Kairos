use portable_pty::{native_pty_system, CommandBuilder, MasterPty, PtySize};
use std::collections::VecDeque;
use std::io::{Read, Write};
use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::{Arc, Mutex};
use std::thread;

const MAX_HISTORY_LINES: usize = 10_000;

pub struct PtySession {
    master: Box<dyn MasterPty + Send>,
    writer: Box<dyn Write + Send>,
    pub parser: Arc<Mutex<vt100::Parser>>,
    pub history: Arc<Mutex<VecDeque<String>>>,
    pub scroll_offset: usize,
    pub exited: Arc<AtomicBool>,
}

impl PtySession {
    pub fn spawn(
        program: &str,
        args: &[String],
        rows: u16,
        cols: u16,
        telemetry_port: u16,
    ) -> Result<Self, Box<dyn std::error::Error>> {
        let pty_system = native_pty_system();
        let pair = pty_system.openpty(PtySize {
            rows: rows.max(1),
            cols: cols.max(1),
            pixel_width: 0,
            pixel_height: 0,
        })?;

        let mut cmd = CommandBuilder::new(program);
        cmd.args(args);
        cmd.env("KAIROS_TELEMETRY_PORT", telemetry_port.to_string());
        cmd.env("TERM", "xterm-256color");
        if let Ok(cwd) = std::env::current_dir() {
            cmd.cwd(cwd);
        }

        let mut child = pair.slave.spawn_command(cmd)?;
        drop(pair.slave);

        let mut reader = pair.master.try_clone_reader()?;
        let writer = pair.master.take_writer()?;

        // Keep vt100 scrollback at 0 to avoid vt100 0.15's scrollback subtraction panic;
        // scrollback is managed safely via `history` below.
        let parser = Arc::new(Mutex::new(vt100::Parser::new(rows.max(1), cols.max(1), 0)));
        let history = Arc::new(Mutex::new(VecDeque::from([String::new()])));
        let exited = Arc::new(AtomicBool::new(false));

        let parser_clone = Arc::clone(&parser);
        let history_clone = Arc::clone(&history);
        let exited_clone = Arc::clone(&exited);

        thread::spawn(move || {
            let mut buf = [0u8; 4096];
            let mut strip_state = AnsiStripper::new();
            loop {
                match reader.read(&mut buf) {
                    Ok(0) | Err(_) => {
                        let _ = child.wait();
                        exited_clone.store(true, Ordering::SeqCst);
                        break;
                    }
                    Ok(n) => {
                        if let Ok(mut p) = parser_clone.lock() {
                            p.process(&buf[..n]);
                        }
                        let text = String::from_utf8_lossy(&buf[..n]);
                        if let Ok(mut hist) = history_clone.lock() {
                            strip_state.push_str(&text, &mut hist);
                        }
                    }
                }
            }
        });

        Ok(Self {
            master: pair.master,
            writer,
            parser,
            history,
            scroll_offset: 0,
            exited,
        })
    }

    pub fn write_bytes(&mut self, bytes: &[u8]) -> std::io::Result<()> {
        self.writer.write_all(bytes)?;
        self.writer.flush()
    }

    pub fn resize(&mut self, rows: u16, cols: u16) {
        let r = rows.max(1);
        let c = cols.max(1);
        let _ = self.master.resize(PtySize {
            rows: r,
            cols: c,
            pixel_width: 0,
            pixel_height: 0,
        });
        if let Ok(mut p) = self.parser.lock() {
            p.set_size(r, c);
        }
    }

    pub fn scroll_up(&mut self, lines: usize) {
        let max_offset = self
            .history
            .lock()
            .map(|h| h.len().saturating_sub(1))
            .unwrap_or(0);
        self.scroll_offset = self.scroll_offset.saturating_add(lines).min(max_offset);
    }

    pub fn scroll_down(&mut self, lines: usize) {
        self.scroll_offset = self.scroll_offset.saturating_sub(lines);
    }

    pub fn reset_scrollback(&mut self) {
        self.scroll_offset = 0;
    }
}

enum StripState {
    Normal,
    Esc,
    Csi,
    Osc,
    OscEsc,
}

struct AnsiStripper {
    state: StripState,
}

impl AnsiStripper {
    fn new() -> Self {
        Self {
            state: StripState::Normal,
        }
    }

    fn push_str(&mut self, s: &str, hist: &mut VecDeque<String>) {
        for ch in s.chars() {
            match self.state {
                StripState::Normal => match ch {
                    '\x1b' => self.state = StripState::Esc,
                    '\n' => {
                        hist.push_back(String::new());
                        if hist.len() > MAX_HISTORY_LINES {
                            hist.pop_front();
                        }
                    }
                    '\r' => {}
                    '\x08' => {
                        if let Some(last) = hist.back_mut() {
                            last.pop();
                        }
                    }
                    c if c.is_control() && c != '\t' => {}
                    c => {
                        if let Some(last) = hist.back_mut() {
                            last.push(c);
                        }
                    }
                },
                StripState::Esc => match ch {
                    '[' => self.state = StripState::Csi,
                    ']' => self.state = StripState::Osc,
                    _ => self.state = StripState::Normal,
                },
                StripState::Csi => {
                    if ('@'..='~').contains(&ch) {
                        self.state = StripState::Normal;
                    }
                }
                StripState::Osc => match ch {
                    '\x07' => self.state = StripState::Normal,
                    '\x1b' => self.state = StripState::OscEsc,
                    _ => {}
                },
                StripState::OscEsc => {
                    if ch == '\\' {
                        self.state = StripState::Normal;
                    } else {
                        self.state = StripState::Osc;
                    }
                }
            }
        }
    }
}
