use portable_pty::{native_pty_system, CommandBuilder, MasterPty, PtySize};
use std::io::{Read, Write};
use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::{Arc, Mutex};
use std::thread;

pub struct PtySession {
    master: Box<dyn MasterPty + Send>,
    writer: Box<dyn Write + Send>,
    pub parser: Arc<Mutex<vt100::Parser>>,
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

        let parser = Arc::new(Mutex::new(vt100::Parser::new(rows.max(1), cols.max(1), 1000)));
        let exited = Arc::new(AtomicBool::new(false));

        let parser_clone = Arc::clone(&parser);
        let exited_clone = Arc::clone(&exited);

        thread::spawn(move || {
            let mut buf = [0u8; 4096];
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
                    }
                }
            }
        });

        Ok(Self {
            master: pair.master,
            writer,
            parser,
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

    pub fn scroll_up(&self, lines: usize) {
        if let Ok(mut p) = self.parser.lock() {
            let cur = p.screen().scrollback();
            p.set_scrollback(cur.saturating_add(lines));
        }
    }

    pub fn scroll_down(&self, lines: usize) {
        if let Ok(mut p) = self.parser.lock() {
            let cur = p.screen().scrollback();
            p.set_scrollback(cur.saturating_sub(lines));
        }
    }

    pub fn reset_scrollback(&self) {
        if let Ok(mut p) = self.parser.lock() {
            if p.screen().scrollback() > 0 {
                p.set_scrollback(0);
            }
        }
    }
}
