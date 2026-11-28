mod input;
mod pty;
mod telemetry;
mod ui;

use clap::Parser;
use crossterm::event::{
    self, DisableBracketedPaste, DisableMouseCapture, EnableBracketedPaste, EnableMouseCapture,
    Event, KeyCode, KeyEventKind, KeyModifiers, MouseEventKind,
};
use crossterm::execute;
use crossterm::terminal::{
    disable_raw_mode, enable_raw_mode, size as term_size, EnterAlternateScreen,
    LeaveAlternateScreen,
};
use pty::PtySession;
use ratatui::backend::CrosstermBackend;
use ratatui::layout::Rect;
use ratatui::Terminal;
use std::io::stdout;
use std::sync::atomic::Ordering;
use std::time::Duration;
use telemetry::spawn_udp_listener;
use ui::{draw, pty_inner_size, AppState, Focus};

#[derive(Parser, Debug)]
#[command(name = "kairos-tui", about = "Single-pane TUI wrapper for Kairos ghci")]
struct Args {
    /// UDP port to listen on for Kairos telemetry
    #[arg(long, default_value_t = 9001)]
    port: u16,

    /// Percentage of terminal height allocated to the visualization pane
    #[arg(long, default_value_t = 45)]
    split: u16,

    /// UI refresh rate (frames per second)
    #[arg(long, default_value_t = 30)]
    fps: u64,

    /// Command to launch inside the PTY (split on whitespace)
    #[arg(long)]
    cmd: Option<String>,
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let args = Args::parse();

    let (program, cmd_args): (String, Vec<String>) = match &args.cmd {
        Some(c) => {
            let mut parts = c.split_whitespace().map(String::from);
            let prog = parts.next().unwrap_or_else(|| "stack".to_string());
            (prog, parts.collect())
        }
        None => (
            "stack".to_string(),
            vec![
                "ghci".to_string(),
                "--ghci-options=-ghci-script BootKairos.hs".to_string(),
            ],
        ),
    };

    let (cols, rows) = term_size()?;
    let init_rect = Rect::new(0, 0, cols, rows);
    let (pty_rows, pty_cols) = pty_inner_size(init_rect, args.split);

    let rx = spawn_udp_listener(args.port)?;
    let mut pty = PtySession::spawn(&program, &cmd_args, pty_rows, pty_cols, args.port)?;

    enable_raw_mode()?;
    let mut out = stdout();
    execute!(out, EnterAlternateScreen, EnableBracketedPaste, EnableMouseCapture)?;
    let backend = CrosstermBackend::new(out);
    let mut terminal = Terminal::new(backend)?;

    let res = run_loop(&mut terminal, &mut pty, &rx, &args);

    disable_raw_mode()?;
    execute!(
        terminal.backend_mut(),
        LeaveAlternateScreen,
        DisableBracketedPaste,
        DisableMouseCapture
    )?;
    terminal.show_cursor()?;

    res
}

fn run_loop(
    terminal: &mut Terminal<CrosstermBackend<std::io::Stdout>>,
    pty: &mut PtySession,
    rx: &std::sync::mpsc::Receiver<telemetry::TelemetryPacket>,
    args: &Args,
) -> Result<(), Box<dyn std::error::Error>> {
    let mut app = AppState::new(args.split);
    let frame_dur = Duration::from_millis((1000 / args.fps.max(1)).max(8));

    loop {
        while let Ok(pkt) = rx.try_recv() {
            app.apply_packet(pkt);
        }

        terminal.draw(|f| draw(f, &mut app, &pty.parser))?;

        if pty.exited.load(Ordering::SeqCst) {
            break;
        }

        if event::poll(frame_dur)? {
            match event::read()? {
                Event::Resize(cols, rows) => {
                    let (pr, pc) = pty_inner_size(Rect::new(0, 0, cols, rows), app.viz_percent);
                    pty.resize(pr, pc);
                }
                Event::Paste(text) => {
                    if app.focus == Focus::Repl {
                        pty.reset_scrollback();
                        let _ = pty.write_bytes(b"\x1b[200~");
                        let _ = pty.write_bytes(text.as_bytes());
                        let _ = pty.write_bytes(b"\x1b[201~");
                    }
                }
                Event::Mouse(m) => match m.kind {
                    MouseEventKind::ScrollUp => pty.scroll_up(3),
                    MouseEventKind::ScrollDown => pty.scroll_down(3),
                    _ => {}
                },
                Event::Key(key) if key.kind != KeyEventKind::Release => {
                    if key.code == KeyCode::F(2) {
                        app.focus = match app.focus {
                            Focus::Repl => Focus::Viz,
                            Focus::Viz => Focus::Repl,
                        };
                        continue;
                    }

                    match app.focus {
                        Focus::Repl => {
                            let shift = key.modifiers.contains(KeyModifiers::SHIFT);
                            match (shift, key.code) {
                                (true, KeyCode::PageUp) => pty.scroll_up(10),
                                (true, KeyCode::PageDown) => pty.scroll_down(10),
                                (true, KeyCode::Up) => pty.scroll_up(1),
                                (true, KeyCode::Down) => pty.scroll_down(1),
                                _ => {
                                    pty.reset_scrollback();
                                    let bytes = input::key_event_to_bytes(key);
                                    if !bytes.is_empty() {
                                        let _ = pty.write_bytes(&bytes);
                                    }
                                }
                            }
                        }
                        Focus::Viz => match key.code {
                            KeyCode::Esc => app.focus = Focus::Repl,
                            KeyCode::Char('j') | KeyCode::Down => app.select_next(),
                            KeyCode::Char('k') | KeyCode::Up => app.select_prev(),
                            KeyCode::Char('J') | KeyCode::PageDown => {
                                app.param_scroll = app.param_scroll.saturating_add(1);
                            }
                            KeyCode::Char('K') | KeyCode::PageUp => {
                                app.param_scroll = app.param_scroll.saturating_sub(1);
                            }
                            KeyCode::Char('i') => app.show_inactive = !app.show_inactive,
                            _ => {}
                        },
                    }
                }
                _ => {}
            }
        }
    }

    Ok(())
}
