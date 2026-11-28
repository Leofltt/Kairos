use crate::telemetry::{ClockState, InstrumentState, NoteEvent, TelemetryPacket};
use ratatui::layout::{Constraint, Direction, Layout, Position, Rect};
use ratatui::style::{Color, Modifier, Style};
use ratatui::text::{Line, Span};
use ratatui::widgets::{Block, Borders, List, ListItem, ListState, Paragraph};
use ratatui::Frame;
use std::collections::{HashMap, HashSet, VecDeque};
use std::sync::{Arc, Mutex};
use std::time::Instant;

// Palette from leofltt-monoish.vim & ghostty/config
const BLACK: Color = Color::Rgb(0x00, 0x00, 0x00);
const DARK_GRAY2: Color = Color::Rgb(0x33, 0x33, 0x33);
const AMETHYST_BG: Color = Color::Rgb(0x2D, 0x26, 0x38);
const MED_GRAY1: Color = Color::Rgb(0x55, 0x55, 0x55);
const MED_GRAY2: Color = Color::Rgb(0x88, 0x88, 0x88);
const COOL_SILVER: Color = Color::Rgb(0xC6, 0xC8, 0xD6);
const LIGHT_GRAY2: Color = Color::Rgb(0xDC, 0xDC, 0xDC);
const OFF_WHITE: Color = Color::Rgb(0xEA, 0xEA, 0xEA);
const WHITE: Color = Color::Rgb(0xFF, 0xFF, 0xFF);
const RED: Color = Color::Rgb(0xCC, 0x55, 0x55);
const MUTED_BLUE: Color = Color::Rgb(0x6B, 0x88, 0xB3);
const DUSKY_PLUM: Color = Color::Rgb(0x8A, 0x7F, 0xA8);
const SMOKY_AMETHYST: Color = Color::Rgb(0xB0, 0xA4, 0xCF);
const DUSTY_LILAC: Color = Color::Rgb(0xB5, 0xA6, 0xC9);
const LILAC: Color = Color::Rgb(0xD3, 0xB1, 0xE6);

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Focus {
    Repl,
    Viz,
}

pub struct AppState {
    pub clock: ClockState,
    pub instruments: Vec<InstrumentState>,
    pub last_note_flash: HashMap<String, Instant>,
    pub recent_notes: HashMap<String, VecDeque<NoteEvent>>,
    pub selected_idx: usize,
    pub list_state: ListState,
    pub param_scroll: u16,
    pub show_inactive: bool,
    pub focus: Focus,
    pub viz_percent: u16,
    pub connected: bool,
    pub last_packet_at: Option<Instant>,
}

impl AppState {
    pub fn new(viz_percent: u16) -> Self {
        Self {
            clock: ClockState {
                bar: 0,
                beat_in_bar: 0.0,
                bpm: 120.0,
                beats_per_bar: 4.0,
                mode: "Local".to_string(),
            },
            instruments: Vec::new(),
            last_note_flash: HashMap::new(),
            recent_notes: HashMap::new(),
            selected_idx: 0,
            list_state: ListState::default(),
            param_scroll: 0,
            show_inactive: false,
            focus: Focus::Repl,
            viz_percent: viz_percent.clamp(15, 80),
            connected: false,
            last_packet_at: None,
        }
    }

    pub fn apply_packet(&mut self, pkt: TelemetryPacket) {
        self.connected = true;
        self.last_packet_at = Some(Instant::now());
        match pkt {
            TelemetryPacket::State { clock, instruments } => {
                self.clock = clock;
                self.instruments = instruments;
                let visible_len = self.visible_instruments().len();
                if visible_len == 0 {
                    self.selected_idx = 0;
                    self.list_state.select(None);
                    self.param_scroll = 0;
                } else {
                    if self.selected_idx >= visible_len {
                        self.selected_idx = visible_len - 1;
                        self.param_scroll = 0;
                    }
                    self.list_state.select(Some(self.selected_idx));
                }
            }
            TelemetryPacket::Note(ev) => {
                self.last_note_flash.insert(ev.instr.clone(), Instant::now());
                let queue = self
                    .recent_notes
                    .entry(ev.instr.clone())
                    .or_insert_with(|| VecDeque::with_capacity(8));
                if queue.len() >= 8 {
                    queue.pop_front();
                }
                queue.push_back(ev);
            }
        }
    }

    pub fn visible_instruments(&self) -> Vec<&InstrumentState> {
        self.instruments
            .iter()
            .filter(|i| self.show_inactive || i.status != "Inactive")
            .collect()
    }

    pub fn select_next(&mut self) {
        let len = self.visible_instruments().len();
        if len > 0 {
            self.selected_idx = (self.selected_idx + 1) % len;
            self.list_state.select(Some(self.selected_idx));
            self.param_scroll = 0;
        }
    }

    pub fn select_prev(&mut self) {
        let len = self.visible_instruments().len();
        if len > 0 {
            self.selected_idx = if self.selected_idx == 0 {
                len - 1
            } else {
                self.selected_idx - 1
            };
            self.list_state.select(Some(self.selected_idx));
            self.param_scroll = 0;
        }
    }
}

pub fn split_areas(area: Rect, viz_percent: u16) -> (Rect, Rect, Rect) {
    let chunks = Layout::default()
        .direction(Direction::Vertical)
        .constraints([
            Constraint::Percentage(viz_percent),
            Constraint::Length(1),
            Constraint::Min(5),
        ])
        .split(area);
    (chunks[0], chunks[1], chunks[2])
}

pub fn pty_inner_size(area: Rect, viz_percent: u16) -> (u16, u16) {
    let (_, _, pty_area) = split_areas(area, viz_percent);
    let rows = pty_area.height.saturating_sub(2).max(1);
    let cols = pty_area.width.saturating_sub(2).max(1);
    (rows, cols)
}

pub fn draw(frame: &mut Frame, app: &mut AppState, parser: &Arc<Mutex<vt100::Parser>>) {
    let (viz_area, sep_area, pty_area) = split_areas(frame.area(), app.viz_percent);

    draw_viz(frame, app, viz_area);
    draw_separator(frame, app, sep_area);
    draw_pty(frame, app, parser, pty_area);
}

fn draw_separator(frame: &mut Frame, app: &AppState, area: Rect) {
    let bpb = if app.clock.beats_per_bar > 0.0 {
        app.clock.beats_per_bar
    } else {
        4.0
    };
    let beat_clamped = app.clock.beat_in_bar.max(0.0);
    let beat_int = beat_clamped.floor() as i64;
    let frac_ms = ((beat_clamped.fract().clamp(0.0, 0.9999)) * 1000.0).floor() as u32;

    let mode_tag = if app.connected {
        app.clock.mode.as_str()
    } else {
        "WAIT"
    };
    let focus_tag = match app.focus {
        Focus::Repl => "REPL (F2->Viz)",
        Focus::Viz => "VIZ (j/k, J/K, F2->REPL)",
    };

    let text = format!(
        " {:.1} BPM │ {:.0} bpb │ {}:{}:{:03} │ {} │ {}",
        app.clock.bpm, bpb, app.clock.bar, beat_int, frac_ms, mode_tag, focus_tag
    );

    let bar = Paragraph::new(text).style(
        Style::default().fg(if app.connected { LIGHT_GRAY2 } else { MED_GRAY2 }),
    );
    frame.render_widget(bar, area);
}

fn draw_viz(frame: &mut Frame, app: &mut AppState, area: Rect) {
    let cols = Layout::default()
        .direction(Direction::Horizontal)
        .constraints([Constraint::Percentage(60), Constraint::Percentage(40)])
        .split(area);

    draw_tpat_grid(frame, app, cols[0]);
    draw_param_inspector(frame, app, cols[1]);
}

fn pattern_bars(tpat: &[f64], bpb: f64) -> i64 {
    let max_beat = tpat.iter().copied().fold(0.0_f64, f64::max);
    if tpat.is_empty() || max_beat <= 0.0 {
        1
    } else {
        ((max_beat / bpb).floor() as i64 + 1).max(1)
    }
}

#[derive(Clone, Copy, PartialEq, Eq)]
enum CellKind {
    Empty,
    BeatDiv,
    BarDiv,
    Hit { looped: bool },
    Unused,
}

fn draw_tpat_grid(frame: &mut Frame, app: &mut AppState, area: Rect) {
    let border_style = if app.focus == Focus::Viz {
        Style::default().fg(SMOKY_AMETHYST)
    } else {
        Style::default().fg(MED_GRAY1)
    };
    let bpb = if app.clock.beats_per_bar > 0.0 {
        app.clock.beats_per_bar
    } else {
        4.0
    };

    let visible = app.visible_instruments();
    if visible.is_empty() {
        let base_title = if app.show_inactive {
            " Active & Inactive TPats "
        } else {
            " Active TPats "
        };
        let block = Block::default()
            .borders(Borders::ALL)
            .border_style(border_style)
            .title(base_title);
        let msg = Paragraph::new("No active instruments. Run `p \"kick\"` in ghci (or press F2 + `i` to show inactive).")
            .style(Style::default().fg(MED_GRAY2))
            .block(block);
        frame.render_widget(msg, area);
        return;
    }

    let inner_width = area.width.saturating_sub(2) as usize;
    let label_width = 22usize;
    let grid_width = inner_width.saturating_sub(label_width + 1).max(8);

    let global_bars = visible
        .iter()
        .map(|ins| pattern_bars(&ins.tpat, bpb))
        .max()
        .unwrap_or(1)
        .max(1);
    let page_bars: i64 = if global_bars >= 2 && grid_width >= 24 { 2 } else { 1 };
    let cur_global_bar = app.clock.bar.rem_euclid(global_bars);
    let page_idx = cur_global_bar / page_bars;
    let page_start_bar = page_idx * page_bars;
    let page_end_bar = (page_start_bar + page_bars).min(global_bars);
    let active_bars_in_page = (page_end_bar - page_start_bar).max(1);

    let bar_label = if global_bars == 1 {
        "1 bar".to_string()
    } else if active_bars_in_page == 1 {
        format!("bar {}/{}", page_start_bar + 1, global_bars)
    } else {
        format!("bars {}-{}/{}", page_start_bar + 1, page_end_bar, global_bars)
    };
    let title = if app.show_inactive {
        format!(" Active & Inactive TPats [{}] ", bar_label)
    } else {
        format!(" Active TPats [{}] ", bar_label)
    };
    let block = Block::default()
        .borders(Borders::ALL)
        .border_style(border_style)
        .title(title);

    let page_beats = (page_bars as f64) * bpb;
    let pos_in_page =
        ((cur_global_bar - page_start_bar) as f64) * bpb + app.clock.beat_in_bar.clamp(0.0, bpb);
    let playhead_col = ((pos_in_page / page_beats) * (grid_width as f64))
        .floor()
        .clamp(0.0, (grid_width - 1) as f64) as usize;

    let items: Vec<ListItem> = visible
        .iter()
        .enumerate()
        .map(|(idx, ins)| {
            let is_selected = idx == app.selected_idx;

            let status_dot = match ins.status.as_str() {
                "Active" => Span::styled("● ", Style::default().fg(LILAC)),
                "Stopping" => Span::styled("◐ ", Style::default().fg(RED)),
                "Init" => Span::styled("○ ", Style::default().fg(MUTED_BLUE)),
                _ => Span::styled("· ", Style::default().fg(MED_GRAY2)),
            };

            let name_str = format!("{:<10} {:<9}", truncate(&ins.name, 10), truncate(&ins.time_f, 9));
            let name_style = if is_selected {
                Style::default().fg(WHITE).add_modifier(Modifier::BOLD)
            } else {
                Style::default().fg(LIGHT_GRAY2)
            };

            let ins_bars = pattern_bars(&ins.tpat, bpb);
            let active_cols = (((active_bars_in_page as f64) / (page_bars as f64)) * (grid_width as f64))
                .round()
                .clamp(1.0, grid_width as f64) as usize;

            let mut cells = vec![CellKind::Empty; grid_width];
            for c in active_cols..grid_width {
                cells[c] = CellKind::Unused;
            }

            let total_beats_shown = (active_bars_in_page as f64) * bpb;
            let total_beat_steps = total_beats_shown.round() as usize;
            for beat_step in 0..total_beat_steps {
                let col = (((beat_step as f64) / page_beats) * (grid_width as f64))
                    .floor()
                    .clamp(0.0, (active_cols - 1) as f64) as usize;
                let is_bar_start = (beat_step as f64 % bpb).abs() < 1e-6;
                cells[col] = if is_bar_start {
                    CellKind::BarDiv
                } else {
                    CellKind::BeatDiv
                };
            }

            for b in page_start_bar..page_end_bar {
                let is_looped = b >= ins_bars;
                let pat_bar = b.rem_euclid(ins_bars);
                let bar_offset_on_page = (b - page_start_bar) as f64 * bpb;

                for &tp in &ins.tpat {
                    if (tp / bpb).floor() as i64 == pat_bar {
                        let beat_in_this_bar = tp - (pat_bar as f64) * bpb;
                        let beat_on_page = bar_offset_on_page + beat_in_this_bar;
                        let c = ((beat_on_page / page_beats) * (grid_width as f64))
                            .floor()
                            .clamp(0.0, (active_cols - 1) as f64) as usize;
                        cells[c] = CellKind::Hit { looped: is_looped };
                    }
                }
            }

            let mut grid_spans = Vec::with_capacity(grid_width);
            for (c_idx, kind) in cells.into_iter().enumerate() {
                let (ch, fg) = match kind {
                    CellKind::Empty => ('·', DARK_GRAY2),
                    CellKind::BeatDiv => ('│', MED_GRAY1),
                    CellKind::BarDiv => ('┃', MED_GRAY2),
                    CellKind::Hit { looped: false } => ('◆', LILAC),
                    CellKind::Hit { looped: true } => ('◆', DUSKY_PLUM),
                    CellKind::Unused => (' ', Color::Reset),
                };

                if c_idx == playhead_col && kind != CellKind::Unused {
                    grid_spans.push(Span::styled(
                        ch.to_string(),
                        Style::default().fg(BLACK).bg(OFF_WHITE).add_modifier(Modifier::BOLD),
                    ));
                } else {
                    grid_spans.push(Span::styled(ch.to_string(), Style::default().fg(fg)));
                }
            }

            let mut spans = vec![status_dot, Span::styled(name_str, name_style), Span::raw(" ")];
            spans.extend(grid_spans);
            ListItem::new(Line::from(spans))
        })
        .collect();

    let highlight_style = if app.focus == Focus::Viz {
        Style::default().bg(AMETHYST_BG)
    } else {
        Style::default()
    };
    let list = List::new(items)
        .block(block)
        .highlight_style(highlight_style);
    frame.render_stateful_widget(list, area, &mut app.list_state);
}

fn draw_param_inspector(frame: &mut Frame, app: &mut AppState, area: Rect) {
    let visible = app.visible_instruments();
    let selected = visible.get(app.selected_idx).cloned();

    let title = match selected {
        Some(ins) => format!(" Params: {} (#{}) ", ins.name, ins.ins_n),
        None => " Params ".to_string(),
    };
    let block = Block::default()
        .borders(Borders::ALL)
        .border_style(Style::default().fg(MED_GRAY1))
        .title(title);

    let Some(ins) = selected else {
        frame.render_widget(Paragraph::new("No instrument selected").block(block), area);
        return;
    };

    let inner = block.inner(area);
    let inner_height = inner.height as usize;
    let inner_width = inner.width as usize;

    let patterned: HashSet<&str> = ins.patterned_pfs.iter().map(|s| s.as_str()).collect();
    let mut entries: Vec<(&String, &serde_json::Value)> = ins.pfields.iter().collect();
    entries.sort_by(|(a, _), (b, _)| {
        let rank = |k: &str| -> (u8, usize) {
            if patterned.contains(k) {
                (0, 0)
            } else {
                let common = ["vol", "dur", "pitch", "sample", "cf", "res"];
                if let Some(pos) = common.iter().position(|&c| c == k) {
                    (1, pos)
                } else {
                    (2, 0)
                }
            }
        };
        rank(a.as_str())
            .cmp(&rank(b.as_str()))
            .then_with(|| a.cmp(b))
    });

    let use_two_cols = entries.len() > inner_height && inner_width >= 44;
    let mut lines: Vec<Line> = Vec::new();

    if use_two_cols {
        let col_width = inner_width / 2;
        let key_w = 12usize;
        let val_w = col_width.saturating_sub(key_w + 4).max(4);
        let half = (entries.len() + 1) / 2;
        for row in 0..half {
            let mut spans = Vec::new();
            let (k1, v1) = entries[row];
            let p1 = patterned.contains(k1.as_str());
            spans.push(Span::styled(
                if p1 { "* " } else { "  " },
                Style::default().fg(SMOKY_AMETHYST),
            ));
            spans.push(Span::styled(
                format!("{:<width$}", truncate(k1, key_w), width = key_w),
                if p1 {
                    Style::default().fg(SMOKY_AMETHYST).add_modifier(Modifier::BOLD)
                } else {
                    Style::default().fg(COOL_SILVER)
                },
            ));
            spans.push(Span::styled(
                format!(" {:<width$}", truncate(&format_json_val(v1), val_w), width = val_w),
                Style::default().fg(LIGHT_GRAY2),
            ));

            if let Some((k2, v2)) = entries.get(row + half) {
                let p2 = patterned.contains(k2.as_str());
                spans.push(Span::styled(
                    if p2 { " * " } else { "   " },
                    Style::default().fg(SMOKY_AMETHYST),
                ));
                spans.push(Span::styled(
                    format!("{:<width$}", truncate(k2, key_w), width = key_w),
                    if p2 {
                        Style::default().fg(SMOKY_AMETHYST).add_modifier(Modifier::BOLD)
                    } else {
                        Style::default().fg(COOL_SILVER)
                    },
                ));
                spans.push(Span::styled(
                    format!(" {}", truncate(&format_json_val(v2), val_w)),
                    Style::default().fg(LIGHT_GRAY2),
                ));
            }
            lines.push(Line::from(spans));
        }
    } else {
        for (k, v) in entries {
            let is_pat = patterned.contains(k.as_str());
            let badge = if is_pat { "* " } else { "  " };
            let key_style = if is_pat {
                Style::default().fg(SMOKY_AMETHYST).add_modifier(Modifier::BOLD)
            } else {
                Style::default().fg(COOL_SILVER)
            };
            let val_str = format_json_val(v);
            lines.push(Line::from(vec![
                Span::styled(badge, Style::default().fg(SMOKY_AMETHYST)),
                Span::styled(format!("{:<16}", truncate(k, 16)), key_style),
                Span::styled(val_str, Style::default().fg(LIGHT_GRAY2)),
            ]));
        }
    }

    if let Some(notes) = app.recent_notes.get(&ins.name) {
        if !notes.is_empty() {
            lines.push(Line::from(Span::styled(
                "── recent notes ──",
                Style::default().fg(MED_GRAY2),
            )));
            for ev in notes.iter().rev().take(3) {
                let pitch = ev.pfields.get("pitch").map(format_json_val).unwrap_or_default();
                let vol = ev.pfields.get("vol").map(format_json_val).unwrap_or_default();
                lines.push(Line::from(Span::styled(
                    format!("  beat {:>6.2}  pitch={} vol={}", ev.beat, pitch, vol),
                    Style::default().fg(DUSTY_LILAC),
                )));
            }
        }
    }

    let max_scroll = lines.len().saturating_sub(inner_height) as u16;
    app.param_scroll = app.param_scroll.min(max_scroll);

    frame.render_widget(
        Paragraph::new(lines).scroll((app.param_scroll, 0)).block(block),
        area,
    );
}

fn draw_pty(frame: &mut Frame, app: &AppState, parser: &Arc<Mutex<vt100::Parser>>, area: Rect) {
    let scrollback = parser
        .lock()
        .map(|p| p.screen().scrollback())
        .unwrap_or(0);
    let border_style = if app.focus == Focus::Repl {
        Style::default().fg(SMOKY_AMETHYST)
    } else {
        Style::default().fg(MED_GRAY1)
    };
    let title = if scrollback > 0 {
        format!(" ghci [SCROLL +{} | type or Shift+PgDn to return] ", scrollback)
    } else {
        " ghci ".to_string()
    };
    let block = Block::default()
        .borders(Borders::ALL)
        .border_style(border_style)
        .title(title);
    let inner = block.inner(area);
    frame.render_widget(block, area);

    if let Ok(p) = parser.lock() {
        let screen = p.screen();
        let buf = frame.buffer_mut();
        for row in 0..inner.height {
            for col in 0..inner.width {
                if let Some(cell) = screen.cell(row, col) {
                    let x = inner.x + col;
                    let y = inner.y + row;
                    if let Some(buf_cell) = buf.cell_mut((x, y)) {
                        let contents = cell.contents();
                        if contents.is_empty() {
                            buf_cell.set_symbol(" ");
                        } else {
                            buf_cell.set_symbol(&contents);
                        }
                        let mut fg = vt_to_ratatui_color(cell.fgcolor());
                        let mut bg = vt_to_ratatui_color(cell.bgcolor());
                        if cell.inverse() {
                            std::mem::swap(&mut fg, &mut bg);
                        }
                        let mut mod_flags = Modifier::empty();
                        if cell.bold() {
                            mod_flags |= Modifier::BOLD;
                        }
                        if cell.italic() {
                            mod_flags |= Modifier::ITALIC;
                        }
                        if cell.underline() {
                            mod_flags |= Modifier::UNDERLINED;
                        }
                        buf_cell.set_style(Style::default().fg(fg).bg(bg).add_modifier(mod_flags));
                    }
                }
            }
        }
        if app.focus == Focus::Repl && scrollback == 0 && !screen.hide_cursor() {
            let (cr, cc) = screen.cursor_position();
            let cx = inner.x + cc.min(inner.width.saturating_sub(1));
            let cy = inner.y + cr.min(inner.height.saturating_sub(1));
            frame.set_cursor_position(Position::new(cx, cy));
        }
    }
}

fn vt_to_ratatui_color(c: vt100::Color) -> Color {
    match c {
        vt100::Color::Default => Color::Reset,
        vt100::Color::Idx(i) => Color::Indexed(i),
        vt100::Color::Rgb(r, g, b) => Color::Rgb(r, g, b),
    }
}

fn format_json_val(v: &serde_json::Value) -> String {
    match v {
        serde_json::Value::Number(n) => {
            if let Some(f) = n.as_f64() {
                if (f.fract()).abs() < 1e-6 {
                    format!("{:.0}", f)
                } else {
                    format!("{:.3}", f)
                }
            } else {
                n.to_string()
            }
        }
        serde_json::Value::String(s) => s.clone(),
        other => other.to_string(),
    }
}

fn truncate(s: &str, max: usize) -> String {
    if s.chars().count() <= max {
        s.to_string()
    } else {
        s.chars().take(max.saturating_sub(1)).collect::<String>() + "…"
    }
}
