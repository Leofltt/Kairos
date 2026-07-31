# ⏳ Kairos

**Kairos** is an interactive live-coding library for real-time electronic music composition and performance. It allows performers to manipulate synthesis and sampler parameters on the fly via the Haskell interactive compiler (GHCi REPL), routing control messages to either the **Csound** audio engine or the **Aillen** sampler/synthesizer via OSC.

Originally conceived as a thesis project at the **Berklee College of Music** (advised by Dr. Richard Boulanger), Kairos is designed for immediate, high-level interaction with synthesis parameters, avoiding the rigid limitations of traditional hardware or DAW-based interfaces.

---

## 🎹 Core Concepts

### 1. Performance Context

At the center of Kairos is the `Performance` data structure (`perf`), initialized automatically on boot. It binds together:

- The **Orchestra** (active instruments and their routing configuration).
- The **Clock** (tempo/BPM, meter/time signature, and sub-bar quantization).
- Active **TimePoint and Parameter Patterns**.

### 2. TimePoint Patterns (When to Play)

Time patterns determine the exact rhythm and beat-placement for instrument events. Kairos supports:

- **Bjorklund/Euclidean Rhythms**: Generate mathematically balanced rhythms (e.g., Euclidean pattern `(5, 8)` for a classic bossa-nova/tresillo feel).
- **Rhythmic Mininotation (Spark)**: Quick string-based patterns where `*` triggers a note, `~` is a rest, and brackets `[...]` subdivide beats (e.g., `textToTP 4 "* * [* * *] ~"`).
- **Binary & Evolutionary Rules**: Convert numbers directly to binary grids (`toBinToTP`), or programmatically evolve structures over iterations using sequence subdivision (e.g., `evolve`, `interp1`, `interp2`).

### 3. Parameter Patterns & Updaters (How to Sound)

Synthesizer and sampler arguments (p-fields) are updated per note-strike using lists paired with an **Updater Function**:

- `keep` / `k`: Hold the current value.
- `nextVal` / `nv`: Cycle sequentially through a list of values.
- `randomize` / `rnd`: Choose a value randomly from the list.
- `percentNext` / `np`: Update to the next value with a given probability percentage; otherwise, sustain the current state.
- `retrograde` / `retro`: Walk backward through the list.
- `runMarkov` / `rMkv`: Walk through values guided by a Markov probability transition matrix (provided as a list-of-lists or parsed directly from a CSV file).

---

## 🚀 Quick Start

### Prerequisites

Make sure you have the following installed:

1. **Haskell GHC / Stack / Cabal** (recommended via [GHCup](https://www.haskell.org/ghcup/))
2. **Csound** (if using the default Csound orchestra) — [Download Csound](https://csound.com/download.html)
3. **Aillen** (if using the external sampler/synth)

### Setup & Installation

1. Clone this repository and navigate into the root directory:

   ```bash
   cd Kairos
   ```

2. **Option A: Build with Cabal**

   ```bash
   cabal install --lib --package-env .
   cabal repl
   ```

3. **Option B: Build with Stack**

   ```bash
   stack install
   stack ghci
   ```

### Booting the Session

1. **Start the Audio Engine**:
   - For Csound: Run `csound Kairos.csd` in your terminal or audio editor.
   - For Aillen: Launch the Aillen engine (default OSC listening port: `8000`).
2. **Load the Live Environment**:
   In your GHCi shell, run:

   ```haskell
   :script BootKairos.hs
   ```

   This will initialize the prompt `κ>`, load your orchestra, and configure `perf` with default parameters.

---

## 🎛️ REPL Cheatsheet

### Basic Controls

- **Display Instruments**: `displayIns`

- **Change Tempo**: `cT 128` (sets BPM to 128)
- **Start Instrument**: `p "instName"`
- **Stop Instrument**: `s "instName"`
- **Solo Instrument**: `solo "instName"`
- **Silence All**: `silence`

### Pattern Management

- **List Patterns**: `displayTP`

- **Assign Time Pattern**: `cPat "sixteenN" "instName"`
- **Add custom rhythm**:

   ```haskell
   -- Creates a custom time pattern and maps it immediately to the kick
   addC "K909" "myRhythm" [TP 0.0, TP 1.5, TP 2.5]
   ```

### Parameter Mapping

You can control individual parameters via dedicated shorthand helpers or bind multiple parameters at once:

```haskell
-- Map individual parameters (instrument, values, updater)
vol "K909" [0.8, 0.6, 0.7] randomize
pan "snS" [-1.0, 1.0] nextVal
pitch "303" (withScale 30 aeolian) (runMarkov csv2)

-- Map multiple parameters simultaneously using prms
prms "hov" [
    (vol, keep, [0.4]),
    (cf, nextVal, [900, 1200, 1600]),
    (pan, randomize, [-0.8, 0.8])
]
```

*Common p-field channels include:*

- `dur`: Duration (seconds or time-signature dependent)
- `vol`: Amplitude Scaling (0 to 1)
- `rev` / `del` / `chorus`: Effect Send levels
- `pan`: Panning (-1.0 to 1.0 / 0 to 1 depending on routing)
- `pitch` / `cf` / `res`: Instrument synthesis targets

---

## 📂 Codebase Overview

- [BootKairos.hs](file:///Users/leofltt/Desktop/Kairos/BootKairos.hs) – Main initialization script mapping shorthand functions for live coding.
- [src/Kairos/Performance.hs](file:///Users/leofltt/Desktop/Kairos/src/Kairos/Performance.hs) – Main scheduler, binding instruments, tempo, and pattern states.
- [src/Kairos/Clock.hs](file:///Users/leofltt/Desktop/Kairos/src/Kairos/Clock.hs) – Manages the master tempo, meter signature, and beat-to-millisecond synchronization.
- [src/Kairos/TimePoint.hs](file:///Users/leofltt/Desktop/Kairos/src/Kairos/TimePoint.hs) – Defines rhythmic representations, default presets, and generators.
- [src/Kairos/Pfield.hs](file:///Users/leofltt/Desktop/Kairos/src/Kairos/Pfield.hs) – Core parameter representations and scaling utilities.
- [src/Kairos/Markov.hs](file:///Users/leofltt/Desktop/Kairos/src/Kairos/Markov.hs) – State machine implementation for Markov-chain probability walks.
- [src/Kairos/Euclidean.hs](file:///Users/leofltt/Desktop/Kairos/src/Kairos/Euclidean.hs) – Implementation of Bjorklund's spacing algorithm for Euclidean beats.
- [src/Kairos/Instrument.hs](file:///Users/leofltt/Desktop/Kairos/src/Kairos/Instrument.hs) – Defines MIDI, OSC, and Csound instrument structures and default orchestras.
- [src/Kairos/Network.hs](file:///Users/leofltt/Desktop/Kairos/src/Kairos/Network.hs) – Manages UDP packet delivery for real-time OSC signaling.

---

## ⚖️ License & Credits

- Developed by **Leonardo Foletto** (2019-2026).
