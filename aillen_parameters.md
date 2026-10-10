# Aillen Synthesizer Orchestration Parameter Reference

This guide provides a comprehensive reference of all Aillen parameters available for patterning via `BootKairos.hs`.

---

## Global Aillen Audio Engine Architecture

The following diagram illustrates the complete audio signal path from individual track instruments through the per-track `FxChain` inserts, master send/return effects, and the final master processing bus:

```
                            +---------------------------------------+
                            |              TRACK                    |
                            |  +---------------+                 L  |
                            |  |  Instrument   |---------------+--->|
                            |  +---------------+               | R  |
                            |                                  v    |
                            |                            +---------+|
                            |                            | FxChain ||
                            |                            +---------+|
                            |                           /     |     |
                            |            (Sends: Dly/Rev)     v     |
                            |                |     |     +---------+|
                            |                |     |     | Panner  ||
                            |                |     |     +---------+|
                            |                |     |          |     |
                            +----------------|-----|----------|-----+
                                             |     |          |
                                             v     v          v
                                         [Sends] [Sends]  [Sum Dry]
                                            |      |          |
                                            v      v          |
                                    +---------+ +---------+   |
                                    | Return  | | Return  |   |
                                    | Delay   | | Reverb  |   |
                                    +---------+ +---------+   |
                                         |           |        |
                                         |           v        |
                                         |     +---------+    |
                                         |     | HighPass|    |
                                         |     | (120 Hz)|    |
                                         |     +---------+    |
                                         |           |        |
                                         v           v        v
                                         +-----------+---> [Mixer Summing]
                                                               |
                                                               v
                                                      +-----------------+
                                                      |  Master Volume  |
                                                      +-----------------+
                                                      | Master DJFilter |
                                                      +-----------------+
                                                      | Master WaveLoss |
                                                      +-----------------+
                                                      | Master Limiter  |
                                                      +-----------------+
                                                               |
                                                               v
                                                      Stereo Output (DAC)
```

### Track FxChain Detail

Each track owns an independent `FxChain` containing a sequential arrangement of stereo processors:

```
                                     FxChain (Stereo)
                                     
                                   L Input     R Input
                                      |           |
                                      v           v
                                 +---------+ +---------+
                                 |AmRingMod| |AmRingMod| <--- Sidechain (optional)
                                 +---------+ +---------+
                                      |           |
                                      v           v
                                 +---------+ +---------+
                                 |WaveFoldr| |WaveFoldr|
                                 +---------+ +---------+
                                      |           |
                                      v           v
                                 +---------+ +---------+
                                 |Distortn | |Distortn |
                                 +---------+ +---------+
                                      |           |
                                      v           v
                                 +---------+ +---------+
                                 |Bitcrshr | |Bitcrshr |
                                 +---------+ +---------+
                                      |           |
                                      v           v
                                 +---------+ +---------+
                                 |CombFiltr| |CombFiltr|
                                 +---------+ +---------+
                                      |           |
                                      v           v
                                 +---------+ +---------+
                                 |DjFilter | |DjFilter |
                                 +---------+ +---------+
                                      |           |
                                      v           v
                                 +---------+ +---------+
                                 |Compressr| |Compressr| <--- Sidechain (optional)
                                 +---------+ +---------+
                                      |           |
                                      v           v
                                   L Output    R Output
```

---

## 1. Track & Mixer General Controls

These control basic volume, panning, muting, and sidechaining across all tracks.

### 1.1 Track Channel Strip Controls

| Haskell Function | OSC Address | Parameter Type / Range | Description |
| :--- | :--- | :--- | :--- |
| `vol i list fun` | `/track/volume` | `Double` (0.0 to 1.0+) | Track volume gain level. |
| `pan i list fun` | `/track/pan` | `Double` (-1.0 to 1.0) | Stereo panning (-1.0 = hard left, 1.0 = hard right). |
| `mute i list fun` | `/track/mute` | `Int` (0 or 1) | Mute track (`1` = muted, `0` = unmuted). |
| `sendDelay i list fun` | `/track/send/delay` | `Double` (0.0 to 1.0) | Send level to the master return delay. |
| `sendReverb i list fun`| `/track/send/reverb` | `Double` (0.0 to 1.0) | Send level to the master return reverb. |
| `sidechainSource i` | `/track/sidechain/source`| `Int` (track ID, or -1) | Route another track's output to this track's sidechain compressor. |

### 1.2 Master Mixer Controls

These apply global changes to Aillen's master output.

| Haskell Function | OSC Address | Parameter Type / Range | Description |
| :--- | :--- | :--- | :--- |
| `mixvol i list fun` | `/mixer/master/volume` | `Double` (0.0 to 1.0+) | Global master volume gain level. |
| `masterFilter i list` | `/mixer/master/filter` | `Double` (-1.0 to 1.0) | Global master DJ filter (`-1.0` = LP, `0.0` = bypass, `1.0` = HP). |
| `masterLimiterGain i` | `/mixer/master/limiter/gain` | `Double` (0.0 to 10.0+) | Master limiter input threshold gain. |
| `masterLimiterRelease`| `/mixer/master/limiter/release`| `Double` (0.01 to 1.0 sec) | Master limiter look-ahead release time. |
| `masterLimiterCeiling`| `/mixer/master/limiter/ceiling`| `Double` (0.0 to 1.0) | Master limiter hard ceiling level. |

### 1.3 Master Waveloss (Degrader) Controls

Waveloss dynamically drops sample frames on the master output to create a digital glitch/bitcrush style degradation.

| Haskell Function | OSC Address | Parameter Type / Range | Description |
| :--- | :--- | :--- | :--- |
| `wlvol i list fun` | `/mixer/master/waveloss/mode` | `Int` (`0` = Bypass, `1` = Fixed, `2` = Variable) | Waveloss degradation mode. |
| `wldrop i list fun` | `/mixer/master/waveloss/drop` | `Int` (0 to 40+) | Number of samples to drop/zero out. |
| `wlmax i list fun` | `/mixer/master/waveloss/outof` | `Int` (Window size, e.g. 40 to 1000) | Period/interval window to drop samples out of. |

### Examples
```haskell
-- Send OH808 to both return delay and reverb (50% wet)
sendDelay OH808 [0.5] (step 1)
sendReverb OH808 [0.5] (step 1)

-- Pan OH808 hard left and then hard right
pan OH808 [-1.0, 1.0] (step 2)
```

---

## 2. Return Effect Track Controls

These parameters control global master return effects tracks.

### 2.1 Master Return Delay Controls

| Haskell Function | OSC Address | Parameter Type / Range | Description |
| :--- | :--- | :--- | :--- |
| `delayMode i list` | `/mixer/return/delay/mode` | `Int` (`0` = Tape, `1` = Granular) | Delay model selection. |
| `delayPingpong i` | `/mixer/return/delay/pingpong`| `Int` (`0` = Standard, `1` = Ping-Pong) | Toggle ping-pong bouncing. |
| `delayDrive i list` | `/mixer/return/delay/drive` | `Double` (1.0 to 5.0+) | Distortion drive in the delay loop. |
| `delayGrainSize i` | `/mixer/return/delay/grain_size`| `Double` (10.0 to 500.0 ms) | Granular delay grain duration. |
| `delayDensity i` | `/mixer/return/delay/density` | `Int` (1 to 8) | Number of overlapping grains. |
| `delaySpray i list` | `/mixer/return/delay/spray` | `Double` (0.0 to 100.0 ms) | Time jitter spray for granular delay. |
| `delayPitch i list` | `/mixer/return/delay/pitch` | `Double` (0.5 to 2.0) | Granular delay pitch shift ratio. |
| `delfb i list fun` | `/mixer/return/delay/feedback`| `Double` (0.0 to 1.0) | Delay feedback level. |
| `delt i list fun` | `/mixer/return/delay/time` | `Double` (seconds, e.g. 0.1 to 2.0) | Delay time in seconds. |

### 2.2 Master Return Reverb Controls

An Elektron-style stereo Plate Reverb filtered through a dedicated 120 Hz Biquad High-Pass filter before return summing to prevent low-end mud.

| Haskell Function | OSC Address | Parameter Type / Range | Description |
| :--- | :--- | :--- | :--- |
| `reverbTime i list` <br>_or_ `revdecay i list` | `/mixer/return/reverb/decay` | `Double` (0.0 to 1.0) | Reverb decay size/time (0.0 small room, 1.0 infinite freeze). |
| `reverbTone i list` <br>_or_ `revtone i list` | `/mixer/return/reverb/tone` | `Double` (-1.0 to 1.0) | Reverb feedback tone (-1.0 dark LP damp, 1.0 bright HP cut). |

### Examples
```haskell
-- Set delay to Granular mode, pitch shifted 1 octave up
delayMode s1 [1] (step 1)
delayPitch s1 [2.0] (step 1)

-- Set master reverb decay to 80% and tone to warm/dark (-0.4)
reverbTime s1 [0.8] (step 1)
reverbTone s1 [-0.4] (step 1)
```

---

## 3. Track FX Chain Controls

Every track features a sequential FX Chain: **Ring Modulator $\rightarrow$ Wavefolder $\rightarrow$ Distortion $\rightarrow$ Bitcrusher $\rightarrow$ Comb Filter $\rightarrow$ DJ Filter $\rightarrow$ Compressor**.

### 3.1 Ring Modulator & Wavefolder

| Haskell Function | OSC Address | Parameter Type / Range | Description |
| :--- | :--- | :--- | :--- |
| `fxRingModMode i` | `/track/fx/ring_mod/mode` | `Int` (0 = Bypass, 1 = On) | Enable ring modulator. |
| `fxRingModSource i` | `/track/fx/ring_mod/source` | `Int` (0 = Sine, 1 = Self, 2 = Sidechain) | Modulation source. |
| `fxRingModDepth i` | `/track/fx/ring_mod/depth` | `Double` (0.0 to 1.0) | Ring modulation depth (amount). |
| `fxRingModFreq i` | `/track/fx/ring_mod/freq` | `Double` (Hz, e.g., 20 to 5000) | Modulation frequency in Hz. |
| `fxWfDrive i list` | `/track/fx/wavefolder/drive` | `Double` (1.0 to 10.0) | Wavefolder input drive gain multiplier. |
| `fxWfFolds i list` | `/track/fx/wavefolder/folds` | `Double` (0.0 to 1.0) | Folding intensity (0.0 is bypass). |
| `fxWfSymmetry i` | `/track/fx/wavefolder/symmetry` | `Double` (-1.0 to 1.0) | DC offset asymmetry shift before folding. |

### 3.2 Distortion & Bitcrusher

| Haskell Function | OSC Address | Parameter Type / Range | Description |
| :--- | :--- | :--- | :--- |
| `fxDistMode i list` | `/track/fx/distortion/mode` | `Int` (0 to 3) | Distortion clipping mode (see table below). |
| `fxDistDrive i list` | `/track/fx/distortion/drive` | `Double` (1.0 to 10.0+) | Distortion input drive level. |
| `fxDistMix i list` | `/track/fx/distortion/mix` | `Double` (0.0 to 1.0) | Distortion wet/dry mix. |
| `fxBcBits i list` | `/track/fx/bitcrusher/bits` | `Double` (1.0 to 16.0) | Quantization bit depth (16.0 = bypass). |
| `fxBcDownsample` | `/track/fx/bitcrusher/downsample`| `Int` (1 to 100+) | Downsampling divider (1 = bypass). |

#### Distortion Modes (`fxDistMode`, `hubassDriveMode`, `swaveDrive`)
- `0` = **Bypass**: Signal passes through uncolored.
- `1` = **Tanh**: Warm analog hyperbolic tangent soft-clipping (`tanh(x)`). Smooth compression that progressively saturates without harsh edges.
- `2` = **HardClip**: Strict brickwall clipping at threshold boundaries (`clamp(-1.0, 1.0)`). Generates rich, aggressive odd-harmonic distortion.
- `3` = **Wavefold / Foldback**: Sinusoidal folding (`sin(x * π/2)`). Waves exceeding threshold are folded backward, creating complex metallic, glassy, resonant harmonic overtones.

### 3.3 Comb Filter, DJ Filter & Compressor

| Haskell Function | OSC Address | Parameter Type / Range | Description |
| :--- | :--- | :--- | :--- |
| `fxCombFreq i list` | `/track/fx/comb/freq` | `Double` (20.0 to 10000.0) | Tuned comb filter frequency in Hz. |
| `fxCombFeedback` | `/track/fx/comb/feedback` | `Double` (-0.99 to 0.99) | Feedback intensity / resonance decay. |
| `fxCombDamp i list` | `/track/fx/comb/damp` | `Double` (cutoff Hz) | Loop lowpass dampening cutoff frequency. |
| `fxFilter i list` | `/track/fx/filter/position` | `Double` (-1.0 to 1.0) | DJ filter (`-1.0` = LP, `0.0` = bypass, `1.0` = HP). |
| `fxCompRatio i list` | `/track/fx/compressor/ratio` | `Double` (1.0 to 20.0) | Compressor ratio. |
| `fxCompThreshold i`| `/track/fx/compressor/threshold`| `Double` (-60.0 to 0.0 dB) | Threshold level. |
| `fxCompAttack i list`| `/track/fx/compressor/attack` | `Double` (0.001 to 0.1 sec) | Attack time. |
| `fxCompRelease i` | `/track/fx/compressor/release` | `Double` (0.01 to 1.0 sec) | Release time. |
| `fxCompMakeup i` | `/track/fx/compressor/makeup` | `Double` (makeup gain in dB) | Makeup gain. |
| `fxCompSidechain i`| `/track/fx/compressor/sidechain`| `Int` (0 = Self, 1 = External) | Enable sidechain compression. |

### Examples
```haskell
-- Wavefold a drum track for glassy saturation
fxWfFolds OH808 [0.6] (step 1)
fxWfDrive OH808 [2.0] (step 1)

-- Tuned metallic/physical comb filter sweep
fxCombFeedback s1 [0.85] (step 1)
fxCombFreq s1 [220, 330, 440, 660] (step 4)
```

---

## 4. Sampler Specific Parameters (Tracks 1, 2, 3, 5)

These control playback, granular time-stretch, and slice options for the Sampler engine.

### Sampler Voice Architecture

```
  +---------------------------------------------------------------------------------+
  |                             Audio Sample Buffer                                 |
  +---------------------------------------------------------------------------------+
                                           |
                                           v
  +---------------------------------------------------------------------------------+
  | Playback Engine:                                                                |
  |  - Mode: OneShot / Loop                                                         |
  |  - Time-Stretch: Resample (pitch+speed coupled) OR Granular Time-Stretch       |
  |    [Granular: grain_size_ms (10..150ms), overlap count (2..8)]                  |
  |  - Slice Engine: num_slices (2..64), selected_slice (0..N-1), stutter_count     |
  +---------------------------------------------------------------------------------+
                                           |
                                    Polyphonic Sum
                                           |
                                           v
                              +-------------------------+
                              | Headroom Gain Normalizer|
                              +-------------------------+
                                           |
                                           v
                              +-------------------------+
                              | Stereo DJ Filter (L/R)  |
                              | (-1.0 LP <-> +1.0 HP)   |
                              +-------------------------+
                                           |
                                           v
                                   Stereo Output -> Track FxChain
```

| Haskell Function | OSC Address | Parameter Type / Range | Description |
| :--- | :--- | :--- | :--- |
| `sampleSelect i list`| `/track/sample/select` | `String` (sample name) | Selects a sample by short relative name/index from the preloaded bank. |
| `sampleMode i list` | `/track/sample/mode` | `Int` (0 = OneShot, 1 = Loop) | Playback loop toggle. |
| `samplePitch i list` | `/track/sample/pitch` | `Double` (0.1 to 4.0) | Pitch playback ratio (`1.0` = normal, `2.0` = double pitch). |
| `sampleSpeed i list` | `/track/sample/speed` | `Double` (0.1 to 4.0) | Speed playback ratio (`1.0` = normal, `0.5` = half speed). |
| `sampleStretch i` | `/track/sample/mode/stretch`| `Int` (0 = Resample, 1 = Granular) | Decouple speed and pitch using granular time-stretching. |
| `sampleGrainSize i` | `/track/sample/grain_size` | `Double` (10.0 to 150.0 ms) | Grain duration for time-stretching. |
| `sampleOverlap i` | `/track/sample/overlap` | `Int` (2 to 8) | Grain overlap count. |
| `sampleFilter i list`| `/track/filter` | `Double` (-1.0 to 1.0) | Sampler internal DJ filter. |
| `aillenSliceMode i` | `/track/sample/slice/mode` | `Int` (0 = Off, 1 = On) | Enable slice mode playback. |
| `aillenSliceCount i`| `/track/sample/slice/count`| `Int` (2 to 64) | Divide sample into equal slice count. |
| `aillenSliceSelect i`| `/track/sample/slice/select`| `Int` (0 to count-1) | Playback targeted slice index. |
| `aillenSliceStutter` | `/track/sample/slice/stutter`| `Int` (1 to 16) | Repeat slice count times (stutter effect). |

---

## 5. Two-Operator Synth Specific Parameters (Track 0)

A versatile FM/AM/RingMod synth engine with phase modulation, wavefolding, noise injection, and dual filtering options.

### Two-Operator Synth Architecture

```
  Pitch Sweep Env ----+
  Pitch LFO ----------+---> Base Pitch (Hz)
                             |
         +-------------------+--------------------+
         |                                        |
         v (x Ratio + Detune)                     v
  +--------------------------------+      +--------------------------------+
  | Operator 2 (Modulator)         |      | Operator 1 (Carrier)           |
  |  - Waveform (Sine/Saw/Sq/Tri)  |      |  - Waveform (Sine/Saw/Sq/Tri)  |
  |  - Self Feedback (osc2_fb)     |      |  - Self Feedback (osc1_fb)     |
  |  - Modulator Phase Noise       |      |  - Carrier Phase Noise         |
  +--------------------------------+      +--------------------------------+
                 |                                       |
                 v                                       |
        [x Op2 ADSR Env]                                 |
                 |                                       |
                 v                                       |
      +--------------------+                             |
      | Wavefolder         |                             |
      | (Diode Reflection) |                             |
      +--------------------+                             |
                 |                                       |
                 v                                       |
        [x Mod Index (LFO)]                              |
                 |                                       |
                 +-------------------+                   |
                                     |                   |
                                     v                   v
                        +---------------------------------------+
                        | Synthesis Mode Interaction:           |
                        |  - 0: Additive: (Op1*Env1 + Op2)/2    |
                        |  - 1: AM:       Op1 * (1 + Op2) * Env1|
                        |  - 2: RM:       Op1 * Op2 * Env1      |
                        |  - 3: FM (PM):  Op1(Phase + Op2)*Env1 |
                        +---------------------------------------+
                                            |
                                            v
                        +---------------------------------------+
                        | Filter Stage (Selected via Switch):   |
                        |  Mode A: Standard Biquad Filter       |
                        |          (LP, HP, BP, Notch)          |
                        |  Mode B: Monomachine Base & Width     |
                        |          (Serial Dual HP -> LP)       |
                        |  Modulation: Filter ADSR + Cutoff LFO |
                        +---------------------------------------+
                                            |
                                            v
                                 Output -> Track FxChain
```

| Haskell Function | OSC Address | Parameter Type / Range | Description |
| :--- | :--- | :--- | :--- |
| `realtime i list` | `/track/realtime` | `Int` (0 or 1) | Update parameters on currently sounding voices. |
| `legato i list` | `/track/legato` | `Int` (0 or 1) | Glide pitch from note to note without retriggering envelopes. |
| `twopMode i list` | `/track/mode` | `Int` (0 to 3) | Synthesis mode: `0`=Additive (sum), `1`=AM (amplitude mod), `2`=RM (ring mod), `3`=FM (phase mod). |
| `twopOsc1Waveform i`| `/track/osc1/waveform` | `Int` (0 to 3) | Carrier waveform: `0`=Sine, `1`=Saw, `2`=Square, `3`=Triangle. |
| `twopOsc2Waveform i`| `/track/osc2/waveform` | `Int` (0 to 3) | Modulator waveform: `0`=Sine, `1`=Saw, `2`=Square, `3`=Triangle. |
| `twopModParams i` | `/track/mod/params` | `[index, ratio, detune]` | Modulator synthesis parameters: index (`0.0..20.0`), ratio multiplier (`0.1..32.0`), detune Hz (`-10.0..10.0`). |
| `twopOsc1Adsr i` | `/track/osc1/adsr` | `[A, D, S, R]` (seconds, sustain 0..1) | Carrier amplitude ADSR envelope. |
| `twopOsc2Adsr i` | `/track/osc2/adsr` | `[A, D, S, R]` | Modulator ADSR envelope (shapes FM index / AM depth dynamically). |
| `twopFilterAdsr i` | `/track/filter/adsr` | `[A, D, S, R]` | Filter cutoff modulation ADSR envelope. |
| `twopFilterParams i`| `/track/filter/params` | `[cutoff, resonance, type (0..3)]` | Base biquad filter settings: Cutoff Hz (`20..20000`), Q factor (`0.1..10+`), Filter type: `0`=LowPass, `1`=HighPass, `2`=BandPass, `3`=Notch. |
| `twopFilterMod i` | `/track/filter/mod` | `[enabled (0/1), depth_hz]` | Filter envelope modulation depth (-20000.0 to 20000.0 Hz). |
| `twopFeedback i` | `/track/feedback` | `Double` (0.0 to 1.0) | Modulator (Operator 2) phase self-feedback intensity (morphs sine toward saw/noise). |
| `twopFeedback1 i` | `/track/feedback1` | `Double` (0.0 to 1.0) | Carrier (Operator 1) phase self-feedback intensity. |
| `twopRatioQuantize i` | `/track/ratio/quantize` | `[enabled (0/1), ratio_index (0..16)]` | Monomachine FM+STATIC style quantized harmonic ratio mode. |
| `twopFilterBaseWidth i` | `/track/filter/basewidth` | `[enabled (0/1), base_hz, width_hz, hp_q]` | Monomachine-inspired serial Base & Width dual HP/LP filter. |
| `twopWavefold i` | `/track/wavefold` | `[gain, mix]` | Modulator wavefolder input gain (1.0 to 10.0) and wet/dry mix (0.0 to 1.0). |
| `twopNoise i list` | `/track/noise` | `[carrier_noise, modulator_noise]` | Random phase noise injection levels (0.0 to 1.0). |
| `twopPitchSweep i` | `/track/pitch/sweep` | `[depth_semitones, decay_sec]` | Pitch envelope sweep (-48.0 to 48.0 semitones, 0.001 to 5.0s decay). |
| `twopLfo i list` | `/track/lfo` | `[wave (0..4), speed_hz, mod, cut]`| Voice LFO routing: waveform (`0`=Sine, `1`=Tri, `2`=Saw, `3`=Square, `4`=S&H), speed Hz, mod index depth, cutoff depth Hz. |

### Synthesis Modes (`twopMode`)
- `0` = **Additive**: Simply sums Operator 1 and Operator 2: `sample = 0.5 * op1 + 0.5 * op2`.
- `1` = **AM (Amplitude Modulation)**: Operator 2 modulates Operator 1 amplitude: `sample = op1 * (1.0 + op2 * index)`.
- `2` = **RM (Ring Modulation)**: Operator 1 and Operator 2 are multiplied: `sample = op1 * op2 * index`.
- `3` = **FM (Phase Modulation)**: Operator 2 phase-modulates Operator 1: `op1_phase = osc1_phase + carrier_fb + op2 * index + noise`.

### Filter Types (`twopFilterParams` type argument)
- `0` = **LowPass**: Attenuates high frequencies above cutoff.
- `1` = **HighPass**: Attenuates low frequencies below cutoff.
- `2` = **BandPass**: Passes only frequencies within bandwidth around cutoff.
- `3` = **Notch**: Rejects frequencies within a narrow band around cutoff.

### Quantized Harmonic Ratios (`twopRatioQuantize`)
When enabled (`1`), `ratio_index` selects from the Monomachine FM+STATIC harmonic interval table:
- `0`: 1/8 (`0.125`)
- `1`: 1/4 (`0.25`)
- `2`: 1/2 (`0.5`)
- `3`: 3/4 (`0.75`)
- `4`: 1 (`1.0`)
- `5`: 5/4 (`1.25`)
- `6`: 3/2 (`1.5`)
- `7`: 2 (`2.0`)
- `8`: 5/2 (`2.5`)
- `9`: 3 (`3.0`)
- `10`: 7/2 (`3.5`)
- `11`: 4 (`4.0`)
- `12`: 5 (`5.0`)
- `13`: 6 (`6.0`)
- `14`: 7 (`7.0`)
- `15`: 8 (`8.0`)
- `16`: 12 (`12.0`)

### Base & Width Filter (`twopFilterBaseWidth`)
When enabled (`[1, base, width, hp_q]`), replaces the standard biquad with the Monomachine-style serial dual HP/LP filter:
- High-Pass cutoff = `base_hz` (10 to 20000 Hz)
- Low-Pass cutoff = `base_hz + width_hz` (preserves bandwidth as `base` sweeps across the spectrum)
- Filter envelope modulation and LFO cutoff modulation modulate `base_hz` directly.

### Examples
```haskell
-- Metallic FM bell with carrier & modulator feedback
twopMode "fm1" [3] keep
twopModParams "fm1" [[4.0, 1.0, 0.0]] keep
twopFeedback "fm1" [0.35] keep   -- Modulator feedback
twopFeedback1 "fm1" [0.15] keep  -- Carrier feedback

-- Quantized musical ratio (e.g. index 7 = 2.0 octave multiplier)
twopRatioQuantize "fm1" [[1, 7]] keep

-- Serial Base & Width filtering (HP at 200 Hz, LP at 4200 Hz)
twopFilterBaseWidth "fm1" [[1, 200.0, 4000.0, 1.2]] keep
```

---

## 6. SynthResonator (Karplus-Strong & Modal Resonator) Specific Parameters (Track 4 - `kp`)

A physical modeling string and modal synthesizer with a noise exciter, fractional delay line, parallel modal resonator, and internal distortion inside the feedback loop.

### SynthResonator Architecture

```
  +-------------------------------------------------------+
  | Noise Generator PRNG                                  |
  +-------------------------------------------------------+
                             |
                             v
              +-----------------------------+
              | Exciter LowPass Filter      | <--- exciter_cutoff (Hz)
              +-----------------------------+
                             |
                             v
                   [x Exciter ADSR Env]
                             |
                   Exciter Impulse Burst
                             |
                             v
           +-----------------+-----------------------+
           |                                         |
           v                                         |
  +-----------------------------+                    |
  | Karplus-Strong Delay Line   | <--- Pitch Hz      |
  | (Fractional Linear Interp)  |                    |
  +-----------------------------+                    |
                 |                                   |
                 v                                   |
  +-----------------------------+                    |
  | Wavefolder (bend_drive/fold)|                    |
  +-----------------------------+                    |
                 |                                   |
                 v                                   |
  +-----------------------------+                    |
  | Bitcrusher (bend_bits)      |                    |
  +-----------------------------+                    |
                 |                                   |
                 v                                   |
  +-----------------------------+                    |
  | Loop Dampening LowPass      | <--- dampening (Hz)|
  +-----------------------------+                    |
                 |                                   |
                 v                                   |
        [x Feedback Gain]                            |
                 |                                   |
                 v                                   |
            ( + Sum ) <------------------------------+
                 |
                 v
        [Soft Clip Tanh()]
                 |
                 +-------------------+ (Loop Feedback to Delay Line)
                 |
                 +-----------------------------------+
                 |                                   |
                 v                                   v
        (1.0 - modal_mix)                     [modal_mix]
                 |                                   |
                 |                                   v
                 |                    +-------------------------------+
                 |                    | Parallel Modal Resonator      |
                 |                    | (BPF at Pitch * modal_ratio)  |
                 |                    +-------------------------------+
                 |                                   |
                 +-----------------+-----------------+
                                   |
                                   v
                        Output -> Track FxChain
```

| Haskell Function | OSC Address | Parameter Type / Range | Description |
| :--- | :--- | :--- | :--- |
| `exciterAdsr i list` | `/track/exciter/adsr` | `[A, D, S, R]` (seconds, sustain 0..1) | Exciter noise burst ADSR parameters. A/D/R: `0.001` to `10.0`s. S: `0.0` to `1.0`. |
| `exciterCutoff i list`| `/track/exciter/cutoff` | `Double` (20.0 to 20000.0 Hz) | Exciter noise burst lowpass cutoff frequency in Hz. |
| `resFeedback i list` | `/track/feedback` | `Double` (0.0 to 0.999) | Delay line decay feedback ratio. |
| `resDampening i list` | `/track/dampening` | `Double` (200.0 to 20000.0 Hz) | Feedback loop lowpass dampening cutoff frequency in Hz. |
| `bendDrive i list` | `/track/bend/drive` | `Double` (1.0 to 10.0) | Wavefolder input drive multiplier inside feedback loop. |
| `bendFolds i list` | `/track/bend/folds` | `Double` (0.0 to 5.0) | Wavefolder intensity inside feedback loop (0.0 is clean). |
| `bendBits i list` | `/track/bend/bits` | `Double` (1.0 to 16.0) | Bitcrusher quantization bits inside feedback loop (16.0 is bypass). |
| `modalRatio i list` | `/track/modal/ratio` | `Double` (0.5 to 8.0) | Detune ratio multiplier for parallel modal resonator filter. |
| `modalMix i list` | `/track/modal/mix` | `Double` (0.0 to 1.0) | Resonator mix (0.0 is pure string, 1.0 is pure modal). |

---

## 7. Synth303 Specific Parameters (Track 6)

A classic 303 bassline synth engine with band-limited PolyBLEP oscillators, analog exponential RC envelopes, and authentic Roland TB-303 accent dynamics. Notes triggered with velocity `> 0.7` engage the accent circuit, boosting filter cutoff envelope modulation, resonance, volume, and driving the non-linear saturation stage.

### Synth303 Architecture

```
  Pitch Envelope (Exponential RC) ----+
  Portamento Glide (glide_time) ------+---> Final Oscillator Pitch (Hz)
                                            |
                         +------------------+
                         |
                         v
  +-------------------------------------------------------+
  | Band-Limited PolyBLEP Oscillator                      |
  |  - Waveform: Saw / Square / Sine / Tri               |
  |  - PWM: Pulse-Width LFO (pwm_rate, pwm_depth)         |
  +-------------------------------------------------------+
                             |
                             v
  +-------------------------------------------------------+
  | 4-Pole 24dB Resonant Diode/Ladder Filter (ZDF)        |
  |  - Cutoff: base_cutoff + (filter_env * depth)         |
  |    + [Accent Boost: +2500 Hz * filter_env]            |
  |  - Resonance: base_res + [Accent Boost: +0.15]        |
  +-------------------------------------------------------+
                             |
                             v
  +-------------------------------------------------------+
  | Amplitude Stage (Exponential RC Amp ADSR)             |
  |  - Signal * amp_env * [Accent Gain: 1.0 + acc*0.35]   |
  +-------------------------------------------------------+
                             |
                             v
  +-------------------------------------------------------+
  | Asymmetric Liquid Saturation                          |
  |  - Drive = Signal * (3.5 + accent * 1.5)              |
  |  - Non-linear Warm Shaping: tanh(drive) * 0.85        |
  +-------------------------------------------------------+
                             |
                             v
                   Output -> Track FxChain
```

| Haskell Function | OSC Address | Parameter Type / Range | Description |
| :--- | :--- | :--- | :--- |
| `waveform303 i` | `/track/6/303/waveform` | `Int` (0=Sine, 1=Saw, 2=Sq, 3=Tri) | Main oscillator waveform. |
| `ampAdsr303 i` | `/track/6/303/amp/adsr` | `[A, D, S, R]` | Amplitude envelope. |
| `filterAdsr303 i` | `/track/6/303/filter/adsr` | `[A, D, S, R]` | Filter cutoff envelope. |
| `pitchAdsr303 i` | `/track/6/303/pitch/adsr` | `[A, D, S, R]` | Pitch envelope. |
| `filter303 i list` | `/track/6/303/filter/params` | `[cutoff, resonance]` | Filter cutoff and Q settings. |
| `filterMod303 i` | `/track/6/303/filter/mod` | `Double` (Hz envelope depth) | Cutoff env modulation depth. |
| `pitchMod303 i` | `/track/6/303/pitch/mod` | `Double` (Hz envelope depth) | Pitch env modulation depth. |
| `pwm303 i list` | `/track/6/303/pwm/params` | `[width, speed_hz, depth]` | Pulse-width modulation settings. |
| `glide303 i list` | `/track/6/303/glide` | `Double` (seconds) | Portamento glide duration. |
| `legato303 i list` | `/track/6/303/legato` | `Int` (0 or 1) | Legato glide mode. |

---

## 8. SynthHubass Specific Parameters (Track 7)

A stereo hyper-bass engine with unison, sub-oscillator, multi-mode filtering, drive saturation, and stereo chorus.

### SynthHubass Architecture

```
  Portamento Glide (0.05s) ---+
  LFO 1 Pitch Mod ------------+---> Oscillator Pitch (Hz)
                                          |
         +--------------------------------+--------------------------------+
         |                                                                 |
         v                                                                 v
  +--------------------------------+                             +--------------------+
  | Stereo Unison Engine           |                             | Mono Sub-Oscillator|
  |  - Voices (1..7)               |                             |  - Waveform (0..2) |
  |  - Detune (0.0..0.2)           |                             |  - Octave (-1 / -2)|
  |  - Stereo Spread (0.0..1.0)    |                             |  - Gain (0.0..2.0) |
  |  - Waveform (Saw/Square/Tri)   |                             +--------------------+
  +--------------------------------+                                       |
          | L            | R                                               |
          v              v                                                 |
       ( + )          ( + ) <--- White Noise Generator                     |
         |              |                                                  |
         +-------+------+                                                  |
                 |                                                         |
                 v Stereo                                                  |
  +-------------------------------------------------------+                |
  | Multi-Mode Stereo Filter:                             |                |
  |  - Mode 0: ZDF Ladder LowPass (24 dB)                 |                |
  |  - Mode 1: ZDF Ladder BandPass                        |                |
  |  - Mode 2: Formant Vowel Morph Filter                 |                |
  |  - Modulation: Filter Cutoff Decay Env + LFO 1 Cutoff |                |
  +-------------------------------------------------------+                |
                 | Stereo                                                  |
                 v                                                         |
  +-------------------------------------------------------+                |
  | Stereo Drive / Saturation:                            |                |
  |  - Mode: Bypass / Tanh / HardClip / Wavefold          |                |
  |  - Drive Gain (0.0..10.0), Mix (0.0..1.0)             |                |
  +-------------------------------------------------------+                |
                 | Stereo                                                  |
                 v                                                         |
       [x Amp ADSR Envelope]                                               |
       [x Master Output Gain]                                              |
                 |                                                         |
                 v Mid/High Stereo                                         |
  +-------------------------------------------------------+                |
  | Stereo Chorus Delay:                                  |                |
  |  - Variable Delay Lines L & R with Jitter LFOs        |                |
  |  - Chorus Mix & Modulation Depth                      |                |
  +-------------------------------------------------------+                |
                 | L            | R                                        |
                 v              v                                          |
              ( + )          ( + ) <---------------------------------------+ (Clean Sub Sum)
                 |              |
                 +-------+------+
                         |
                         v
               Stereo Output -> Track FxChain
```

| Haskell Function | OSC Address | Parameter Type / Range | Description |
| :--- | :--- | :--- | :--- |
| `ampAdsrHubass i` | `/track/7/hubass/amp/adsr` | `[A, D, S, R]` | Amplitude ADSR envelope parameters. |
| `hubassFilterParams`| `/track/7/hubass/filter/params` | `[start_mult, end_cf, dec, res]` | Cutoff envelope: start multiplier (`0.1..10.0`), end cutoff Hz (`20..20000`), decay seconds, resonance (`0.0..0.99`). |
| `unison i list` | `/track/7/hubass/osc/unison` | `[wave (0..2), detune, spread, voices]` | Detuned unison: waveform (`0`=Saw, `1`=Square, `2`=Triangle), detune (`0.0..0.2`), stereo spread (`0.0..1.0`), voices (`1..7`). |
| `subHubass i list` | `/track/7/hubass/osc/sub` | `[wave (0..2), octave (-1/-2), gain]` | Sub-oscillator: waveform (`0`=Sine, `1`=Triangle, `2`=Square), octave offset (`-1` or `-2`), gain (`0.0..2.0`). |
| `noiseHubass i` | `/track/7/hubass/osc/noise` | `Double` (0.0 to 1.0) | White noise generator gain level. |
| `hubassFilterMode i`| `/track/7/hubass/filter/mode` | `Int` (0 to 2) | Filter mode: `0`=ZDF Ladder LowPass, `1`=ZDF Ladder BandPass, `2`=Formant vowel morph filter. |
| `hubassDriveMode i`| `/track/7/hubass/drive/mode` | `[mode (0..3), gain, mix]` | Drive saturation: mode (`0`=Bypass, `1`=Tanh, `2`=HardClip, `3`=Wavefold), gain (`0.0..10.0`), mix (`0.0..1.0`). |
| `hubassLfo1 i` | `/track/7/hubass/lfo/1` | `[wave (0..4), speed_hz, cutoff, pitch]` | LFO 1: wave (`0`=Sine, `1`=Tri, `2`=Saw, `3`=Square, `4`=S&H), speed Hz, cutoff depth Hz, pitch depth (semitones). |
| `chorusHubass i` | `/track/7/hubass/chorus/params` | `[mix, depth]` | Stereo chorus wet mix (`0.0..1.0`) and modulation depth (`0.0..1.0`). |
| `legatoHubass i` | `/track/7/hubass/legato` | `Int` (0 or 1) | Legato slide toggle (`0`=Off, `1`=On). |
| `gainHubass i list`| `/track/7/hubass/gain` | `Double` (0.0 to 5.0) | Main Hubass output channel gain factor. |

---

## 9. SWAVE Synth Specific Parameters (Track 8 - `swave`)

Inspired by the Elektron Monomachine SuperWave architecture (`SWAVE-SAW` and `SWAVE-ENS`), featuring a 5-oscillator PolyBLEP anti-aliased detuned saw cluster with configurable inner & outer unison pairs, a dedicated sub-oscillator, chord ensemble interval modes, Monomachine Base & Width dual HP/LP serial filtering, and integrated saturation drive.

### SWAVE Synth Architecture

```
  Fundamental Note Frequency (Hz)
                  |
         +--------+-------------------------------------------------+
         |                                                          |
         | Mode 0: SWAVE-SAW (5-Saw Cluster)                        | Mode 1: SWAVE-ENS (Ensemble Chords)
         v                                                          v
  +---------------------------------------+              +---------------------------------------+
  | Osc 0: Center Base Freq (Pan: Center) |              | Osc 0: Root Pitch (Pan: Center)       |
  | Osc 1: +1x Detune UNID (Pan: -0.5*Spr)|              | Osc 1: Root + Interval 1 (Pan: -0.8)  |
  | Osc 2: -1x Detune UNID (Pan: +0.5*Spr)|              | Osc 2: Root + Interval 2 (Pan: +0.8)  |
  | Osc 3: +2x Detune UNIX (Pan: -1.0*Spr)|              | Osc 3: Root + Interval 3 (Pan: Center)|
  | Osc 4: -2x Detune UNIX (Pan: +1.0*Spr)|              | (PolyBLEP Oscillators)                |
  +---------------------------------------+              +---------------------------------------+
                     | L           | R                                      | L           | R
                     +------+------+                                        +------+------+
                            |                                                      |
                            +--------------------------+---------------------------+
                                                       |
                                                       v Stereo
                                   +---------------------------------------+
                                   | Stereo Oscillator Cluster Sum         |
                                   +---------------------------------------+
                                           | L                   | R
                                           v                     v
                                        ( + )                 ( + )
                                           ^                     ^
                                           |                     |
                                   +---------------------------------------+
                                   | Sub-Oscillator (Square/Sine/Saw/Tri)  |
                                   | - Octave: -1 or -2, Level: SUBD       |
                                   | - Solid Center Mono Punch (both L & R)|
                                   +---------------------------------------+
                                                       |
                                                       v Stereo
                                            [x Amp ADSR Envelope]
                                                       |
                                                       v Stereo
                                   +---------------------------------------+
                                   | Monomachine Base & Width Filter:      |
                                   |  - High-Pass Filter (Base Hz)         |
                                   |  - Low-Pass Filter (Base + Width Hz)  |
                                   |  - Modulation: Filter ADSR onto Base  |
                                   +---------------------------------------+
                                                       |
                                                       v Stereo
                                   +---------------------------------------+
                                   | Stereo Saturation Drive:              |
                                   |  - Mode: Bypass/Tanh/HardClip/Wavefold|
                                   |  - Gain (1..10x), Mix (0.0..1.0)      |
                                   +---------------------------------------+
                                                       |
                                                       v
                                            Stereo Output -> Track FxChain
```

| Haskell Function | OSC Address | Parameter Type / Range | Description |
| :--- | :--- | :--- | :--- |
| `swaveMode i list` | `/track/swave/mode` | `Int` (0 or 1) | Synthesis mode: `0` = SuperWave Saw (5-saw cluster + sub), `1` = SuperWave Ensemble (4-voice chord / intervals + sub). |
| `swaveWaveform i list` | `/track/swave/waveform` | `Int` (0 to 3) | Base oscillator waveform: `0`=Sine, `1`=Saw, `2`=Square, `3`=Triangle. |
| `swaveUnison i list` | `/track/swave/unison` | `[detune, inner_level, outer_level, stereo_spread]` | Unison stack parameters: detune (`0.0..0.1`), inner pair level UNID (`0.0..1.0`), outer pair level UNIX (`0.0..1.0`), stereo spread (`0.0` mono to `1.0` wide stereo). |
| `swaveEnsemble i list` | `/track/swave/ensemble` | `[semitone2, semitone3, semitone4, level]` or `Chord` | Ensemble mode chord voicings: semitone offsets relative to root note (e.g. `[3.0, 7.0, 10.0]` or simply `min7`, `maj7`, `sus4`, etc. from `Kairos.Chords`), voice level (`0.0..1.0`, defaults to `0.8` when passing a 3-interval chord or can use `withGain level chord`). |
| `swaveSub i list` | `/track/swave/sub` | `[waveform (0..3), octave (-1/-2), gain]` | Sub-oscillator: waveform (`0`=Square, `1`=Sine, `2`=Saw, `3`=Triangle), octave offset (`-1` or `-2`), gain SUBD (`0.0..1.0`). |
| `swaveFilter i list` | `/track/swave/filter` | `[base_hz, width_hz, hp_q, lp_q, env_amount]` | Monomachine Base & Width filter: Base HP cutoff (`10..20000 Hz`), Width span (`10..20000 Hz`), HP Q (`0.1..10.0`), LP Q (`0.1..10.0`), envelope modulation amount Hz (`-20000..20000`). |
| `swaveAmpAdsr i list` | `/track/swave/amp/adsr` | `[A, D, S, R]` | Amplitude ADSR envelope parameters (seconds, sustain `0.0..1.0`). |
| `swaveFilterAdsr i list` | `/track/swave/filter/adsr` | `[A, D, S, R]` | Filter cutoff modulation ADSR envelope parameters. |
| `swaveDrive i list` | `/track/swave/drive` | `[mode (0..3), gain, mix]` | Saturation drive config: mode (`0`=Bypass, `1`=Tanh, `2`=HardClip, `3`=Foldback/Wavefold), gain (`1.0..10.0`), mix (`0.0..1.0`). |

### SWAVE Architecture Details
- **SuperWave Saw Mode (`mode = 0`)**: Inspired by `SWAVE-SAW`. Generates a central base oscillator plus two symmetrically detuned pairs: an inner pair (attenuated by `inner_level`) and an outer pair (attenuated by `outer_level`). `stereo_spread` pans the pairs across the stereo field.
- **SuperWave Ensemble Mode (`mode = 1`)**: Inspired by `SWAVE-ENS`. Turns the 5-oscillator cluster into a 4-voice polyphonic chord generator where voices 2, 3, and 4 track pitch intervals defined in semitones by `swaveEnsemble`.
- **Sub-Oscillator**: Independent PolyBLEP oscillator running 1 or 2 octaves below the fundamental frequency for massive low-end reinforcement.
- **Base & Width Filtering**: Serial High-Pass $\rightarrow$ Low-Pass topology. Sweeping `base_hz` shifts the entire passband while maintaining constant musical bandwidth `width_hz`. Envelope modulation modulates `base_hz`.

