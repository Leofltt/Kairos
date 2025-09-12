;KAIROS.csd

;the audio engine for the kairos live coding library

;Leonardo Foletto, 2018-2022

<CsoundSynthesizer>
<CsOptions>
-odac1
--port=11000
-d
-B 512 
-b 256 
--opcode-lib=~/Users/$USER/Library/csound/7.0/plugins64


</CsOptions>
<CsInstruments>

sr = 48000
ksmps = 32
nchnls = 2
0dbfs = 1.0

zakinit 50,50
; channels (L,R):
; 1, 2 : mix
; 3, 4 : rev in
; 5, 6 : rev out
; 7, 8 : del in
; 9, 10 : del out
; 11, 12 : chorus in
; 13, 14 : chorus out
; 15, 16 : SideChain RingMod in (TODO)
; 17, 18 : Sidechain Comp in (TODO)

; common p-fields :
; p4 : amplitude (0 - 1)
; p5 : reverb send (0 - 1)
; p6 : delay send (0 - 1)
; p7 : panning (0 - 1)
; p8 : chorus (0 - 1)
; p  : distortion d/w (0 - 1)
; p  : ring mod d/w (0 - 1)
; p  : ring mod sidechain send (0 - 1)
; p  : compressor sidechain send (0 - 1)
; p  :

; kDistMix, kDistPregain, kDistPostgain, kDistShape1, kDistShape2,  
; kRingModMix, kRingModGain, iRingModSource, iRingModWavetable, kRingModFreq, 
; kDjFilterMix, kFilterLpFreq, kFilterLpRes, kFilterHpFreq, kFilterHpRes 

; TABLES

gisine   ftgen 1, 0, 4096, 10, 1                           ; Sine wave
gisquare ftgen 2, 0, 4096, 7, 1, 2048, 1, 0, -1, 2048, -1  ; Square wave
gisaw    ftgen 3, 0, 4096, 7, 0, 2048, 1, 0, -1, 2048, 0   ; Saw wave  


; INIT CHANNELS FOR FXs

; Delay

gkfbdel  init 0.5
gkdtdel  init 375
gkvoldel init 1

gkfbdel chnexport "fbdel", 1, 2, 0.4, 0, 0.99
gkdtdel chnexport "dtdel", 1, 2, 375, 1, 5000
gkvoldel chnexport "voldel", 1, 2, 1, 0, 1

; Reverb

gkfbrev init 0.4
gkcfrev init 15000
gkvolrev init 1

gkfbrev chnexport "fbrev", 1, 2, 0.4, 0, 0.99
gkcfrev chnexport "cfrev", 1, 2, 15000, 0, 20000
gkvolrev chnexport "volrev", 1, 2, 1, 0, 1

; Chorus

gkvolchorus init 1
gkdelchorus init 0
gkdivchorus init 30

gkvolchorus chnexport "volchorus", 1, 2, 1, 0, 1
gkdelchorus chnexport "delchorus", 1, 2, 0, 0, 500
gkdivchorus chnexport "divchorus", 1, 2, 30, 1, 60

; Mixer / Master

gkvolMaster init 1

gkvolMaster chnexport "m_vol", 1, 2, 1, 0, 1

; Waveloss

gkvolWaveloss init 0 
gkdropWaveloss init 0
gkmaxWaveloss init 50

gkvolWaveloss chnexport "wl", 1, 2, 1, 0, 1
gkdropWaveloss chnexport "dropwl", 1, 2, 1, 0, 1
gkmaxWaveloss chnexport "maxwl", 1, 2, 5, 1, 100
gkmodeWaveloss chnexport "modewl", 1, 2, 0, 0, 1

;opcode for declicking an audio signal.
;Should only be used in instruments that have positive p3 duration.
;taken from Steven Yi livecode.orc
opcode declick, a, a
ain xin
aenv = linseg:a(0, 0.01, 1, p3 - 0.02, 1, 0.01, 0)
xout ain * aenv
endop

;opcode to load an audio file into a table.
opcode loadSample, i, S
Sample xin
iNum ftgen 0, 0, 0, -1, Sample, 0, 0, 0
xout iNum
endop

;opcode to generate dtmf tones
opcode dtmf, a, Skk
    Sbutton, kamp_low, kamp_high xin

    kRowFreqs[] fillarray 697, 770, 852, 941
    kColFreqs[] fillarray 1209, 1336, 1477, 1633

    ; Determine frequencies based on the button input (Sbutton)
    ; DTMF Layout mapping:
    ;           Col0    Col1    Col2    Col3
    ; Row0:     1       2       3       A
    ; Row1:     4       5       6       B
    ; Row2:     7       8       9       C
    ; Row3:     *       0       #       D

    kfreq_low = kRowFreqs[0]
    kfreq_high = kColFreqs[0]

    if strcmpk(Sbutton, "1") == 0 then
        kfreq_low = kRowFreqs[0]
        kfreq_high = kColFreqs[0]
    elseif strcmpk(Sbutton, "2") == 0 then
        kfreq_low = kRowFreqs[0]
        kfreq_high = kColFreqs[1]
    elseif strcmpk(Sbutton, "3") == 0 then
        kfreq_low = kRowFreqs[0]
        kfreq_high = kColFreqs[2]
    elseif strcmpk(Sbutton, "A") == 0 then
        kfreq_low = kRowFreqs[0]
        kfreq_high = kColFreqs[3]
    elseif strcmpk(Sbutton, "4") == 0 then
        kfreq_low = kRowFreqs[1]
        kfreq_high = kColFreqs[0]
    elseif strcmpk(Sbutton, "5") == 0 then
        kfreq_low = kRowFreqs[1]
        kfreq_high = kColFreqs[1]
    elseif strcmpk(Sbutton, "6") == 0 then
        kfreq_low = kRowFreqs[1]
        kfreq_high = kColFreqs[2]
    elseif strcmpk(Sbutton, "B") == 0 then
        kfreq_low = kRowFreqs[1]
        kfreq_high = kColFreqs[3]
    elseif strcmpk(Sbutton, "7") == 0 then
        kfreq_low = kRowFreqs[2]
        kfreq_high = kColFreqs[0]
    elseif strcmpk(Sbutton, "8") == 0 then
        kfreq_low = kRowFreqs[2]
        kfreq_high = kColFreqs[1]
    elseif strcmpk(Sbutton, "9") == 0 then
        kfreq_low = kRowFreqs[2]
        kfreq_high = kColFreqs[2]
    elseif strcmpk(Sbutton, "C") == 0 then
        kfreq_low = kRowFreqs[2]
        kfreq_high = kColFreqs[3]
    elseif strcmpk(Sbutton, "*") == 0 then
        kfreq_low = kRowFreqs[3]
        kfreq_high = kColFreqs[0]
    elseif strcmpk(Sbutton, "0") == 0 then
        kfreq_low = kRowFreqs[3]
        kfreq_high = kColFreqs[1]
    elseif strcmpk(Sbutton, "#") == 0 then
        kfreq_low = kRowFreqs[3]
        kfreq_high = kColFreqs[2]
    elseif strcmpk(Sbutton, "D") == 0 then
        kfreq_low = kRowFreqs[3]
        kfreq_high = kColFreqs[3]
    endif

    ; Generate the tones

    aosc_low poscil kamp_low, kfreq_low, gisine 
    aosc_high poscil kamp_high, kfreq_high, gisine 
    xout aosc_low + aosc_high
endop


;Should only be used in instruments that have positive p3 duration.
opcode ADEnv, a, ii
iAD, iInvert xin 

iMin = (0 + iInvert) % 2 
iMax = (1 + iInvert) % 2

if iAD > 1 then 
iADD = iAD % 1
iMin = (iMin == 0) ? 0.001 : iMin 
iMax = (iMax == 0) ? 0.001 : iMax
aEnv = expseg:a(iMin, (p3 -0.02)*iADD+0.01, iMax, (p3 -0.02)*(1-iADD)+0.01, iMin)
else 
aEnv = linseg:a(iMin, (p3 -0.02)*iAD+0.01, iMax, (p3 -0.02)*(1-iAD)+0.01, iMin)
endif
xout aEnv
endop

; a stereo phaser fx
;opcode PhaserFx, aa, aakkkk
;    aInL, aInR, kMix, kRate, kFdbk, kStages xin

;    aPhaserL phaser2 aInL, kRate, kFdbk, kStages, 4
;    aPhaserR phaser2 aInR, kRate, kFdbk, kStages, 4

    ; Dry/Wet Mix
;    aOutL = (aInL * (1 - kMix)) + (aPhaserL * kMix)
;    aOutR = (aInR * (1 - kMix)) + (aPhaserR * kMix)

;    xout aOutL, aOutR
;endop

; a basic distortion thingy with a single character knob.
opcode DistortionFx, aa, aakkkk
    aInL, aInR, kMix, kPregain, kPostgain, kCharacter xin

    kShape1, kShape2 init 0, 0

    ; kCharacter (0-1) maps to different distortion styles:
    ; 0.0 = Soft Saturation
    ; 0.5 = Hard Clipping
    ; 1.0 = Asymmetrical Fuzz
    if kCharacter < 0.5 then
        ; Interpolate from Soft (0.0) to Hard (0.5)
        kNorm = kCharacter * 2 ; Normalize to 0-1 range
        kShape1 = 0.5 - (kNorm * 0.5)
        kShape2 = 0.5 - (kNorm * 0.5)
    else
        ; Interpolate from Hard (0.5) to Fuzzy (1.0)
        kNorm = (kCharacter - 0.5) * 2 ; Normalize to 0-1 range
        kShape1 = 0 + (kNorm * 0.2)
        kShape2 = 0 - (kNorm * 0.3)
    endif

    aDistL distort1 aInL, kPregain, kPostgain, kShape1, kShape2, 1
    aDistR distort1 aInR, kPregain, kPostgain, kShape1, kShape2, 1

    ; Dry/Wet Mix
    aOutL = (aInL * (1 - kMix)) + (aDistL * kMix)
    aOutR = (aInR * (1 - kMix)) + (aDistR * kMix)

    xout aOutL, aOutR
endop

; a Ring Modulator which can be sidechained to channels 15, 16 or used w/ regular waveforms
opcode RingModFx, aa, aakkiik
    aInL, aInR, kMix, kGain, iSource, iWavetable, kFreq xin
    aModL, aModR init 0, 0

    if iSource >= 1 then
        ; Source is external sidechain from ZAK
        aModL zar 15 ; Read left sidechain signal from zak channel 15
        aModR zar 16 ; Read right sidechain signal from zak channel 16
    else
        ; Source is internal oscillator
        aInternalMod poscil 1, kFreq, iWavetable
        aModL = aInternalMod
        aModR = aInternalMod 
    endif

    aRingL = aInL * aModL * kGain
    aRingR = aInR * aModR * kGain

    ; Dry/Wet Mix
    aOutL = (aInL * (1 - kMix)) + (aRingL * kMix)
    aOutR = (aInR * (1 - kMix)) + (aRingR * kMix)

    xout aOutL, aOutR
endop

; a couple of resonant LP + HP filter inspired by the filter on Elektron MachineDrum
opcode DjFilterFx, aa, aakkkkk
    aInL, aInR, kMix, kLpFreq, kLpRes, kHpFreq, kHpRes xin

    ; Resonant Lowpass Filter (Moog Ladder style)
    aLpL zdf_ladder aInL, kLpFreq, kLpRes
    aLpR zdf_ladder aInR, kLpFreq, kLpRes

    ; Resonant Highpass Filter (State-Variable Filter)
    aLp_dummyL, aHpL, aBp_dummyL svfilter aInL, kHpFreq, kHpRes
    aLp_dummyR, aHpR, aBp_dummyR svfilter aInR, kHpFreq, kHpRes

    aFilteredL = (aLpL + aHpL) * 0.7
    aFilteredR = (aLpR + aHpR) * 0.7

    ; Dry/Wet Mix
    aOutL = (aInL * (1 - kMix)) + (aFilteredL * kMix)
    aOutR = (aInR * (1 - kMix)) + (aFilteredR * kMix)

    xout aOutL, aOutR
endop

; A sidechainable compressor/limiter with a macro 'hardness' control.
; It uses the 'compress2' opcode, which works in the dB domain.
;
; kHardness (0-1) transitions the effect from a soft compressor to a hard limiter:
; - Hardness 0: Soft-knee, slow attack/release, low ratio.
; - Hardness 1: Hard-knee, fast attack/release, high ratio.
opcode CompressorLimiterFx, aa, aakkki
    aInL, aInR, kThreshDb, kHardness, kMix, iSidechainEnable xin

    ; --- Define Min/Max values for interpolation ---
    kRatioMin = 2       ; Soft compressor ratio (2:1)
    kRatioMax = 100     ; Hard limiter ratio (100:1)
    kAttackMax = 0.03   ; Soft compressor attack (30ms)
    kAttackMin = 0.001  ; Hard limiter attack (1ms)
    kReleaseMax = 0.5   ; Soft compressor release (500ms)
    kReleaseMin = 0.05  ; Hard limiter release (50ms)
    iMaxKneeDb = 12     ; Max one-sided knee width in dB (for a total 24dB span at hardness=0)

    ; --- Interpolate parameters based on kHardness (0 to 1) ---
    ; Use squared hardness for a more aggressive curve on the ratio
    kHardnessSquared = kHardness * kHardness

    kCurrentRatio   = kRatioMin + (kHardnessSquared * (kRatioMax - kRatioMin))
    kCurrentAttack  = kAttackMax - (kHardness * (kAttackMax - kAttackMin))
    kCurrentRelease = kReleaseMax - (kHardness * (kReleaseMax - kReleaseMin))

    ; Calculate knee width in dB. As hardness -> 1, width -> 0.
    kKneeWidthDb = iMaxKneeDb * (1 - kHardness)
    kLoKneeDb = kThreshDb - kKneeWidthDb
    kHiKneeDb = kThreshDb + kKneeWidthDb

    ; --- Sidechain Logic ---
    aSideL, aSideR init 0, 0
    if iSidechainEnable >= 1 then
        aSideL zar 17
        aSideR zar 18
    else
        aSideL = aInL
        aSideR = aInR
    endif
    aSideControl = (aSideL + aSideR) * 0.5

    ; --- Apply compression using compress2 ---
    kGateThresh = -90 ; Standard noise floor threshold for compress2
    ilookahead = 0.005 ; 5ms lookahead

    aCompL compress2 aInL, aSideControl, kGateThresh, kLoKneeDb, kHiKneeDb, kCurrentRatio, kCurrentAttack, kCurrentRelease, ilookahead
    aCompR compress2 aInR, aSideControl, kGateThresh, kLoKneeDb, kHiKneeDb, kCurrentRatio, kCurrentAttack, kCurrentRelease, ilookahead

    ; --- Dry/Wet Mix ---
    aOutL = (aInL * (1 - kMix)) + (aCompL * kMix)
    aOutR = (aInR * (1 - kMix)) + (aCompR * kMix)

    xout aOutL, aOutR
endop


; FxChain to be used within each instrument before main send
opcode FxChainInstr, aa, aakkkkkkiikkkkkkkkki
    aLeft, aRight, kDistMix, kDistPregain, kDistPostgain, kDistCharacter,  kRingModMix, kRingModGain, iRingModSource, iRingModWavetable, kRingModFreq, kDjFilterMix, kFilterLpFreq, kFilterLpRes, kFilterHpFreq, kFilterHpRes, kCompThreshDb, kCompHardness, kCompMix, iCompSideChain xin
   
    aSigL = aLeft
    aSigR = aRight

    ; --- FX CHAIN ---
    ;aSigL, aSigR PhaserFx    aSigL, aSigR, kPhaserMix, kPhaserRate, kPhaserFdbk, kPhaserStages
    aSigL, aSigR DistortionFx aSigL, aSigR, kDistMix, kDistPregain, kDistPostgain, kDistCharacter
    aSigL, aSigR RingModFx    aSigL, aSigR, kRingModMix, kRingModGain, iRingModSource, iRingModWavetable, kRingModFreq
    aSigL, aSigR DjFilterFx   aSigL, aSigR, kDjFilterMix, kFilterLpFreq, kFilterLpRes, kFilterHpFreq, kFilterHpRes
    aSigL, aSigR CompressorLimiterFx aSigL, aSigR, kCompThreshDb, kCompHardness, kCompMix, iCompSideChain

    xout aSigL, aSigR
endop


instr 1 ; Sampler

SFile = p28

p3 = filelen(SFile)
ivol = p4
irev = p5
idel = p6
ipan = p7
ichor = p8
isidecomp = p9
isidermod = p10
kDistMix = p11 
kDistPregain = p12 
kDistPostgain = p13 
kDistCharacter = p14 ;0.5; 0=soft, 0.5=hard, 1=fuzzy
kRingModMix = p15 
kRingModGain = p16 
iRingModSource = p17 ; Oscillator by default
iRingModWavetable = p18 
kRingModFreq = p19 
kDjFilterMix = 1 
kFilterLpFreq = p20 
kFilterLpRes = p21 ; 0 - 1 (usually) 
kFilterHpFreq = p22
kFilterHpRes = p23 ; 1 - 500 (better keep it lowish for more stability) 
kCompThreshDb = p24 ;-18
kCompHardness = p25 ;0.2
kCompMix = p26
iCompSideChain = p27 ;0 for disable, 1 for enable
iTune = 2^(p29 / 12)

inchs = filenchnls(SFile)

if inchs == 1 then

aLeft diskin2 SFile, iTune
aL, aR pan3 aLeft, aLeft, ipan, 1

else

aLeft, aRight diskin2 SFile, iTune
aL, aR pan3 aLeft, aRight, ipan, 1

endif

aL, aR FxChainInstr aL, aR, kDistMix, kDistPregain, kDistPostgain, kDistCharacter,  kRingModMix, kRingModGain, iRingModSource, iRingModWavetable, kRingModFreq, kDjFilterMix, kFilterLpFreq, kFilterLpRes, kFilterHpFreq, kFilterHpRes, kCompThreshDb, kCompHardness, kCompMix, iCompSideChain

zawm ivol * aL, 1
zawm ivol * aR, 2

zawm irev * aL, 3
zawm irev * aR, 4

zawm idel * aL, 7
zawm idel * aR, 8

zawm ichor * aL, 11
zawm ichor * aR, 12

zawm isidecomp * aL, 17
zawm isidecomp * aR, 18

zawm isidermod * aL, 15
zawm isidermod * aR, 16

endin


instr 2 ; Stutter

Sname = p28

idur = p3 
ivol = p4
irev = p5
idel = p6
ipan = p7
ichor = p8
isidecomp = p9
isidermod = p10
kDistMix = p11 
kDistPregain = p12 
kDistPostgain = p13 
kDistCharacter = p14 ;0.5; 0=soft, 0.5=hard, 1=fuzzy
kRingModMix = p15 
kRingModGain = p16 
iRingModSource = p17 ; Oscillator by default
iRingModWavetable = p18 
kRingModFreq = p19 
kDjFilterMix = 1 
kFilterLpFreq = p20 
kFilterLpRes = p21 ; 0.5 - 25 
kFilterHpFreq = p22
kFilterHpRes = p23 ; 1 - 500 (better keep it lowish for more stability) 
kCompThreshDb = p24 ;-18
kCompHardness = p25 ;0.2
kCompMix = p26
iCompSideChain = p27 ;0 for disable, 1 for enable

i_tune = 2^(p29 / 12)
i_divisor = p30
i_pick = p31
i_repeat = (p32 == 0) ? 1 : p32

inchs = filenchnls(Sname)
ilength = filelen(Sname)
isr = filesr(Sname)
isize = ilength * isr
isamdur = idur * sr
isamslice =  ceil(isamdur / i_repeat)
ipos = i_pick / i_divisor * isize

andx =  phasor((sr/isamslice) * i_tune) * isamslice + ipos

if inchs == 1 then
isam ftgen 0, 0, isize, -1, Sname, 0, 0, 1
aL table3 andx, isam, 0
aL, aR pan3 aL, aL, ipan, 1
else
iLeft ftgen 0, 0, isize, -1, Sname, 0, 0, 1
iRight ftgen 0, 0,isize, -1, Sname, 0, 0, 2
aL table3 andx, iLeft, 0
aR table3 andx, iRight, 0
aL, aR pan3 aL, aR, ipan, 1
endif

aL = declick(aL)
aR = declick(aR)

aL, aR FxChainInstr aL, aR, kDistMix, kDistPregain, kDistPostgain, kDistCharacter,  kRingModMix, kRingModGain, iRingModSource, iRingModWavetable, kRingModFreq, kDjFilterMix, kFilterLpFreq, kFilterLpRes, kFilterHpFreq, kFilterHpRes, kCompThreshDb, kCompHardness, kCompMix, iCompSideChain


zawm ivol * aL, 1
zawm ivol * aR, 2

zawm irev * aL, 3
zawm irev * aR, 4

zawm idel * aL, 7
zawm idel * aR, 8

zawm ichor * aL, 11
zawm ichor * aR, 12

zawm isidermod * aL, 15
zawm isidermod * aR, 16

zawm isidecomp * aL, 17
zawm isidecomp * aR, 18


endin

instr 3 ; Bass 303

;adapted from Steven Yi Livecode.orc

idur = p3
ivol = p4
irev = p5
idel = p6
ipan = p7
ichor = p8
isidecomp = p9
isidermod = p10
kDistMix = p11 
kDistPregain = p12 
kDistPostgain = p13 
kDistCharacter = p14 ;0.5; 0=soft, 0.5=hard, 1=fuzzy
kRingModMix = p15 
kRingModGain = p16 
iRingModSource = p17 ; Oscillator by default
iRingModWavetable = p18 
kRingModFreq = p19 
kDjFilterMix = 1 
kFilterLpFreq = p20 
kFilterLpRes = p21 ; 0 - 1 (usually) 
kFilterHpFreq = p22
kFilterHpRes = p23 ; 1 - 500 (better keep it lowish for more stability) 
kCompThreshDb = p24 ;-18
kCompHardness = p25 ;0.2
kCompMix = p26
iCompSideChain = p27 ;0 for disable, 1 for enable
ifreq = cpsmidinn(p28)
icf = p29
ires = p30

imode = p31

acut = 200 + expon(1, idur, 0.001) * icf
asig = vco2(1, ifreq, imode)
asig = diode_ladder(asig, acut, ires, 1, 4)
asig = (tanh (asig * 4)) * 0.5
asig declick asig
aL, aR pan3 asig, asig, ipan, 1

aL, aR FxChainInstr aL, aR, kDistMix, kDistPregain, kDistPostgain, kDistCharacter,  kRingModMix, kRingModGain, iRingModSource, iRingModWavetable, kRingModFreq, kDjFilterMix, kFilterLpFreq, kFilterLpRes, kFilterHpFreq, kFilterHpRes, kCompThreshDb, kCompHardness, kCompMix, iCompSideChain 

zawm ivol * aL, 1
zawm ivol * aR, 2

zawm irev * aL, 3
zawm irev * aR, 4

zawm idel * aL, 7
zawm idel * aR, 8

zawm ichor * aL, 11
zawm ichor * aR, 12

zawm isidermod * aL, 15
zawm isidermod * aR, 16

zawm isidecomp * aL, 17
zawm isidecomp * aR, 18

endin

instr 4 ; Hoover Bass

idur = p3
ivol = p4
irev = p5
idel = p6
ipan = p7
ichor = p8
isidecomp = p9
isidermod = p10
kDistMix = p11 
kDistPregain = p12 
kDistPostgain = p13 
kDistCharacter = p14 ;0.5; 0=soft, 0.5=hard, 1=fuzzy
kRingModMix = p15 
kRingModGain = p16 
iRingModSource = p17 ; Oscillator by default
iRingModWavetable = p18 
kRingModFreq = p19 
kDjFilterMix = 1 
kFilterLpFreq = p20 
kFilterLpRes = p21 ; 0 - 1 (usually) 
kFilterHpFreq = p22
kFilterHpRes = p23 ; 1 - 500 (better keep it lowish for more stability) 
kCompThreshDb = p24 ;-18
kCompHardness = p25 ;0.2
kCompMix = p26
iCompSideChain = p27 ;0 for disable, 1 for enable
ifreq = cpsmidinn(p28)
icf = p29
ires = p30
iad = p31

ifEnvMax = 1.333*ifreq >= sr/2 ? ifreq : 1.333*ifreq

kcfEnv = expon(ifEnvMax, p3, icf)

aenv = ADEnv(iad, 0)

kr3 unirand 1
kr3 port kr3, 0.01
klf3 = lfo:k(0.5, 1.5*kr3, 0)
klf3 = limit((klf3+0.5), 0.05, 0.95)
a1 = vco2(1, ifreq,4,(klf3*0.01))
a2 = vco2(1, ifreq*(0.08+(7/12)),4,(klf3*0.01))
a3 = vco2(1, ifreq*0.52)
af = a1 + a3 * 0.88 + a2 * 0.66
ao = diode_ladder(af, kcfEnv, ires)
kr1 unirand 1
kr2 unirand 1
kr1 port kr1, 0.01
kr2 port kr2, 0.01
alfo = lfo(0.005, kr2 + 0.1)
alfo2 = lfo(0.005, kr1 + 0.1)
adel = vdelay3(ao/2, (0.1+alfo)*1000, 1000)
adel2 = vdelay3(ao/2, (0.1+alfo2)*1000, 1000)
adecl = declick(ao+adel*0.8)
adecr = declick(ao+adel2*0.8)

aL, aR pan3 adecl* aenv, adecl* aenv, ipan, 1

aL, aR FxChainInstr aL, aR, kDistMix, kDistPregain, kDistPostgain, kDistCharacter,  kRingModMix, kRingModGain, iRingModSource, iRingModWavetable, kRingModFreq, kDjFilterMix, kFilterLpFreq, kFilterLpRes, kFilterHpFreq, kFilterHpRes, kCompThreshDb, kCompHardness, kCompMix, iCompSideChain

zawm ivol * aL, 1
zawm ivol * aR, 2

zawm irev * aL, 3
zawm irev * aR, 4

zawm idel * aL, 7
zawm idel * aR, 8

zawm ichor * aL, 11
zawm ichor * aR, 12

zawm isidermod * aL, 15
zawm isidermod * aR, 16

zawm isidecomp * aL, 17
zawm isidecomp * aR, 18

endin

instr 5 ; HiHats 808

idur = p3
ivol = p4
irev = p5
idel = p6
ipan = p7
ichor = p8
isidecomp = p9
isidermod = p10
kDistMix = p11 
kDistPregain = p12 
kDistPostgain = p13 
kDistCharacter = p14 ;0.5; 0=soft, 0.5=hard, 1=fuzzy
kRingModMix = p15 
kRingModGain = p16 
iRingModSource = p17 ; Oscillator by default
iRingModWavetable = p18 
kRingModFreq = p19 
kDjFilterMix = 1 
kFilterLpFreq = p20 
kFilterLpRes = p21 ; 0 - 1 (usually) 
kFilterHpFreq = p22
kFilterHpRes = p23 ; 1 - 500 (better keep it lowish for more stability) 
kCompThreshDb = p24 ;-18
kCompHardness = p25 ;0.2
kCompMix = p26
iCompSideChain = p27 ;0 for disable, 1 for enable
iopen = p28
itune = p29

pa        =        (iopen >= 0.5 ? 1 : .15)   ; Select open or closed
ifreq1    =        540*itune                     ; Tune
ifreq2    =        800*itune                     ; Tune

aenv  =   expsega(.01, .0005, 1, pa - .0005, .01)   ; Percussive envelope
asqr1 =   poscil(1, ifreq1, 2, -1)
asqr2 =   poscil(1, ifreq1*1.342, 2, -1)
asqr3 =   poscil(1, ifreq1*1.2312, 2, -1)
asqr4 =   poscil(1, ifreq1*1.6532, 2, -1)
asqr5 =   poscil(1, ifreq1*1.9523, 2, -1)
asqr6 =   poscil(1, ifreq1*2.1523, 2, -1)
a808 = sum(asqr1, asqr2, asqr3, asqr4, asqr5, asqr6)
a808 =    butterhp(a808, 5270)
a808 =    butterhp(a808, 5270)

aL, aR pan3 a808*aenv, a808*aenv, ipan, 1

aL, aR FxChainInstr aL, aR, kDistMix, kDistPregain, kDistPostgain, kDistCharacter,  kRingModMix, kRingModGain, iRingModSource, iRingModWavetable, kRingModFreq, kDjFilterMix, kFilterLpFreq, kFilterLpRes, kFilterHpFreq, kFilterHpRes, kCompThreshDb, kCompHardness, kCompMix, iCompSideChain

zawm ivol * aL, 1
zawm ivol * aR, 2

zawm irev * aL, 3
zawm irev * aR, 4

zawm idel * aL, 7
zawm idel * aR, 8

zawm ichor * aL, 11
zawm ichor * aR, 12

zawm isidermod * aL, 15
zawm isidermod * aR, 16

zawm isidecomp * aL, 17
zawm isidecomp * aR, 18

endin

instr 6 ; Simple subtractive-FM

idur = p3
ivol = p4
irev = p5
idel = p6
ipan = p7
ichor = p8
isidecomp = p9
isidermod = p10
kDistMix = p11 
kDistPregain = p12 
kDistPostgain = p13 
kDistCharacter = p14 ;0.5; 0=soft, 0.5=hard, 1=fuzzy
kRingModMix = p15 
kRingModGain = p16 
iRingModSource = p17 ; Oscillator by default
iRingModWavetable = p18 
kRingModFreq = p19 
kDjFilterMix = 1 
kFilterLpFreq = p20 
kFilterLpRes = p21 ; 0 - 1 (usually) 
kFilterHpFreq = p22
kFilterHpRes = p23 ; 1 - 500 (better keep it lowish for more stability) 
kCompThreshDb = p24 ;-18
kCompHardness = p25 ;0.2
kCompMix = p26
iCompSideChain = p27 ;0 for disable, 1 for enable
ifreq = cpsmidinn(p28)

kindx = p34
kcar = p32
kfilt = p29
kdpth = p33
iad = p31
kres = p30
aenv = ADEnv(iad, 0)

amod = poscil(1, cpsmidinn(ifreq) * kindx, gisine)
acar = poscil(1, cpsmidinn(ifreq) + amod * kdpth * sr/4, gisine)

audio = diode_ladder(acar, kfilt, kres , 1, 1.5)

aL, aR pan3 audio*aenv, audio*aenv, ipan, 1

aL, aR FxChainInstr aL, aR, kDistMix, kDistPregain, kDistPostgain, kDistCharacter,  kRingModMix, kRingModGain, iRingModSource, iRingModWavetable, kRingModFreq, kDjFilterMix, kFilterLpFreq, kFilterLpRes, kFilterHpFreq, kFilterHpRes, kCompThreshDb, kCompHardness, kCompMix, iCompSideChain

zawm ivol * aL, 1
zawm ivol * aR, 2

zawm irev * aL, 3
zawm irev * aR, 4

zawm idel * aL, 7
zawm idel * aR, 8

zawm ichor * aL, 11
zawm ichor * aR, 12

zawm isidermod * aL, 15
zawm isidermod * aR, 16

zawm isidecomp * aL, 17
zawm isidecomp * aR, 18

endin

instr 7 ; SuperSaw

;adapted from Steven Yi Livecode.orc && inspired by the JP8080

idur = p3
ivol = p4
irev = p5
idel = p6
ipan = p7
ichor = p8
isidecomp = p9
isidermod = p10
kDistMix = p11 
kDistPregain = p12 
kDistPostgain = p13 
kDistCharacter = p14 ;0.5; 0=soft, 0.5=hard, 1=fuzzy
kRingModMix = p15 
kRingModGain = p16 
iRingModSource = p17 ; Oscillator by default
iRingModWavetable = p18 
kRingModFreq = p19 
kDjFilterMix = 1 
kFilterLpFreq = p20 
kFilterLpRes = p21 ; 0 - 1 (usually) 
kFilterHpFreq = p22
kFilterHpRes = p23 ; 1 - 500 (better keep it lowish for more stability) 
kCompThreshDb = p24 ;-18
kCompHardness = p25 ;0.2
kCompMix = p26
iCompSideChain = p27 ;0 for disable, 1 for enable
ifreq = cpsmidinn(p28)
icf = p29
ires = p30 ; 0.5 - 25 (?)
iad = p31
idetune = p32
imix = p33

isidecomp = 0
isidermod = 0

idetune *= idetune 

aenv  = ADEnv(iad, 0)

kcfEnv  = linseg(ifreq, (p3 -0.02)*iad+0.01, icf,   (p3 -0.02)*(1-iad)+0.01, 200)

; kcfEnv = expon(icf, p3, ifreq/8)

asig1 = vco2(1,  ifreq)

asig = vco2(imix,  ifreq * (1 + idetune * -0.107))
asig += vco2(imix,  ifreq * (1 + idetune * -0.061))
asig += vco2(imix,  ifreq * (1 + idetune * -0.019))
asig += vco2(imix,  ifreq * (1 + idetune * 0.019))
asig += vco2(imix,  ifreq * (1 + idetune * 0.064))
asig += vco2(imix,  ifreq * (1 + idetune * 0.11))

asig *= 0.17

asig += asig1

asig = zdf_ladder(asig, kcfEnv, ires)
asig = K35_hpf(asig, ifreq * 0.5, 2)

asig = declick(asig)

aL, aR pan3 asig*aenv, asig*aenv, ipan, 1

aL, aR FxChainInstr aL, aR, kDistMix, kDistPregain, kDistPostgain, kDistCharacter,  kRingModMix, kRingModGain, iRingModSource, iRingModWavetable, kRingModFreq, kDjFilterMix, kFilterLpFreq, kFilterLpRes, kFilterHpFreq, kFilterHpRes, kCompThreshDb, kCompHardness, kCompMix, iCompSideChain

zawm ivol * aL, 1
zawm ivol * aR, 2

zawm irev * aL, 3
zawm irev * aR, 4

zawm idel * aL, 7
zawm idel * aR, 8

zawm ichor * aL, 11
zawm ichor * aR, 12

zawm isidermod * aL, 15
zawm isidermod * aR, 16

zawm isidecomp * aL, 17
zawm isidecomp * aR, 18

endin

instr 8	;String pad from Bay at Night, Diaz

idur = p3
iamp = p4
irev = p5
idel = p6
ipan = p7
ichor = p8
isidecomp = p9
isidermod = p10
kDistMix = p11 
kDistPregain = p12 
kDistPostgain = p13 
kDistCharacter = p14 ;0.5; 0=soft, 0.5=hard, 1=fuzzy
kRingModMix = p15 
kRingModGain = p16 
iRingModSource = p17 ; Oscillator by default
iRingModWavetable = p18 
kRingModFreq = p19 
kDjFilterMix = 1 
kFilterLpFreq = p20 
kFilterLpRes = p21 ; 0 - 1 (usually) 
kFilterHpFreq = p22
kFilterHpRes = p23 ; 1 - 500 (better keep it lowish for more stability) 
kCompThreshDb = p24 ;-18
kCompHardness = p25 ;0.2
kCompMix = p26
iCompSideChain = p27 ;0 for disable, 1 for enable
ihz = cpsmidinn(p28)

kctrl = linseg(0, p3*.3, iamp, idur*.3, iamp, idur*.4, 0)
anoise = rand(kctrl)
anoise = butterlp(anoise, 5*ihz)
  ; Sligth chorus effect
afund = oscil(kctrl, ihz, 3)           
acel1 = oscil(kctrl, ihz - .1, 3)     
acel2 = oscil(kctrl, ihz + .1, 3)      
asig = afund + acel1 + acel2
  ; (larger velocity implies more brighter sound)
asig = asig + .2*anoise
asig = butterlp(asig, (iamp * 127 - 60)*40+600)

aL, aR pan3 asig, asig, ipan, 1

aL, aR FxChainInstr aL, aR, kDistMix, kDistPregain, kDistPostgain, kDistCharacter,  kRingModMix, kRingModGain, iRingModSource, iRingModWavetable, kRingModFreq, kDjFilterMix, kFilterLpFreq, kFilterLpRes, kFilterHpFreq, kFilterHpRes, kCompThreshDb, kCompHardness, kCompMix, iCompSideChain

zawm  aL, 1
zawm  aR, 2

zawm irev * aL, 3
zawm irev * aR, 4

zawm idel * aL, 7
zawm idel * aR, 8

zawm ichor * aL, 11
zawm ichor * aR, 12

zawm isidermod * aL, 15
zawm isidermod * aR, 16

zawm isidecomp * aL, 17
zawm isidecomp * aR, 18

endin

instr 9 ; Karplus - Strong

idur = p3
ivol = p4
irev = p5
idel = p6
ipan = p7
ichor = p8
isidecomp = p9
isidermod = p10
kDistMix = p11 
kDistPregain = p12 
kDistPostgain = p13 
kDistCharacter = p14 ;0.5; 0=soft, 0.5=hard, 1=fuzzy
kRingModMix = p15 
kRingModGain = p16 
iRingModSource = p17 ; Oscillator by default
iRingModWavetable = p18 
kRingModFreq = p19 
kDjFilterMix = 1 
kFilterLpFreq = p20 
kFilterLpRes = p21 ; 0 - 1 (usually) 
kFilterHpFreq = p22
kFilterHpRes = p23 ; 1 - 500 (better keep it lowish for more stability) 
kCompThreshDb = p24 ;-18
kCompHardness = p25 ;0.2
kCompMix = p26
iCompSideChain = p27 ;0 for disable, 1 for enable
ipitch = cpsmidinn(p28)
irough = p29
istretch = p30

kpitch = expseg:k(ipitch, idur, 432)

asig = pluck(1, ipitch, 432, 0, 4, irough, (49*istretch)+1) 

aL, aR pan3 asig, asig, ipan, 1

aL, aR FxChainInstr aL, aR, kDistMix, kDistPregain, kDistPostgain, kDistCharacter,  kRingModMix, kRingModGain, iRingModSource, iRingModWavetable, kRingModFreq, kDjFilterMix, kFilterLpFreq, kFilterLpRes, kFilterHpFreq, kFilterHpRes, kCompThreshDb, kCompHardness, kCompMix, iCompSideChain

zawm ivol * aL, 1
zawm ivol * aR, 2

zawm irev * aL, 3
zawm irev * aR, 4

zawm idel * aL, 7
zawm idel * aR, 8

zawm ichor * aL, 11
zawm ichor * aR, 12

zawm isidermod * aL, 15
zawm isidermod * aR, 16

zawm isidecomp * aL, 17
zawm isidecomp * aR, 18

endin

; instr 10 ; phaserSynth 

; idur = p3
; ivol = p4
; irev = p5
; idel = p6
; ipan = p7
; ichor = p8
; isidecomp = 0 
; isidermod = 0

; inote = cpsmidinn(p9)

; icf = p10
; ires = p11
; iad = 12
; iadp1 = p13
; isim = p14
; iadp2 = abs(abs(isim) - abs(iadp1))
; itable = p15
; itable2 = p16
; iwmix = p17
; itune = p18
; isep = p19
; imode = p20
; ienv_amp = p21
; ifb = p22

; aenv = ADEnv(iad, 0)

; if (iadp1 >= 0 && isim >= 0) then
; kep1 = linseg:k(0, idur*iadp1, ienv_amp, idur*(1-iadp1), 0)
; kep2 = linseg:k(0, idur*iadp2, ienv_amp, idur*(1-iadp2), 0)
; elseif (iadp1 < 0 && isim >= 0) then
; iadp1 = abs(iadp1)
; kep1 = linseg:k(ienv_amp, idur*iadp1, 0, idur*(1-iadp1), ienv_amp)
; kep2 = linseg:k(ienv_amp, idur*iadp2, 0, idur*(1-iadp2), ienv_amp)
; elseif (iadp1 >= 0 && isim < 0) then
; kep1 = linseg:k(0, idur*iadp1, ienv_amp, idur*(1-iadp1), 0)
; kep2 = linseg:k(ienv_amp, idur*iadp2, 0, idur*(1-iadp2), ienv_amp)
; elseif (iadp1 < 0 && isim < 0) then
; iadp1 = abs(iadp1)
; kep1 = linseg:k(ienv_amp, idur*iadp1, 0, idur*(1-iadp1), ienv_amp)
; kep2 = linseg:k(0, idur*iadp2, ienv_amp, idur*(1-iadp2), 0)
; endif

; kep1 += (1 -ienv_amp)
; kep2 += (1 -ienv_amp)

; aosc1 = poscil:a(1, inote, itable)
; aosc2 = poscil:a(1, inote, itable2)
 
; inote2 = inote * itune
 
; aosc3 = poscil:a(1, inote2, itable)
; aosc4 = poscil:a(1, inote2, itable2)

; a1 = aosc1 * (1 - iwmix) + aosc2 * (iwmix)
; a2 = aosc3 * (1 - iwmix) + aosc4 * (iwmix)

; a3 = a1 + a2 

; ap1 phaser2 a3, kep1 * (sr/2 - icf) + icf, ires, 6, imode, kep1*isep, ifb
; ap2 phaser2 a3 + ap1, kep2 * (sr/2 - icf) + icf, ires, 6, imode, kep2*isep, ifb

; ap2 *= aenv 
; ap1 *= aenv
; a3 *= aenv 
; ap2 += a3

; ap2 = declick(ap2)

; ap2 dam ap2, 0.8, 0.8, 2, 0.01, 0.3 

; aL, aR pan3 ap2, ap2, ipan, 1

; zawm ivol * aL, 1
; zawm ivol * aR, 2

; zawm irev * aL, 3
; zawm irev * aR, 4

; zawm idel * aL, 7
; zawm idel * aR, 8

; zawm ichor * aL, 11
; zawm ichor * aR, 12

; zawm isidermod * aL, 15
; zawm isidermod * aR, 16

; zawm isidecomp * aL, 17
; zawm isidecomp * aR, 18

; endin

instr 11 ;DTMF

idur = p3
ivol = p4
irev = p5
idel = p6
ipan = p7
ichor = p8
isidecomp = p9
isidermod = p10
kDistMix = p11 
kDistPregain = p12 
kDistPostgain = p13 
kDistCharacter = p14 ;0.5; 0=soft, 0.5=hard, 1=fuzzy
kRingModMix = p15 
kRingModGain = p16 
iRingModSource = p17 ; Oscillator by default
iRingModWavetable = p18 
kRingModFreq = p19 
kDjFilterMix = 1 
kFilterLpFreq = p20 
kFilterLpRes = p21 ; 0 - 1 (usually) 
kFilterHpFreq = p22
kFilterHpRes = p23 ; 1 - 500 (better keep it lowish for more stability) 
kCompThreshDb = p24 ;-18
kCompHardness = p25 ;0.2
kCompMix = p26
iCompSideChain = p27 ;0 for disable, 1 for enable

SBut = p28
iYAmp = p29
iXAmp = p30
iADRatio = p31

aDTMF = dtmf(SBut, iXAmp, iYAmp)

aenv = ADEnv(iADRatio, 0)

aDTMF = aDTMF * aenv

aL, aR pan3 aDTMF, aDTMF, ipan, 1

aL, aR FxChainInstr aL, aR, kDistMix, kDistPregain, kDistPostgain, kDistCharacter,  kRingModMix, kRingModGain, iRingModSource, iRingModWavetable, kRingModFreq, kDjFilterMix, kFilterLpFreq, kFilterLpRes, kFilterHpFreq, kFilterHpRes, kCompThreshDb, kCompHardness, kCompMix, iCompSideChain

zawm ivol * aL, 1
zawm ivol * aR, 2

zawm irev * aL, 3
zawm irev * aR, 4

zawm idel * aL, 7
zawm idel * aR, 8

zawm ichor * aL, 11
zawm ichor * aR, 12

zawm isidermod * aL, 15
zawm isidermod * aR, 16

zawm isidecomp * aL, 17
zawm isidecomp * aR, 18

endin

instr 100 ; Model:Cycles / Model:Samples MIDI Out

ivol = p4
irev = p5
idel = p6
ipan = p7
ichan = p8
inote = p9
ivel =  p10
ipitch = p11
idecay = p12
icolor = p13
ishape = p14
isweep = p15
icontour = p16
;ilfospeed = p17
;iswing = p18
;ichance = p19
;imachine = p20
;ipunch = p21
;igate = p22

;midiout_i 176, ichan, 7, ivol
;midiout_i 176, ichan, 65, ipitch
;midiout_i 176, ichan, 80, idecay
midiout_i 176, ichan, 10, ipan
;
;midiout_i 176, ichan, 12, idel
;midiout_i 176, ichan, 13, irev
;
;midiout_i 176, ichan, 16, icolor
;midiout_i 176, ichan, 17, ishape
;midiout_i 176, ichan, 18, isweep
;midiout_i 176, ichan, 19, icontour

midion ichan, inote, ivel

endin 

instr 101 ; Model:Cycles chord control

ivol = p4
irev = p5
idel = p6
ipan = p7
ichan = p8
inote = p9
ivel =  p10
ipitch = p11
idecay = p12
icolor = p13
ishape = p14
isweep = p15
icontour = p16
;ilfospeed = p17
;iswing = p18
;ichance = p19
;imachine = p20
;ipunch = p21
;igate = p22

;midiout_i 176, ichan, 7, ivol
;midiout_i 176, ichan, 65, ipitch
;midiout_i 176, ichan, 80, idecay
midiout_i 176, ichan, 10, ipan
;
;midiout_i 176, ichan, 12, idel
;midiout_i 176, ichan, 13, irev
;
;midiout_i 176, ichan, 16, icolor
midiout_i 176, ichan, 17, ishape
;midiout_i 176, ichan, 18, isweep
;midiout_i 176, ichan, 19, icontour

midion ichan, inote, ivel

endin 

instr 550 ; ReverbSC

arvbL zar 3
arvbR zar 4

aoutL, aoutR reverbsc arvbL, arvbR, gkfbrev, gkcfrev

zawm aoutL * gkvolrev, 5
zawm aoutR * gkvolrev, 6

endin

instr 551 ; Delay

adelLr zar 7
adelRr zar 8

kporttime	linseg		0, .001, 1, 1, 1
kdlt		portk		(gkdtdel/1000.0), kporttime
adlt		interp		kdlt

abufferL	delayr 	5
adelL 	deltap3	adlt
		delayw	 adelLr + (adelL * gkfbdel)

abufferR	delayr	 5
adelR 	deltap3	adlt
		delayw	 adelRr + (adelR * gkfbdel)

zawm adelL * gkvoldel, 9
zawm adelR * gkvoldel, 10

endin

instr 552	;Spectral Chorus

 a1 zar 11
 a2 zar 12

 kdlyml = gkdelchorus ;delay in milliseconds
 kblurtime = gkdelchorus / 1000
 
 ak1 = oscili:k(kdlyml/gkdivchorus, 1)
 ar1l = vdelay3(a1, kdlyml/5+ak1, 900)
 ar1r = vdelay3(a2, kdlyml/5+ak1, 900)
 ak2 = oscili:k(kdlyml/gkdivchorus, .995)
 ar2l = vdelay3(a1, kdlyml/5+ak2, 700)
 ar2r = vdelay3(a2, kdlyml/5+ak2, 700)
 ak3 = oscili:k(kdlyml/gkdivchorus, 1.05)
 ar3l = vdelay3(a1, kdlyml/5+ak3, 700)
 ar3r = vdelay3(a2, kdlyml/5+ak3, 700)
 ak4 = oscili:k(kdlyml/gkdivchorus, 1)
 ar4l = vdelay3(a1, kdlyml/5+ak4, 900)
 ar4r = vdelay3(a2, kdlyml/5+ak4, 900)
 aoutl = (ar1l+ar2l+ar3l+ar4l)*.5
 aoutr = (ar1r+ar2r+ar3r+ar4r)*.5
 
 fsigl pvsanal aoutl, 512, 128, 1024, 0  
 fsiglBlur pvsblur fsigl, kblurtime, 5
 aBlurL pvsynth fsiglBlur
 
 fsigr pvsanal aoutl, 512, 128, 1024, 0  
 fsigrBlur pvsblur fsigr, kblurtime, 5
 aBlurR pvsynth fsigrBlur

 aL = aBlurL * gkvolchorus
 aR = aBlurR * gkvolchorus

 zawm aL, 13
 zawm aR, 14

endin


instr 999 ; Mixer / Master out

aL zar 1
aR zar 2
arvbL zar 5
arvbR zar 6
adelL zar 9
adelR zar 10
achoL zar 13
achoR zar 14

aL += arvbL
aR += arvbR
aL += adelL
aR += adelR
aL += achoL
aR += achoR
aLW waveloss aL * gkvolWaveloss, gkdropWaveloss * gkmaxWaveloss, gkmaxWaveloss, 0
arW waveloss aR * gkvolWaveloss, gkdropWaveloss * gkmaxWaveloss, gkmaxWaveloss, 0
aL *= (1-gkvolWaveloss)
aR *= (1-gkvolWaveloss)
aL += aLW
aR += arW
; TODO : Add "Dynamix" Style Comp + Parametric Eq 
aL *= gkvolMaster
aR *= gkvolMaster

outs aL, aR
zacl 0, 50

endin

</CsInstruments>
<CsScore>
i 999 0 -1
i 550 0 -1
i 551 0 -1
i 552 0 -1

</CsScore>
</CsoundSynthesizer>