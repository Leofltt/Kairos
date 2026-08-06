import Kairos.Lib

-- | init the library

-- csd1 = "11000"
aillenPort = "8000"
:set prompt "κ> "
:set -w 
perf <- defaultPerformance
displayIns = displayInstruments perf
displayP = displayParams perf
displayTP = displayTPat perf
p = play perf
s = stop perf
pn = playNow perf
cPat p i = changeTimeF perf i p
cT = changeTempo (clock perf)
addT =  addTS (clock perf)
maybeAddP = maybeAddTPf perf 
addIns = addInstrument perf
addI name ins = addIns name =<< ins
addC i n s = maybeAddP n s >> cPat n i
getTP = getTimePoint perf
addPf' = addPfPath' perf
addPf i pfnum pfname list fun = addPf' i pfnum =<< createPfPat pfnum pfname list fun

gSU = genSeqU

-- common parameters
dur i list fun = addPf i 3 "dur" (toPfs list) fun      -- in seconds
d' i fun list = dur i (fromPfsD list) fun
durTS i list fun = d' i fun  =<< withTimeSignature perf (toPfs list)  -- time signature dependent
vol i list fun = addPf i 4 "vol" (toPfs list) fun     
rev i list fun = addPf i 5 "rev" (toPfs list) fun
sendDelay i list fun = addPf i 6 "del" (toPfs list) fun
sendReverb i list fun = addPf i 7 "rev" (toPfs list) fun
del i list fun = addPf i 6 "del" (toPfs list) fun
pan i list fun = addPf i 7 "pan" (toPfs list) fun
chorus i list fun = addPf i 8 "chorus" (toPfs list) fun
phaser i list fun = addPf i 28 "phaser" (toPfs list) fun
sideComp i list fun = addPf i 9 "sideComp" (toPfs list) fun
sideRing i list fun = addPf i 10 "sideRing" (toPfs list) fun
dist i list fun = addPf i 11 "dist" (toPfs list) fun
distPreGain i list fun = addPf i 12 "distPreGain" (toPfs list) fun
distPostGain i list fun = addPf i 13 "distPostGain" (toPfs list) fun
distChar i list fun = addPf i 14 "distChar" (toPfs list) fun
ringMod i list fun = addPf i 15 "ringMod" (toPfs list) fun
rmGain i list fun = addPf i 16 "rmGain" (toPfs list) fun
rmSc i list fun = addPf i 17 "rmSc" (toPfs list) fun
rmWt i list fun = addPf i 18 "rmWt" (toPfs list) fun
rmFreq i list fun = addPf i 19 "rmFreq" (toPfs list) fun
lpFreq i list fun = addPf i 20 "lpFreq" (toPfs list) fun
lpRes i list fun = addPf i 21 "lpRes" (toPfs list) fun
hpFreq i list fun = addPf i 22 "hpFreq" (toPfs list) fun
hpRes i list fun = addPf i 23 "hpRes" (toPfs list) fun
compThreshDb i list fun = addPf i 24 "compThreshDb" (toPfs list) fun
compHard i list fun = addPf i 25 "compHard" (toPfs list) fun
comp i list fun = addPf i 26 "comp" (toPfs list) fun
compSc i list fun = addPf i 27 "compSc" (toPfs list) fun

-- instrument specific parameters (try to keep the consistent for ease of use)
pitch i list fun = addPf i 29 "pitch" (toPfs list) fun     -- 303, hoover, karp, lpFM, superSaw, stringPad
cf i list fun = addPf i 30 "cf" (toPfs list) fun       -- 303, hoover, lpFM, superSaw
res i list fun = addPf i 31 "res" (toPfs list) fun      -- 303, hoover, lpFM, superSaw
wf02 i list fun = addPf i 32 "wf02" (toPfs list) fun    -- 303 : 0 is saw, 2 or 10 is square, 12 is triangle

cps i list fun = addPf i 30 "cps" (toPfs list) fun      -- sampler, stutter
sample i list fun = addPf i 29 "sample" (toPfs list) fun    -- sampler, stutter

divs i list fun = addPf i 31 "divs" (toPfs list) fun     -- stutter
pick i list fun = addPf i 32 "pick" (toPfs list) fun     -- stutter
stuts i list fun = addPf i 33 "stuts" (toPfs list) fun    -- stutter

openclose i list fun = addPf i 29 "oc" (toPfs list) fun   -- hihat 808 open/close
tuning i list fun = addPf i 30 "tuning" (toPfs list) fun   -- hihat 808 tuning

rough i list fun = addPf i 30 "rough" (toPfs list) fun    -- karp roughness (0 - 1)
stretch i list fun = addPf i 31 "stretch" (toPfs list) fun  -- karp stretch (0 - 1)

detune i list fun = addPf i 33 "detune" (toPfs list) fun    -- superSaw
sawmix i list fun = addPf i 34 "mix" (toPfs list) fun    -- superSaw

adRatio i list fun = addPf i 32 "adRatio" (toPfs list) fun  -- lpFM, hoover, phax, superSaw, dtmf
fmCar i list fun = addPf i 33 "fmCar" (toPfs list) fun    -- lpFM
fmIndx i list fun = addPf i 35 "fmIndx" (toPfs list) fun   -- lpFM
fmDepth i list fun = addPf i 34 "fmDepth" (toPfs list) fun  -- lpFM

-- adP i list fun =  addPf i 13 "adP" (toPfs list) fun     -- phax
-- simP i list fun =  addPf i 14 "simP" (toPfs list) fun    -- phax
-- wt1 i list fun =  addPf i 15 "wt1" (toPfs list) fun     -- phax
-- wt2 i list fun =  addPf i 16 "wt2" (toPfs list) fun     -- phax
-- wtMix i list fun =  addPf i 17 "wtMix" (toPfs list) fun   -- phax
-- oscTune i list fun =  addPf i 18 "oscTune" (toPfs list) fun -- phax
-- sepP i list fun =  addPf i 19 "sepP" (toPfs list) fun    -- phax
-- modeP i list fun =  addPf i 20 "modeP" (toPfs list) fun   -- phax
-- edP i list fun =  addPf i 21 "edP" (toPfs list) fun     -- phax
-- fbP i list fun =  addPf i 22 "fbP" (toPfs list) fun     -- phax

btn i list fun = addPf i 29 "button" (toPfs list) fun -- dtmf
ampX i list fun = addPf i 31 "ampX" (toPfs list) fun -- dtmf 
ampY i list fun = addPf i 30 "ampY" (toPfs list) fun -- dtmf 

chan i list fun = addPf i 8 "chan" (toPfs list) fun      -- model:cycles
vel i list fun = addPf i 10 "vel" (toPfs list) fun      -- model:cycles

-- Aillen Mixer & General Track Controls
masterFilter i list fun = addPf i 50 "/mixer/master/filter" (toPfs list) fun
masterLimiterGain i list fun = addPf i 51 "/mixer/master/limiter/gain" (toPfs list) fun
masterLimiterRelease i list fun = addPf i 52 "/mixer/master/limiter/release" (toPfs list) fun
masterLimiterCeiling i list fun = addPf i 53 "/mixer/master/limiter/ceiling" (toPfs list) fun
mute i list fun = addPf i 54 "/track/mute" (toPfs list) fun
sidechainSource i list fun = addPf i 55 "/track/sidechain/source" (toPfs list) fun

-- Aillen Return Delay Controls
delayMode i list fun = addPf i 56 "/mixer/return/delay/mode" (toPfs list) fun
delayPingpong i list fun = addPf i 57 "/mixer/return/delay/pingpong" (toPfs list) fun
delayDrive i list fun = addPf i 58 "/mixer/return/delay/drive" (toPfs list) fun
delayGrainSize i list fun = addPf i 59 "/mixer/return/delay/grain_size" (toPfs list) fun
delayDensity i list fun = addPf i 60 "/mixer/return/delay/density" (toPfs list) fun
delaySpray i list fun = addPf i 61 "/mixer/return/delay/spray" (toPfs list) fun
delayPitch i list fun = addPf i 62 "/mixer/return/delay/pitch" (toPfs list) fun

-- Aillen Return Reverb Controls
reverbTime i list fun = addPf i 60 "/mixer/return/reverb/decay" (toPfs list) fun
reverbTone i list fun = addPf i 61 "/mixer/return/reverb/tone" (toPfs list) fun

-- Aillen Track FX Chain Controls
fxFilter i list fun = addPf i 63 "/track/fx/filter/position" (toPfs list) fun
fxRingModMode i list fun = addPf i 64 "/track/fx/ring_mod/mode" (toPfs list) fun
fxRingModSource i list fun = addPf i 65 "/track/fx/ring_mod/source" (toPfs list) fun
fxRingModDepth i list fun = addPf i 66 "/track/fx/ring_mod/depth" (toPfs list) fun
fxRingModFreq i list fun = addPf i 67 "/track/fx/ring_mod/freq" (toPfs list) fun
fxDistMode i list fun = addPf i 68 "/track/fx/distortion/mode" (toPfs list) fun
fxDistDrive i list fun = addPf i 69 "/track/fx/distortion/drive" (toPfs list) fun
fxDistMix i list fun = addPf i 70 "/track/fx/distortion/mix" (toPfs list) fun
fxCompRatio i list fun = addPf i 71 "/track/fx/compressor/ratio" (toPfs list) fun
fxCompThreshold i list fun = addPf i 72 "/track/fx/compressor/threshold" (toPfs list) fun
fxCompAttack i list fun = addPf i 73 "/track/fx/compressor/attack" (toPfs list) fun
fxCompRelease i list fun = addPf i 74 "/track/fx/compressor/release" (toPfs list) fun
fxCompMakeup i list fun = addPf i 75 "/track/fx/compressor/makeup" (toPfs list) fun
fxCompSidechain i list fun = addPf i 76 "/track/fx/compressor/sidechain" (toPfs list) fun

-- Aillen Track FX Chain additions
fxWfDrive i list fun = addPf i 126 "/track/fx/wavefolder/drive" (toPfs list) fun
fxWfFolds i list fun = addPf i 127 "/track/fx/wavefolder/folds" (toPfs list) fun
fxWfSymmetry i list fun = addPf i 128 "/track/fx/wavefolder/symmetry" (toPfs list) fun
fxBcBits i list fun = addPf i 129 "/track/fx/bitcrusher/bits" (toPfs list) fun
fxBcDownsample i list fun = addPf i 130 "/track/fx/bitcrusher/downsample" (toPfs list) fun
fxCombFreq i list fun = addPf i 131 "/track/fx/comb/freq" (toPfs list) fun
fxCombFeedback i list fun = addPf i 132 "/track/fx/comb/feedback" (toPfs list) fun
fxCombDamp i list fun = addPf i 133 "/track/fx/comb/damp" (toPfs list) fun

-- Aillen TwoOp Synth specific parameters
realtime i list fun = addPf i 77 "/track/realtime" (toPfs list) fun
legato i list fun = addPf i 78 "/track/legato" (toPfs list) fun
twopMode i list fun = addPf i 79 "/track/mode" (toPfs list) fun
twopOsc1Waveform i list fun = addPf i 80 "/track/osc1/waveform" (toPfs list) fun
twopOsc2Waveform i list fun = addPf i 81 "/track/osc2/waveform" (toPfs list) fun
twopModParams i list fun = addPf i 82 "/track/mod/params" (toPfs list) fun
twopOsc1Adsr i list fun = addPf i 83 "/track/osc1/adsr" (toPfs list) fun
twopOsc2Adsr i list fun = addPf i 84 "/track/osc2/adsr" (toPfs list) fun
twopFilterAdsr i list fun = addPf i 85 "/track/filter/adsr" (toPfs list) fun
twopFilterParams i list fun = addPf i 86 "/track/filter/params" (toPfs list) fun
twopFilterMod i list fun = addPf i 87 "/track/filter/mod" (toPfs list) fun
twopFeedback i list fun = addPf i 88 "/track/feedback" (toPfs list) fun
twopWavefold i list fun = addPf i 89 "/track/wavefold" (toPfs list) fun
twopNoise i list fun = addPf i 90 "/track/noise" (toPfs list) fun
twopPitchSweep i list fun = addPf i 91 "/track/pitch/sweep" (toPfs list) fun
twopLfo i list fun = addPf i 92 "/track/lfo" (toPfs list) fun

-- Aillen Sampler specific parameters
sampleMode i list fun = addPf i 93 "/track/sample/mode" (toPfs list) fun
samplePitch i list fun = addPf i 94 "/track/sample/pitch" (toPfs list) fun
sampleSpeed i list fun = addPf i 95 "/track/sample/speed" (toPfs list) fun
sampleStretch i list fun = addPf i 96 "/track/sample/mode/stretch" (toPfs list) fun
sampleGrainSize i list fun = addPf i 97 "/track/sample/grain_size" (toPfs list) fun
sampleOverlap i list fun = addPf i 98 "/track/sample/overlap" (toPfs list) fun
sampleFilter i list fun = addPf i 99 "/track/filter" (toPfs list) fun
aillenSliceMode i list fun = addPf i 100 "/track/sample/slice/mode" (toPfs list) fun
aillenSliceCount i list fun = addPf i 101 "/track/sample/slice/count" (toPfs list) fun
aillenSliceSelect i list fun = addPf i 102 "/track/sample/slice/select" (toPfs list) fun
aillenSliceStutter i list fun = addPf i 103 "/track/sample/slice/stutter" (toPfs list) fun
sampleSelect i list fun = addPf i 125 "/track/sample/select" (toPfs list) fun

-- Aillen Synth303 specific parameters
waveform303 i list fun = addPf i 104 "/track/6/303/waveform" (toPfs list) fun
ampAdsr303 i list fun = addPf i 105 "/track/6/303/amp/adsr" (toPfs list) fun
filterAdsr303 i list fun = addPf i 106 "/track/6/303/filter/adsr" (toPfs list) fun
pitchAdsr303 i list fun = addPf i 107 "/track/6/303/pitch/adsr" (toPfs list) fun
filter303 i list fun = addPf i 108 "/track/6/303/filter/params" (toPfs list) fun
filterMod303 i list fun = addPf i 109 "/track/6/303/filter/mod" (toPfs list) fun
pitchMod303 i list fun = addPf i 110 "/track/6/303/pitch/mod" (toPfs list) fun
pwm303 i list fun = addPf i 111 "/track/6/303/pwm/params" (toPfs list) fun
glide303 i list fun = addPf i 112 "/track/6/303/glide" (toPfs list) fun
legato303 i list fun = addPf i 113 "/track/6/303/legato" (toPfs list) fun

-- Aillen SynthHubass specific parameters
ampAdsrHubass i list fun = addPf i 114 "/track/7/hubass/amp/adsr" (toPfs list) fun
hubassFilterParams i list fun = addPf i 115 "/track/7/hubass/filter/params" (toPfs list) fun
unison i list fun = addPf i 116 "/track/7/hubass/osc/unison" (toPfs list) fun
subHubass i list fun = addPf i 117 "/track/7/hubass/osc/sub" (toPfs list) fun
noiseHubass i list fun = addPf i 118 "/track/7/hubass/osc/noise" (toPfs list) fun
hubassFilterMode i list fun = addPf i 119 "/track/7/hubass/filter/mode" (toPfs list) fun
hubassDriveMode i list fun = addPf i 120 "/track/7/hubass/drive/mode" (toPfs list) fun
hubassLfo1 i list fun = addPf i 121 "/track/7/hubass/lfo/1" (toPfs list) fun
chorusHubass i list fun = addPf i 122 "/track/7/hubass/chorus/params" (toPfs list) fun
legatoHubass i list fun = addPf i 123 "/track/7/hubass/legato" (toPfs list) fun
gainHubass i list fun = addPf i 124 "/track/7/hubass/gain" (toPfs list) fun

-- mc_pitch i list fun = addPf i 11 "pitch" (toPfs list) fun -- model:cycles
-- decay i list fun = addPf i 12 "decay" (toPfs list) fun    -- model:cycles
-- color i list fun = addPf i 13 "color" (toPfs list) fun    -- model:cycles
-- shape i list fun = addPf i 14 "shape" (toPfs list) fun    -- model:cycles
-- sweep i list fun = addPf i 15 "sweep" (toPfs list) fun    -- model:cycles
-- contour i list fun =  addPf i 16 "contour" (toPfs list) fun -- model:cycles

silence = stopAll perf
playA = playAll perf
solo = soloIns perf

runKitMarkov csv kit = runKitMarkovR csv kit (resolvePfield perf)

-- fbdel = setChannel csd1 "fbdel"
-- delfb i list fun =  addPf i 3 "fbdel" (toPfs list) fun -- delay fb
fbdel = sendAillenParam aillenPort "/mixer/return/delay/feedback"
delfb i list fun =  addPf i 3 "/mixer/return/delay/feedback" (toPfs list) fun

-- dtdel = setChannel csd1 "dtdel"
-- delt i list fun =  addPf i 2 "dtdel" (toPfs list) fun -- delay time
dtdel = sendAillenParam aillenPort "/mixer/return/delay/time"
delt i list fun =  addPf i 2 "/mixer/return/delay/time" (toPfs list) fun

-- voldel = setChannel csd1 "voldel"
-- delvol i list fun =  addPf i 1 "voldel" (toPfs list) fun -- delay volume

-- fbrev = setChannel csd1 "fbrev"
-- revfb i list fun =  addPf i 3 "fbrev" (toPfs list) fun -- rev fb
-- cfrev = setChannel csd1 "cfrev"
-- revcf i list fun =  addPf i 2 "cfrev" (toPfs list) fun -- rev cf
-- volrev = setChannel csd1 "volrev"
-- revvol i list fun =  addPf i 1 "volrev" (toPfs list) fun -- rev volume

-- volchorus = setChannel csd1 "volchorus"
-- chorvol i list fun =  addPf i 1 "volchorus" (toPfs list) fun -- chorus volume
-- delchorus = setChannel csd1 "delchorus"
-- chordt i list fun =  addPf i 2 "delchorus" (toPfs list) fun -- chorus delay time
-- divchorus = setChannel csd1 "divchorus"
-- chordiv i list fun =  addPf i 3 "divchorus" (toPfs list) fun -- chorus divisor

-- m_vol = setChannel csd1 "m_vol"
-- mixvol i list fun =  addPf i 1 "m_vol" (toPfs list) fun -- mix volume
m_vol = sendAillenParam aillenPort "/mixer/master/volume"
mixvol i list fun =  addPf i 1 "/mixer/master/volume" (toPfs list) fun

-- wl = setChannel csd1 "wl"
-- wlvol i list fun =  addPf i 2 "wl" (toPfs list) fun -- waveloss volume
wl = sendAillenParam aillenPort "/mixer/master/waveloss/mode"
wlvol i list fun =  addPf i 2 "/mixer/master/waveloss/mode" (toPfs list) fun

-- dropwl = setChannel csd1 "dropwl"
-- wldrop i list fun =  addPf i 3 "dropwl" (toPfs list) fun -- waveloss drop
dropwl = sendAillenParam aillenPort "/mixer/master/waveloss/drop"
wldrop i list fun =  addPf i 3 "/mixer/master/waveloss/drop" (toPfs list) fun

-- maxwl = setChannel csd1 "maxwl"
-- wlmax i list fun =  addPf i 4 "maxwl" (toPfs list) fun -- waveloss max
maxwl = sendAillenParam aillenPort "/mixer/master/waveloss/outof"
wlmax i list fun =  addPf i 4 "/mixer/master/waveloss/outof" (toPfs list) fun

-- voluzu = setChannel csd1 "voluzu"
-- uzuvol i list fun = addPf i 1 "voluzu" (toPfs list) fun
-- uzuwidth = setChannel csd1 "uzuwidth"
-- uzuwd i list fun = addPf i 2 "uzuwidth" (toPfs list) fun
-- uzuoffset = setChannel csd1 "uzuoffset"
-- uzuoff i list fun = addPf i 3 "uzuoffset" (toPfs list) fun
-- uzudepth = setChannel csd1 "uzudepth"
-- uzudp i list fun = addPf i 4 "uzudepth" (toPfs list) fun
-- uzuspeed = setChannel csd1 "uzuspeed"
-- uzusp i list fun = addPf i 5 "uzuspeed" (toPfs list) fun
-- uzublur = setChannel csd1 "uzublur"
-- uzubl i list fun = addPf i 6 "uzublur" (toPfs list) fun
-- uzumix = setChannel csd1 "uzumix"
-- uzumx i list fun = addPf i 7 "uzumix" (toPfs list) fun
-- uzuhzmode = setChannel csd1 "uzuhzmode"
-- uzuhz i list fun = addPf i 8 "uzuhzmode" (toPfs list) fun
-- uzubass = setChannel csd1 "uzubass"
-- uzubs i list fun = addPf i 9 "uzubass" (toPfs list) fun
-- uzuspread = setChannel csd1 "uzuspread"
-- uzuspr i list fun = addPf i 10 "uzuspread" (toPfs list) fun

techno1 k s h = cPat "fourFloor" k >> cPat "downB" s >> cPat "upFour" h
dnb1 k s h = cPat "dbk" k >> cPat "downB" s >> cPat "eightN" h

runPfield i (fun,upd,list) = fun i list upd
prms i ls = mapM_ (runPfield i) ls

-- | Shorthand for Kit remapping with current performance resolver
withK' kit upd = withK kit (resolvePfield perf) upd
