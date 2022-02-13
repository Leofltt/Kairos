import Kairos.Lib

-- | init the library

csd1 = "11000"
:set prompt "κ> "
perf <- defaultPerformance
displayIns = displayInstruments perf
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
-- common parameters
dur i list fun = addPf i 3 "dur" list fun      -- in seconds
d' i fun list = dur i list fun
durTS i list fun = d' i fun  =<< withTimeSignature perf list
vol i list fun = addPf i 4 "vol" list fun      -- time signature dependent
rev i list fun = addPf i 5 "rev" list fun
del i list fun = addPf i 6 "del" list fun
pan i list fun = addPf i 7 "pan" list fun
chorus i list fun = addPf i 8 "chorus" list fun
-- instrument specific parameters (try to keep the consistent for ease of use)
pitch i list fun = addPf i 9 "pitch" list fun     -- 303, hoover, karp, lpFM, superSaw, stringPad
cf i list fun = addPf i 10 "cf" list fun       -- 303, hoover, lpFM, superSaw
res i list fun = addPf i 11 "res" list fun      -- 303, hoover, lpFM, superSaw
wf02 i list fun = addPf i  12 "wf02" list fun    -- 303 : 0 is saw, 2 or 10 is square, 12 is triangle
cps i list fun = addPf i 10 "cps" list fun      -- sampler, stutter
sample i list fun = addPf i 9 "sample" list fun    -- sampler, stutter
tresh i list fun = addPf i 11 "tresh" list fun    -- sampler, stutter
ratio i list fun = addPf i 12 "ratio" list fun    -- sampler, stutter
divs i list fun = addPf i 13 "divs" list fun     -- stutter
pick i list fun = addPf i 14 "pick" list fun     -- stutter
stuts i list fun = addPf i 15 "stuts" list fun    -- stutter
tuning i list fun = addPf i 10 "tuning" list fun   -- hihat 808 tuning
rough i list fun = addPf i 10 "rough" list fun    -- karp roughness (0 - 1)
stretch i list fun = addPf i 11 "stretch" list fun  -- karp stretch (0 - 1)
detune i list fun = addPf i 13 "detune" list fun    -- superSaw
sawmix i list fun = addPf i 14 "mix" list fun    -- superSaw
adRatio i list fun = addPf i 12 "adRatio" list fun  -- lpFM, hoover, phax, superSaw
fmCar i list fun = addPf i 13 "fmCar" list fun    -- lpFM
fmIndx i list fun = addPf i 15 "fmIndx" list fun   -- lpFM
fmDepth i list fun = addPf i 14 "fmDepth" list fun  -- lpFM

adP i list fun =  addPf i 13 "adP" list fun     -- phax
simP i list fun =  addPf i 14 "simP" list fun    -- phax
wt1 i list fun =  addPf i 15 "wt1" list fun     -- phax
wt2 i list fun =  addPf i 16 "wt2" list fun     -- phax
wtMix i list fun =  addPf i 17 "wtMix" list fun   -- phax
oscTune i list fun =  addPf i 18 "oscTune" list fun -- phax
sepP i list fun =  addPf i 19 "sepP" list fun    -- phax
modeP i list fun =  addPf i 20 "modeP" list fun   -- phax
edP i list fun =  addPf i 21 "edP" list fun     -- phax
fbP i list fun =  addPf i 22 "fbP" list fun     -- phax

chan i list fun = addPf i 8 "chan" list fun      -- model:cycles
note i list fun = addPf i 9 "note" list fun      -- model:cycles
vel i list fun = addPf i 10 "vel" list fun      -- model:cycles
mc_pitch i list fun = addPf i 11 "pitch" list fun -- model:cycles
decay i list fun = addPf i 12 "decay" list fun    -- model:cycles
color i list fun = addPf i 13 "color" list fun    -- model:cycles
shape i list fun = addPf i 14 "shape" list fun    -- model:cycles
sweep i list fun = addPf i 15 "sweep" list fun    -- model:cycles
contour i list fun =  addPf i 16 "contour" list fun -- model:cycles

silence = stopAll perf
playA = playAll perf
solo = soloIns perf

fbdel = setChannel csd1 "fbdel"
delfb i list fun =  addPf i 3 "fbdel" list fun -- delay fb
dtdel = setChannel csd1 "dtdel"
delt i list fun =  addPf i 2 "dtdel" list fun -- delay time
voldel = setChannel csd1 "voldel"
delvol i list fun =  addPf i 1 "voldel" list fun -- delay volume

fbrev = setChannel csd1 "fbrev"
revfb i list fun =  addPf i 3 "fbrev" list fun -- rev fb
cfrev = setChannel csd1 "cfrev"
revcf i list fun =  addPf i 2 "cfrev" list fun -- rev cf
volrev = setChannel csd1 "volrev"
revvol i list fun =  addPf i 1 "volrev" list fun -- rev volume

volchorus = setChannel csd1 "volchorus"
chorvol i list fun =  addPf i 1 "volchorus" list fun -- chorus volume
delchorus = setChannel csd1 "delchorus"
chort i list fun =  addPf i 2 "delchorus" list fun -- chorus delay time
divchorus = setChannel csd1 "divchorus"
chordiv i list fun =  addPf i 3 "divchorus" list fun -- chorus divisor

m_vol = setChannel csd1 "m_vol"
mixvol i list fun =  addPf i 1 "m_vol" list fun -- mix volume

techno1 k s h = cPat "fourFloor" k >> cPat "downB" s >> cPat "upFour" h
dnb1 k s h = cPat "dbk" k >> cPat "downB" s >> cPat "eightN" h

runPfield i (b,a,c) = b i c a
prms i ls = mapM_ (runPfield i) ls

