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
dur i list fun = addPf i 3 "dur" (toPfs list) fun      -- in seconds
d' i fun list = dur i (fromPfsD list) fun
durTS i list fun = d' i fun  =<< withTimeSignature perf (toPfs list)
vol i list fun = addPf i 4 "vol" (toPfs list) fun      -- time signature dependent
rev i list fun = addPf i 5 "rev" (toPfs list) fun
del i list fun = addPf i 6 "del" (toPfs list) fun
pan i list fun = addPf i 7 "pan" (toPfs list) fun
chorus i list fun = addPf i 8 "chorus" list fun
-- instrument specific parameters (try to keep the consistent for ease of use)
pitch i list fun = addPf i 9 "pitch" (toPfs list) fun     -- 303, hoover, karp, lpFM, superSaw, stringPad
cf i list fun = addPf i 10 "cf" (toPfs list) fun       -- 303, hoover, lpFM, superSaw
res i list fun = addPf i 11 "res" (toPfs list) fun      -- 303, hoover, lpFM, superSaw
wf02 i list fun = addPf i  12 "wf02" (toPfs list) fun    -- 303 : 0 is saw, 2 or 10 is square, 12 is triangle
cps i list fun = addPf i 10 "cps" (toPfs list) fun      -- sampler, stutter
sample i list fun = addPf i 9 "sample" (toPfs list) fun    -- sampler, stutter
tresh i list fun = addPf i 11 "tresh" (toPfs list) fun    -- sampler, stutter
ratio i list fun = addPf i 12 "ratio" (toPfs list) fun    -- sampler, stutter
divs i list fun = addPf i 13 "divs" (toPfs list) fun     -- stutter
pick i list fun = addPf i 14 "pick" (toPfs list) fun     -- stutter
stuts i list fun = addPf i 15 "stuts" (toPfs list) fun    -- stutter
tuning i list fun = addPf i 10 "tuning" (toPfs list) fun   -- hihat 808 tuning
rough i list fun = addPf i 10 "rough" (toPfs list) fun    -- karp roughness (0 - 1)
stretch i list fun = addPf i 11 "stretch" (toPfs list) fun  -- karp stretch (0 - 1)
detune i list fun = addPf i 13 "detune" (toPfs list) fun    -- superSaw
sawmix i list fun = addPf i 14 "mix" (toPfs list) fun    -- superSaw
adRatio i list fun = addPf i 12 "adRatio" (toPfs list) fun  -- lpFM, hoover, phax, superSaw
fmCar i list fun = addPf i 13 "fmCar" (toPfs list) fun    -- lpFM
fmIndx i list fun = addPf i 15 "fmIndx" (toPfs list) fun   -- lpFM
fmDepth i list fun = addPf i 14 "fmDepth" (toPfs list) fun  -- lpFM

adP i list fun =  addPf i 13 "adP" (toPfs list) fun     -- phax
simP i list fun =  addPf i 14 "simP" (toPfs list) fun    -- phax
wt1 i list fun =  addPf i 15 "wt1" (toPfs list) fun     -- phax
wt2 i list fun =  addPf i 16 "wt2" (toPfs list) fun     -- phax
wtMix i list fun =  addPf i 17 "wtMix" (toPfs list) fun   -- phax
oscTune i list fun =  addPf i 18 "oscTune" (toPfs list) fun -- phax
sepP i list fun =  addPf i 19 "sepP" (toPfs list) fun    -- phax
modeP i list fun =  addPf i 20 "modeP" (toPfs list) fun   -- phax
edP i list fun =  addPf i 21 "edP" (toPfs list) fun     -- phax
fbP i list fun =  addPf i 22 "fbP" (toPfs list) fun     -- phax

chan i list fun = addPf i 8 "chan" (toPfs list) fun      -- model:cycles
note i list fun = addPf i 9 "note" (toPfs list) fun      -- model:cycles
vel i list fun = addPf i 7 "vel" (toPfs list) fun      -- model:cycles
mc_pitch i list fun = addPf i 11 "pitch" (toPfs list) fun -- model:cycles
decay i list fun = addPf i 12 "decay" (toPfs list) fun    -- model:cycles
color i list fun = addPf i 13 "color" (toPfs list) fun    -- model:cycles
shape i list fun = addPf i 14 "shape" (toPfs list) fun    -- model:cycles
sweep i list fun = addPf i 15 "sweep" (toPfs list) fun    -- model:cycles
contour i list fun =  addPf i 16 "contour" (toPfs list) fun -- model:cycles

silence = stopAll perf
playA = playAll perf
solo = soloIns perf

fbdel = setChannel csd1 "fbdel"
delfb i list fun =  addPf i 3 "fbdel" (toPfs list) fun -- delay fb
dtdel = setChannel csd1 "dtdel"
delt i list fun =  addPf i 2 "dtdel" (toPfs list) fun -- delay time
voldel = setChannel csd1 "voldel"
delvol i list fun =  addPf i 1 "voldel" (toPfs list) fun -- delay volume

fbrev = setChannel csd1 "fbrev"
revfb i list fun =  addPf i 3 "fbrev" (toPfs list) fun -- rev fb
cfrev = setChannel csd1 "cfrev"
revcf i list fun =  addPf i 2 "cfrev" (toPfs list) fun -- rev cf
volrev = setChannel csd1 "volrev"
revvol i list fun =  addPf i 1 "volrev" (toPfs list) fun -- rev volume

volchorus = setChannel csd1 "volchorus"
chorvol i list fun =  addPf i 1 "volchorus" (toPfs list) fun -- chorus volume
delchorus = setChannel csd1 "delchorus"
chort i list fun =  addPf i 2 "delchorus" (toPfs list) fun -- chorus delay time
divchorus = setChannel csd1 "divchorus"
chordiv i list fun =  addPf i 3 "divchorus" (toPfs list) fun -- chorus divisor

m_vol = setChannel csd1 "m_vol"
mixvol i list fun =  addPf i 1 "m_vol" (toPfs list) fun -- mix volume

wl = setChannel csd1 "wl"
wlvol i list fun =  addPf i 2 "wl" (toPfs list) fun -- waveloss volume
dropwl = setChannel csd1 "dropwl"
wldrop i list fun =  addPf i 3 "dropwl" (toPfs list) fun -- waveloss drop
maxwl = setChannel csd1 "maxwl"
wlmax i list fun =  addPf i 4 "maxwl" (toPfs list) fun -- waveloss max

techno1 k s h = cPat "fourFloor" k >> cPat "downB" s >> cPat "upFour" h
dnb1 k s h = cPat "dbk" k >> cPat "downB" s >> cPat "eightN" h

runPfield i (b,a,c) = b i c a
prms i ls = mapM_ (runPfield i) ls

