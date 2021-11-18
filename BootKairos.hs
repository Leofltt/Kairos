--init the library

import Kairos.Lib

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
addP = addTPf perf
addIns = addInstrument perf
addI name ins = addIns name =<< ins
maybeAddC i n s = maybeAddP n s >> cPat n i
addC i n s = addP n s >> cPat n i
getTP = getTimePoint perf
addPf' = addPfPath' perf
addPf i pfnum pfname list fun = addPf' i pfnum =<< createPfPat pfnum pfname list fun
dur i list fun = addPf i 3 "dur" list fun      -- global
d' i fun list = dur i list fun
durTS i list fun = d' i fun  =<< withTimeSignature perf list
vol i list fun = addPf i 4 "vol" list fun
rev i list fun = addPf i 5 "rev" list fun
del i list fun = addPf i 6 "del" list fun
pan i list fun = addPf i 7 "pan" list fun
chorus i list fun = addPf i 8 "chorus" list fun
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
width i list fun = addPf i 13 "width" list fun    -- superSaw
adRatio i list fun = addPf i 12 "adRatio" list fun  -- lpFM, hoover, phax
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
-- playFx = playEffect perf | DEPRECATED : Fxs are started by Csound on startup and run forever
solo = soloIns perf
fbdel = setChannel csd1 "fbdel"
dtdel = setChannel csd1 "dtdel"
fbrev = setChannel csd1 "fbrev"
cfrev = setChannel csd1 "cfrev"
volrev = setChannel csd1 "volrev"
voldel = setChannel csd1 "voldel"
volchorus = setChannel csd1 "volchorus"
delchorus = setChannel csd1 "delchorus"
divchorus = setChannel csd1 "divchorus"
m_vol = setChannel csd1 "m_vol"
techno1 k s h = cPat "fourFloor" k >> cPat "downB" s >> cPat "upFour" h
dnb1 k s h = cPat "dbk" k >> cPat "downB" s >> cPat "eightN" h
runPfield i (a,b,c) = b i c a
params i ls = mapM_ (runPfield i) ls
