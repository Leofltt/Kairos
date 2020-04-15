--init the library

import Kairos.Lib

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
addP = addTPf perf
addIns = addInstrument perf
addI name ins = addIns name =<< ins
addC i n s = addP n s >> cPat n i
getTP = getTimePoint perf
addPf' = addPfPath' perf
addPf i pfnum list fun = addPf' i pfnum =<< createPfPat pfnum list fun
dur i list fun = addPf i 3 list fun      -- global
d' i fun list = dur i list fun
durTS i list fun = d' i fun  =<< withTimeSignature perf list
vol i list fun = addPf i 4 list fun
rev i list fun = addPf i 5 list fun
del i list fun = addPf i 6 list fun
pan i list fun = addPf i 7 list fun
chorus i list fun = addPf i 8 list fun
pitch i list fun = addPf i 9 list fun    -- 303, hoover, karp, lpFM, superSaw, stringPad
cf i list fun = addPf i 10 list fun       -- 303, hoover, lpFM, superSaw
res i list fun = addPf i 11 list fun     -- 303, hoover, lpFM, superSaw
wf02 i list fun = addPf i  12 list fun      -- 303 : 0 is saw, 2 or 10 is square, 12 is triangle
cps i list fun = addPf i 10 list fun      -- sampler
sample i list fun = addPf i 9 list fun   -- sampler
tune i list fun = addPf i 10 list fun     -- hihat 808 tuning
rough i list fun = addPf i 10 list fun    -- karp roughness (0 - 1)
stretch i list fun = addPf i 11 list fun -- karp stretch (0 - 1)
adRatio i list fun = addPf i 12 list fun -- lpFM, hoover
dist i list fun = addPf i 13 list fun    -- lpFM
fmIndx i list fun = addPf i 15 list fun  -- lpFM
fmDepth i list fun = addPf i 14 list fun -- lpFM
tresh i list fun = addPf i 11 list fun -- sampler, stutter
ratio i list fun = addPf i 12 list fun -- sampler, stutter 
divs i list fun = addPf i 10 list fun  -- stutter
pick i list fun = addPf i 13 list fun  -- stutter
stuts i list fun = addPf i 14 list fun -- stutter
silence = stopAll perf
playA = playAll perf
playFx = playEffect perf
solo = soloIns perf
fbdel = setChannel "fbdel"
dtdel = setChannel "dtdel"
fbrev = setChannel "fbrev"
cfrev = setChannel "cfrev"
volrev = setChannel "volrev"
voldel = setChannel "voldel"
volchorus = setChannel "volchorus"
delchorus = setChannel "delchorus"
divchorus = setChannel "divchorus"
m_vol = setChannel "m_vol"
mapM_ playFx ["rev","del","chorus","mix"]
techno1 k s h = cPat "fourFloor" k >> cPat "downB" s >> cPat "upFour" h
dnb1 k s h = cPat "dbk" k >> cPat "downB" s >> cPat "eightN" h
runPfield i (a,b,c) = b i c a
params i ls = mapM_ (runPfield i) ls
