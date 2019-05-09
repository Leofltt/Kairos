--init the library

import Kairos.Lib

:set prompt "> "
perf <- defaultPerformance
displayIns = displayInstruments perf
p = play perf
s = stop perf
cPat p i = changeTimeF perf i p
cT = changeTempo (clock perf)
addP = addTPf perf
addIns = addInstrument perf
addI name ins = addIns name =<< ins
addC i n s = addP n s >> cPat n i
addPf = addPfPath' perf
addPf' i pfnum list fun = addPf i pfnum =<< createPfPat pfnum list fun
vol i list fun = addPf' i 4 list fun    -- global
pan i list fun = addPf' i 7 list fun
rev i list fun = addPf' i 5 list fun
del i list fun = addPf' i 6 list fun
dur i list fun = addPf' i 3 list fun
freq i list fun = addPf' i 8 list fun    -- 303, hoover, karp
cf i list fun = addPf' i 9 list fun      -- 303, hoover
res i list fun = addPf' i 10 list fun     -- 303, hoover
cps i list fun = addPf' i 9 list fun     -- sampler
sample i list fun = addPf' i 8 list fun  -- sampler
tune i list fun = addPf' i 9 list fun    -- hihat 808 tuning
rough i list fun = addPf' i 9 list fun   -- karp roughness (0 - 1)
stretch i list fun = addPf' i 10 list fun -- karp stretch (0 - 1)
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
mapM_ playFx ["rev","del"]
