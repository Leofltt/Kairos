--inits && useful
------------------

:set prompt ""
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
rev i list fun = addPf' i 5 list fun
del i list fun = addPf' i 6 list fun
dur i list fun = addPf' i 3 list fun
freq i list fun = addPf' i 7 list fun   -- 303, hoover
cf i list fun = addPf' i 8 list fun     -- 303, hoover
res i list fun = addPf' i 9 list fun    -- 303, hoover
cps i list fun = addPf' i 8 list fun    -- sampler
sample i list fun = addPf' i 7 list fun -- sampler
silence = stopAll perf
playA = playAll perf
playFx = playEffect perf
solo = soloIns perf
fs n string | n <= 0 = [] | otherwise = string ++ " " ++ fs (n-1) string
defPath s = "/Users/leofltt/Desktop/KairosSamples" ++ s
addI "sh" $ sampler $ defPath "/ch/shortHat.wav"
addI "r1" $ sampler $ defPath "/rim/HollowRim.wav"
addI "rS" $ sampler $ defPath "/rim/SmallRim.wav"
addI "sS" $ sampler $ defPath "/snares/SNSandy.wav"
addI "sj1" $ sampler $ defPath "/snares/Snare4JungleMidHigh.wav"
addI "sj2" $ sampler $ defPath "/snares/Snare4JungleMidLow.wav"
addI "glass" $ sampler $ defPath "/fracture/Glass1Dry.wav"
mapM_ playFx ["rev","del"]

:! clear


--TEST PERFORMANCE
----------------------
