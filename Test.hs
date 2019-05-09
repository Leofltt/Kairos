-- create Sampler instruments

defPath s = "/Users/leofltt/Desktop/KairosSamples" ++ s
addI "sh" $ sampler $ defPath "/ch/shortHat.wav"
addI "r1" $ sampler $ defPath "/rim/HollowRim.wav"
addI "rS" $ sampler $ defPath "/rim/SmallRim.wav"
addI "sS" $ sampler $ defPath "/snares/SNSandy.wav"
addI "sj1" $ sampler $ defPath "/snares/Snare4JungleMidHigh.wav"
addI "sj2" $ sampler $ defPath "/snares/Snare4JungleMidLow.wav"
addI "glass" $ sampler $ defPath "/fracture/Glass1Dry.wav"
addI "st1" $ sampler $ defPath "/stomps/stomp1.wav"
addI "st2" $ sampler $ defPath "/stomps/stomp2.wav"
addI "st3" $ sampler $ defPath "/stomps/stomp3.wav"
fs n string | n <= 0 = [] | otherwise = string ++ " " ++ fs (n-1) string

--TEST PERFORMANCE
----------------------

displayIns
displayTP

addC "st2" "stompy" $ toTP [0.75, 2.25]

cPat "eightN" "st1"
vol "st1" [Pd 0.3] keep
p "st1"

del "st2" (toPfD [0.0]) keep

dtdel 666
fbdel 0.9

p "CH808"

p "st2"
p "K909"
pan"K909" [Pd 0, Pd 0, Pd 1] retrograde
pan "st2" [Pd 1] keep

cT 143

silence

addC "K909" "testA" $ toTP $ evolve 1 (interp1 4) (fromTP jGhost1)

cPat "sixteenN" "303"
vol "303" [Pd 0.51] keep
cf "303" [Pd 9000] keep
rev "303" [Pd 0.4] keep
p "303"

cPat "downB" "sS"
p "sS"
del "sS" [Pd 0.8] keep


del "K909" [Pd 0.8] keep

addC "karp" "p1" $ toTP $ [0.25, 1.75, 2.75]
rough "karp" [Pd 0.5] randomize
stretch "karp" [Pd 0.1,  Pd 0.2] nextVal
freq "karp" (toPfD [36, 44, 48]) retrograde
vol "karp" [Pd 0.2] keep
del "karp" [Pd 0.8] keep
p "karp"

cPat "eightN" "hov"
freq "hov" (toPfD [36, 40, 43, 36]) randomize
dur "hov" [Pd 0.4] keep
cf "hov" [Pd 10000, Pd 12000] retrograde
res "hov" [Pd 8, Pd 10, Pd 15] retrograde
vol "hov" [Pd 0.8] keep
p "hov"


s "karp"
