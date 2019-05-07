-- create Sampler instruments
defPath s = "/Users/leofltt/Desktop/KairosSamples" ++ s
addI "sh" $ sampler $ defPath "/ch/shortHat.wav"
addI "r1" $ sampler $ defPath "/rim/HollowRim.wav"
addI "rS" $ sampler $ defPath "/rim/SmallRim.wav"
addI "sS" $ sampler $ defPath "/snares/SNSandy.wav"
addI "sj1" $ sampler $ defPath "/snares/Snare4JungleMidHigh.wav"
addI "sj2" $ sampler $ defPath "/snares/Snare4JungleMidLow.wav"
addI "glass" $ sampler $ defPath "/fracture/Glass1Dry.wav"
fs n string | n <= 0 = [] | otherwise = string ++ " " ++ fs (n-1) string

--TEST PERFORMANCE
----------------------


cT 143

silence

addC "K909" "testA" $ toTP $ evolve 1 (interp1 4) (fromTP jGhost1)


s "K909"

cPat "downB" "sS"
p "sS"
del "sS" [Pd 0.8] keep

del "K909" [Pd 0.8] keep
voldel 1
fbdel 0.9

dtdel 500

fbrev 0.9
cfrev 20000

addC "karp" "p1" $ toTP $ [0.25, 1.75, 2.75]
rough "karp" [Pd 0.5, Pd 0.1] randomize
stretch "karp" [Pd 0.1, Pd 0.5, Pd 0.2] nextVal
freq "karp" (toPfD [36, 44, 48]) rev
vol "karp" [Pd 0.5] keep
p "karp"

cPat "sixteenN" "303"
freq "303" (toPfD [36, 40, 43, 36]) randomize
dur "303" [Pd 0.4] keep
cf "303" [Pd 10000, Pd 12000] rev
res "303" [Pd 8, Pd 10, Pd 15] randomize
vol "303" [Pd 4]
p "303"


freq "hov" (toPfD $ [ 50, 55 ] ) randomize

s "karp"

cf "hov" (toPfD $ [4000]) keep
res "hov" (toPfD $ [5, 8]) randomize
dur "hov" [Pd 1.666] keep
rev "hov" [Pd 0.4] keep
