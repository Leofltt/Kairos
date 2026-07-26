--  

cT 147

--

fbdel $ Pd 0.3
dtdel $ Pd 227
divchorus $ Pd 4
delchorus $ Pd 44
volchorus $ Pd 0.4

volrev $ Pd 0.88


fbrev $ Pd 0.81
cfrev $ Pd 11888


addC "hov" "ddb" $ [0,8,12]

prms "hov" [(durTS, nv, [8, 4, 4]),(pitch, nv, [48, 52, 56])]


rev "hov" [0.6] k 

cf "hov" [2000] k 

--

fbdel $ Pd 0.3
dtdel $ Pd 227
divchorus $ Pd 4
delchorus $ Pd 44
volchorus $ Pd 0.4
volrev $ Pd 0.88


prms "mp" [(ringMod, k, [0.4]), (compHard, k, [0.8]), (compThreshDb, k, [(-60)]), (rmFreq, k, [20]), (hpFreq, k, [500]),(lpFreq, k, [7000]), (rev, rnd, [0.4, 0.6]), (del, k, [0.1]), (dist, k, [0.8]), (distPostGain, k, [1.66]), (distChar, rnd, [0.2, 0.4, 0.77]) ]

pn "mp"

addC "cp808d" "cp" $ [2.5] 

prms "cp808d" [(pan, rnd, [0.2, 0.6, 0.8, 0.4]), (del, rnd, [0.2, 0, 0.3])]

cPat "eightN" "mys"
cPat "eightN" "ohbre"
cPat "eightN" "horn"
cPat "eightN" "orb"

prms "mys" [(cps, k, [(-1)]),(durTS, k, [(1/8)]), (stuts, k, [1]), (divs,k,[16]),(pick,nv,[0,1..15])]

p "mys"

addC "cp808d" "tre" $ fmap (+2) uno

prms "orb" [(durTS, k, [(1/8)]), (stuts, k, [1]), (divs,k,[16]),(pick,nv,[0,1..15]),(cps, k, [(-1)])] 
prms "ohbre" [(durTS, k, [(1/8)]), (stuts, k, [1]), (divs,k,[16]),(pick,nv,[0,1..15]),(cps, k, [(-1)])] 
prms "horn" [(durTS, k, [(1/8)]), (stuts, k, [1]), (divs,k,[16]),(pick,nv,[0,1..15]),(cps, k, [(-1)])]


addC "dtmf" "dtmeu_" $ euclid (13, 64) 4 32


prms "dtmf" [(adRatio, nv,[0.4, 0.7,0.2]), (ringMod, k, [0]), (rmFreq, rnd, [300, 180, 300]), (durTS, k, [0.5]), (vol, k, [0.4])]

addC "snare


btn "dtmf" ["#"] $ nv 

s "dtmf"

cPat "fourFloor" "K909" 
p "K909"

addC "wrdsn" "eu_" $ euclid (2, 19) 4 32  

prms "wrdsn" [(del, np 66, [0.2, 0.4, 0.01, 0.3, 0.18] )]

p "wrdsn"

 --  m
 --  [62,65,69,72]
 --  [57,57,55,53]
 --  [50, 53, 57, 53]
 --  [57, 60]
 --
 --
 --  b 
 --
 -- [38, 36]
 -- [38, 45, 48]
 -- [38, 43]
 -- [38, 38, 41, 43, 41, 38, 38]
 --

prms "karp" [vol, k,[0.2]),(durTS, nv, [(1/2), (1/4), (1/4)]), (rough, k, [0.3]),(stretch, k, [0.73])]



addC "karp" "w1" $ textToTP 64 "cuneo"

addC "karp" "w2" $ []
pitch "karp" [38, 45, 38] nv

p "karp" 


cPat "sixteenN" "shatcs"

prms "shatcs" [(vol, rnd, [0.8, 0.7, 0.77, 0.75])]

prms "lpFM" [ (fmIndx, k, [2.3141568]),(fmDepth, nv, [1.171717]),(pitch, nv, [38,41,45,48]), (adRatio, rnd, [0.2, 0.4, 0.6]), (durTS, rnd, [0.25, 0.4, 0.77])]

cPat "uno" "lpFM" >> 

s "lpFM"

cPat "upFour" "CH808"

prms "CH808" [(lpFreq, k, [13000]), (hpFreq, k, [700])]

p "CH808"

prms "303" [(pitch, k, [50,50,53, 50 ]),(cf, nv, [ 2000, 5000, 3000, 6000, 1800]), (dist, rnd, [0.2, 0.4, 0]), (durTS, k, [(1/16)])]


cPat "sixteenN" "303" 
p "303"

prms "sSaw" [(vol, k,[0.2]), (detune, k, [0.2313]), (sawmix, k, [0.95]), (dist, k, [0.3]),(del, k, [0.23]),(lpFreq, k, [17000]),(hpFreq, k, [1200]),(adRatio, nv, [0.4, 0.6, 0.3, 0.7]), (durTS, rnd, 0.2:(take 8 [0.1, 0.1 ..])), (del, k, [0.4]), (chorus, k, [0.3])]

pitch "sSaw" [] nv

cPat "dubb" "sSaw"


detune "sSaw" [0.2313, 0.5, 0.11, 0.3] rnd

cPat "irsn" "snare41"

prms "snare41" [(vol, k, [0.8])]


s "karp"

cPat "uno" "hov"
p "hov"

prms "r707" [(chorus, rnd, [0.8, 0.3, 0.2, 0.66]),(ringMod, rnd, [0.6, 0.4, 0.7]),(rmFreq, rnd, [30, 15, 50]), (hpFreq, k, [388])]

cPat "ukgch" "r707"
p "r707"

addC "cp808m" "wpwwd" =<< patternWithDensity 16 32  47

-- bongos 2, 5, 4

prms "spaceperc2" [(vol, rnd , [0.8, 0, 0.2, 0.6]), (hpFreq, rnd, [88,222,444]), (rev, rnd, [0.4, 0.2, 0])] 
prms "spaceperc8" [(vol, rnd , [0.8, 0, 0.2, 0.6]), (hpFreq, rnd, [88,222,444]), (rev, rnd, [0.4, 0.2, 0])] 
prms "spaceperc4" [(vol, rnd , [0.8, 0, 0.2, 0.6]), (hpFreq, rnd, [88,222,444]), (rev, rnd, [0.4, 0.2, 0])] 
prms "spaceperc5" [(vol, rnd , [0.8, 0, 0.2, 0.6]), (hpFreq, rnd, [88,222,444]), (rev, rnd, [0.4, 0.2, 0])] 


addC "snare41" "ee" $ [2]
cPat "dbk1" "K909" 


addC "cmksyn" "u" $ [0, 2.5] 

prms "cmksyn" [(hpFreq, rnd, [800, 1300, 500]), (del, rnd, [0.6, 0, 0.7, 0.3]), (chorus, rnd ,[0.3, 0.6])]


p "spaceperc4"

prms "hov" [(durTS, nv, [1, 1]),(pitch, nv, [50, 48])]

prms "K909" [()]

prms "ptc" [(rev, k, [0.8]),(chorus, rnd, [0.6, 0.2, 0]), (del, rnd, [0.3, 0.5, 0.4])]

prms "letthebeat" [(dur, nv, [0.5, 0.5, 0.5, 1.5]),(vol, k, [0.66]),(rev, rnd, [0.2, 0.6, 0.3])]

cPat "uno" "letthebeat" 
p "letthebeat"

prms "hit4" [(rev, k, [0.4])] 

addC "hit4" "h!" $ toTP [2.15] 
p "hit4"

s "hit4"


addC "hit1" "y!" $ textToTP 32 "y!"
s "hit3" >> p "hit1"

addC "hit5" "hh!" $ toTP [5.5]
p "hit5" 

silence 
