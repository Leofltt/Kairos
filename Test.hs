--inits && useful
------------------

:set prompt ""
e <- defaultPerformance
p = play e
s = stop e
cPat p i = changeTimeF e i p
cT = changeTempo (clock e)
addP = addTPf e
addIns = addInstrument e
addI name ins = addIns name =<< ins
addC i n s = addP n s >> cPat n i
addPf = addPfPath' e
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
silence = stopAll e
playA = playAll e
playFx = playEffect e
solo = soloIns e
fs n string | n <= 0 = [] | otherwise = string ++ " " ++ fs (n-1) string
defPath s = "/Users/leofltt/Desktop/KairosSamples" ++ s
addI "sh" $ sampler $ defPath "/ch/shortHat.wav"
addI "r1" $ sampler $ defPath "/rim/HollowRim.wav"
addI "rS" $ sampler $ defPath "/rim/SmallRim.wav"
addI "sS" $ sampler $ defPath "/snares/SNSandy.wav"
addI "sj1" $ sampler $ defPath "/snares/Snare4JungleMidHigh.wav"
addI "sj2" $ sampler $ defPath "/snares/Snare4JungleMidLow.wav"
addI "glass" $ sampler $ defPath "/fracture/Glass1Dry.wav"
mapM playFx ["rev","del"]
:! clear


--TEST PERFORMANCE
----------------------

addC "303" "lS3" $ toTP $ lSys 4 (interp2 4) $ lSys 6 (interp1 4) (fromTP dbk1)

silence 

freq "303" (toPfD $ [57, 60, 65]) randomize

p "303"

cf "303" (toPfD $ [5000, 8000]) randomize

cPat "eightN" "CH808" 
vol "CH808" [Pd 0.7] keep
p "CH808"

rev "CH808" [Pd 0.15] keep
del "CP909" [Pd 0.3, Pd 0] randomize


cPat "downB" "CP909"
p "CP909"

cPat "dbk1" "K909" 
cPat "upFour" "sh"
p "K909"
p "sh"

cT 127













-- example 1 : Techno

--setup

cPat "fourFloor" "K909"
cPat "upFour" "CH808"
cPat "downB" "CP909"

-- demo walkthrough

cT 127

mapM_ p ["K909","CH808"]

p "CP909"


addC "OH808" "OH1" $ toTP $ [1.5,3.5]
p "OH808"

addPf' "OH808" 4 [Pd 0.1] keep

vol "CP909" [Pd 1.4] keep
rev "CP909" (toPfD [0.4]) keep
rev "303" [Pd 0.5] keep

solo "CP909"

p "303"

s "OH808"

rev "OH808" [(Pd 0.3)] keep

cPat "sixteenN"  "303"
freq "303" (toPfD $ [36, 43]) nextVal
cf "303" (toPfD $ [6000]) keep

p "303"

vol "OH808" [Pd 0.4] keep

-----------------------------

-- example 2 : Jungle / Drum & Bass

-- setup

cPat "eightN" "sh"
cPat "jGhost1" "rS"
cPat "jGhost" "r1"
cPat "dbk1" "kcj"
cPat "upFour" "OH808"

vol "OH808" [Pd 0.4] keep

mapM_ (cPat "downB") ["sS", "sj1", "sj2"]

-- play

cT 148

mapM_ p ["kcj", "sS"]

p "sh"

p "OH808"

p "sS"

dur "hov" (toPfD [0.75, 0.8]) randomize

addC "hov" "lSys1" $ toTP $ lSys 1 (interp1 4) (fromTP jGhost)
freq "hov" (toPfD [48, 51, 55]) randomize
vol "hov" [Pd 0.8] keep
cf "hov" [Pd 900] keep

p "hov"

p "r1"

solo "hov" 
p "sS"

silence 

mapM_ p ["sh","r1","kcj"]

addC "r1" "lSr" $ toTP $ lSys 2 (interp2 4) (fromTP jGhost1)


addC "K909" "lS1" $ toTP $ lSys 2 (interp1 4) [0,2.5]

addC "rS" "lS2" $ toTP $ lSys 2 (interp1 8) [1.75,2.25,5.75, 6.25, 7.75]

cPat "downB" "sj1"
p "sj1"

silence
