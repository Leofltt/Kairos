--inits && useful
------------------

:set prompt "\n"
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
addPf' i pf pfnum list fun = addPf i pf =<< createPfPat pfnum list fun
silence = stopAll e
playA = playAll e
platFx = playEffect e
solo = soloIns e
fs n string | n <= 0 = [] | otherwise = string ++ " " ++ fs (n-1) string
defPath s = "/Users/leofltt/Desktop/KairosSamples" ++ s

--TEST PERFORMANCE
----------------------

cPat "jGhost1" "sJ"

solo "CP909"

playA

silence

p "K909"

p "OH808"

p "CH808"
p "snJ1"

addC "CP909" "empty" $ toTP []

silence

cPat "sixteenN"  "sJ"

addP "roll" $ toTP $ takeWhile (<4) [0,0.2..]

addC "sJ" "jGhost1" $ toTP [1.75,2.25,5.75,6.25,7.75]
p "sJ"

mapM_ (cPat "dbk1") ["K909","OH808"]

addI "sJ" $ sampler $ defPath "/Snares/Snare4JungleMidLow.wav"

fs 888 "finding beauty in dissonance"

cT 156

sj1 <- samplePath [(Ps "/Users/leofltt/Desktop/KairosSamples/Kicks/KickCymbJungle.wav"),(Ps "/Users/leofltt/Desktop/KairosSamples/909/Kick-909.aif")] nextVal

addPf "OH808" "vol" =<< volume (toPfD [0.6]) keep

vol2 <- volume [(Pd 1),(Pd 0.53),(Pd 0.55),(Pd 0.53)] nextVal

oc <- createPfPat 5 (toPfD [0.1,0.1,0.8,0.1]) nextVal

addPf "CH808" "opcl" oc

addPf "CH808" "vol" volUp

cPat "sJ" "eightN"

addC "sJ" "hi" $ toTP [5,13]

addPf "K909" "vol" =<< volume  (toPfD [0]) keep

addPf "sJ" "samples" snares1
