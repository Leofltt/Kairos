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
addPf' i pf pfnum list fun = addPf i pf =<< createPfPat pfnum list fun
silence = stopAll e
playA = playAll e
playFx = playEffect e
solo = soloIns e
fs n string | n <= 0 = [] | otherwise = string ++ " " ++ fs (n-1) string
defPath s = "/Users/leofltt/Desktop/KairosSamples" ++ s

:! clear


--TEST PERFORMANCE
----------------------

silence

addPf' "303" "rev" 5 (toPfD $ [0.5, 0.7]) nextVal

mapM_ p ["kcJ","CH808"]

cPat "jGhost1" "CH808"
cPat "dbk1" "kcJ"

addC "CP909" "yolo" $ toTP $ [0.4, 0.8, 1.6, 3.2]

addPf' "CP909" "vol" 4 (toPfD $ [0.75, 0.5, 0]) randomize
p "CP909"
   
playFx "rev"

playFx "del"

addPf' "CH808" "vol" 4 (toPfD $ [0.75]) nextVal

addPf' "303" "delS" 6 (toPfD $ [0.3,0.7,0.9]) randomize

addPf' "303" "dur" 3 (toPfD $ [ 0.7, 0.8 ,0.5]) randomize
addPf' "303" "pitch" 7 (toPfD $ [42, 48, 52, 36]) randomize
addPf' "303" "cf" 8 (toPfD $ [6000, 888, 2222]) randomize
cPat "eightN" "303"
p "303"

addPf' "303" "reverb" 5 (toPfD $ [0.6]) keep

addC "303" "test" $ toTP $ takeWhile (<4) [0,0.33..]

cPat "sixteenN"  "sJ"

addP "roll" $ toTP $ takeWhile (<4) [0,0.2..]

mapM_ (cPat "dbk1") ["K909","OH808"]

addI "sJ" $ sampler $ defPath "/Snares/Snare4JungleMidLow.wav"

fs 888 "finding beauty in dissonance"


sj1 <- samplePath (toPfS $ map defPath ["/Kicks/KickCymbJungle.wav","/909/Kick-909.aif"]) nextVal

addPf "303" "vol" =<< volume (toPfD [1.5]) keep

