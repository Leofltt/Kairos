inits && useful
------------------

e <- defaultPerformance
p = play e
s = stop e
cPat = changeTimeF e
cT = changeTempo (clock e)
addP = addTPf e
addC i n s = addP n s >> cPat i n
addPf = addPfPath' e
addPf' i pf pfnum list fun = addPf i pf =<< createPfPat pfnum list fun
silence = stopAll e
playA = playAll e
fs n string | n <= 0 = [] | otherwise = string ++ " " ++ fs (n-1) string
:set prompt "\n"

TEST PERFORMANCE
----------------------

s "K909"

cPat "snJ1" "fourFloor"

p "CH808"


fs 888 "| KAIROS"

cT 140

sp1 <- samplePath [(Ps "/Users/leofltt/Desktop/KairosSamples/Kicks/KickCymbJungle.wav"),(Ps "/Users/leofltt/Desktop/KairosSamples/909/Kick-909.aif")] nextVal

addPf "K909" "vol" =<< volume (toPfD [0,0.6,0,0.9]) nextVal

snares1 <- samplePath [(Ps "/Users/leofltt/Desktop/KairosSamples/Snares/Snare4Jungle1.wav"),(Ps "/Users/leofltt/Desktop/KairosSamples/Snares/Snare4JungleGhostNote.wav"),(Ps "/Users/leofltt/Desktop/KairosSamples/Snares/Snare4JungleGhostNote.wav"),(Ps "/Users/leofltt/Desktop/KairosSamples/Snares/Snare4JungleGhostNote.wav")] nextVal

vol2 <- volume [(Pd 1),(Pd 0.53),(Pd 0.55),(Pd 0.53)] nextVal

oc <- createPfPat 5 [(Pd 0.1),(Pd 0.1),(Pd 0.8),(Pd 0.1)] nextVal

addPf "CH808" "opcl" oc
addPf "CH808" "vol" volUp

cPat "CH808" "sixteenN"

addC "snJ1" "hi" [(TP 1),(TP 2.5)]

addPf "snJ1" "vol" vol2
addPf "snJ1" "samples" snares1

