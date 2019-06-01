-- create Sampler instruments

defPath s = "/Users/leofltt/Desktop/KairosSamples" ++ s
addI "sh" $ sampler $ defPath "/ch/shortHat.wav"
addI "r1" $ sampler $ defPath "/rim/HollowRim.wav"
addI "rS" $ sampler $ defPath "/rim/SmallRim.wav"
addI "r707" $ sampler $ defPath "/rim/707_rim.wav"
addI "rRev" $ sampler $ defPath "/rim/HeavyRevRim.wav"
addI "snS" $ sampler $ defPath "/snares/SNSandy.wav"
addI "sj1" $ sampler $ defPath "/snares/Snare4JungleMidHigh.wav"
addI "sj2" $ sampler $ defPath "/snares/Snare4JungleMidLow.wav"
addI "sSnap" $ sampler $ defPath "/snares/EkaliShortSnare.wav"
addI "siS" $ sampler $ defPath "/snares/SNDry2.wav"
addI "snMute" $ sampler $ defPath "/snares/SNMuteAC.wav"
addI "sSplash" $ sampler $ defPath "/snares/snSplash.wav"
addI "glass" $ sampler $ defPath "/fracture/Glass1Dry.wav"
addI "st1" $ sampler $ defPath "/stomps/stomp1.wav"
addI "st2" $ sampler $ defPath "/stomps/stomp2.wav"
addI "st3" $ sampler $ defPath "/stomps/stomp3.wav"
addI "lz" $ sampler $ defPath "/laser/gblzr1.wav"
addI "llz" $ sampler $ defPath "/laser/gblzr2.wav"
addI "bs" $ sampler $ defPath "/bass/sinFol.wav"
addI "chp" $ sampler $ defPath "/ch/pointHat.wav"
addI "shSh" $ sampler $ defPath "/ch/ShHihat.wav"
addI "shPsh" $ sampler $ defPath "/ch/shPShat.wav"
addI "mtlh" $ sampler $ defPath "/ch/veryMetalHihat.wav"
addI "CPhi" $ sampler $ defPath "/clap/CLAPHi.wav"
addI "snap" $ sampler $ defPath "/clap/Snap.wav"
addI "cr01" $ sampler $ defPath "/cym/Crash01.wav"
addI "crH" $ sampler $ defPath "/cym/crashHighPitch.wav"
addI "crHl" $ sampler $ defPath "/cym/crashHPLonger.wav"
addI "crPz" $ sampler $ defPath "/cym/CymbolL.wav"
addI "brokWin" $ sampler $ defPath "/fx/BrokenWindow.aif"
addI "pop" $ sampler $ defPath "/fx/pop.wav"
addI "shut" $ sampler $ defPath "/fx/shutter.wav"
addI "gtStab" $ sampler $ defPath "/Gt/AmGtChopStab.wav"
addI "orStab" $ sampler $ defPath "/Key/GShmOrganStab.wav"
addI "kWd" $ sampler $ defPath "/kicks/KickWoody.wav"
addI "kp" $ sampler $ defPath "/kicks/OrphansSonKick.wav"
addI "kN" $ sampler $ defPath "/kicks/EkaliKick.wav"
addI "kbaSh" $ sampler $ defPath "/kicks/basskickShallow.wav"
addI "kSw" $ sampler $ defPath "/kicks/KickSweepOd.wav"
addI "ohlE" $ sampler $ defPath "/oh/arohlong.wav"
addI "ohsE" $ sampler $ defPath "/oh/arohshort.wav"
addI "ohSk" $ sampler $ defPath "/oh/hhopenSnake.wav"
addI "cbE" $ sampler $ defPath "/percIt/arcbcl.wav"
addI "conRhi" $ sampler $ defPath "/percIt/CongaRimHi.aif"
addI "conMid" $ sampler $ defPath "/percIt/MidConga.wav"
addI "slapWa" $ sampler $ defPath "/percIt/Smakwa.aif"
addI "tbish" $ sampler $ defPath "/percIt/tablaish.wav"
addI "lilShake" $ sampler $ defPath "/shakeIt/lilShake.wav"
addI "ohsn" $ sampler $ defPath "/shakeIt/ohsn.wav"
addI "GOT" $ sampler $ defPath "/vox/GOT.wav"
addI "makeit" $ sampler $ defPath "/vox/makeit.wav"
addI "dum" $ sampler $ defPath "/vox/VoxDum.wav"
addI "AaH" $ sampler $ defPath "/vox/voxHiAaA.wav"
addI "OoH" $ sampler $ defPath "/vox/voxHiOO.wav"
addI "Oh" $ sampler $ defPath "/vox/voxPointOH.wav"
addI "Ao" $ sampler $ defPath "/vox/voxStabAO.wav"
fs n string | n <= 0 = [] | otherwise = string ++ " " ++ fs (n-1) string


--TEST PERFORMANCE
----------------------


displayIns

displayTP

addC "st2" "stompy2" $ toTP $ (evolve 1) (interp1 4) [0.75, 2.25]
vol "st2" [Pd 0.7] keep
p "st2"

cPat "eightN" "st1"
vol "st1" [Pd 0.3] keep
p "st1"

del "st1" (toPfD [0.9]) keep

dtdel 700
fbdel 0.9

cPat "upFour" "CH808"
pan "CH808" (toPfD [0, 1]) randomize
p "CH808"

solo "303"

s "st2"

p "K909"
cPat "downB" "sS"
del "sS" [Pd 0.1] keep
p "sS"

vol "303" [Pd 0.1] keep

silence

p "K909"
cPat "jGhost" "sS"
del "sS" [Pd 0.7, Pd 0] randomize
p "sS"

pan"K909" [Pd 0, Pd 0, Pd 1] retrograde
pan "st2" [Pd 1] keep

cT 125

silence

addC "K909" "testA" $ toTP $ evolve 1 (interp1 4) (fromTP jGhost1)

cPat "sixteenN" "303"
dur "303" [Pd 0.3] keep
freq "303" [Pd 36, Pd 40, Pd 38] randomize
vol "303" [Pd 0.51] keep
cf "303" [Pd 7000] keep
rev "303" [Pd 0.4] keep
p "303"


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
