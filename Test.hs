-- create Sampler instruments

defPath s = "/Users/leofltt/Desktop/KairosSamples" ++ s
markPath s = "/Users/leofltt/Desktop/Kairos/MarkovTables"
k909 = defPath "/909/Kick-909.aif"
addI "K909" $ sampler k909
kcj = defPath "/kicks/KickCymbJungle.wav"
addI "kcj" $ sampler kcj
cp909 = defPath "/909/Clap-909.aif"
addI "CP909" $ sampler cp909
sh = defPath "/ch/shortHat.wav"
addI "sh" $ sampler sh
r1 = defPath "/rim/HollowRim.wav"
addI "r1" $ sampler r1
rS = defPath "/rim/SmallRim.wav"
addI "rS" $ sampler rS
r707 = defPath "/rim/707_rim.wav"
addI "r707" $ sampler r707
addI "rRev" $ sampler $ defPath "/rim/HeavyRevRim.wav"
snS = defPath "/snares/SNSandy.wav"
addI "snS" $ sampler snS
sj1 = defPath "/snares/Snare4JungleMidHigh.wav"
addI "sj1" $ sampler sj1
sj2 = defPath "/snares/Snare4JungleMidLow.wav"
addI "sj2" $ sampler sj2
sSnap = defPath "/snares/EkaliShortSnare.wav"
addI "sSnap" $ sampler sSnap
addI "siS" $ sampler $ defPath "/snares/SNDry2.wav"
addI "snMute" $ sampler $ defPath "/snares/SNMuteAC.wav"
sSplash = defPath "/snares/snSplash.wav"
addI "sSplash" $ sampler sSplash
glass = defPath "/fracture/Glass1Dry.wav"
addI "glass" $ sampler glass
st1 = defPath "/stomps/stomp1.wav"
addI "st1" $ sampler st1
st2 = defPath "/stomps/stomp2.wav"
addI "st2" $ sampler st2
st3 = defPath "/stomps/stomp3.wav"
addI "st3" $ sampler st3
lz = defPath "/laser/gblzr1.wav"
addI "lz" $ sampler lz
llz = defPath "/laser/gblzr2.wav"
addI "llz" $ sampler llz
bs = defPath "/bass/sinFol.wav"
addI "bs" $ sampler bs
chp = defPath "/ch/pointHat.wav"
addI "chp" $ sampler chp
shSh = defPath "/ch/ShHihat.wav"
addI "shSh" $ sampler shSh
shPsh = defPath "/ch/shPShat.wav"
addI "shPsh" $ sampler shPsh
mtlh = defPath "/ch/veryMetalHihat.wav"
addI "mtlh" $ sampler mtlh
cphi = defPath "/clap/CLAPHi.wav"
addI "CPhi" $ sampler cphi
snap = defPath "/clap/Snap.wav"
addI "snap" $ sampler snap
cr01 = defPath "/cym/Crash01.wav"
addI "cr01" $ sampler cr01
crH = defPath "/cym/crashHighPitch.wav"
addI "crH" $ sampler crH
addI "crHl" $ sampler $ defPath "/cym/crashHPLonger.wav"
addI "crPz" $ sampler $ defPath "/cym/CymbolL.wav"
brokWin = defPath "/fx/BrokenWindow.aif"
addI "brokWin" $ sampler brokWin
pop = defPath "/fx/pop.wav"
addI "pop" $ sampler pop
shut = defPath "/fx/shutter.wav"
addI "shut" $ sampler shut
addI "gtStab" $ sampler $ defPath "/Gt/AmGtChopStab.wav"
addI "orStab" $ sampler $ defPath "/Key/GShmOrganStab.wav"
kWd = defPath "/kicks/KickWoody.wav"
addI "kWd" $ sampler kWd
kp = defPath "/kicks/OrphansSonKick.wav"
addI "kp" $ sampler kp
kN = defPath "/kicks/EkaliKick.wav"
addI "kN" $ sampler kN
kbaSh = defPath "/kicks/basskickShallow.wav"
addI "kbaSh" $ sampler kbaSh
addI "kSw" $ sampler $ defPath "/kicks/KickSweepOd.wav"
ohlE = defPath "/oh/arohlong.wav"
addI "ohlE" $ sampler ohlE
ohsE = defPath "/oh/arohshort.wav"
addI "ohsE" $ sampler ohsE
addI "ohSk" $ sampler $ defPath "/oh/hhopenSnake.wav"
cbE = defPath "/percIt/arcbcl.wav"
addI "cbE" $ sampler cbE
conRhi = defPath "/percIt/CongaRimHi.aif"
addI "conRhi" $ sampler conRhi
conMid = defPath "/percIt/MidConga.wav"
addI "conMid" $ sampler conMid
slapWa = defPath "/percIt/Smakwa.aif"
addI "slapWa" $ sampler slapWa
tbish = defPath "/percIt/tablaish.wav"
addI "tbish" $ sampler tbish
lilShake = defPath "/shakeIt/lilShake.wav"
addI "lilShake" $ sampler lilShake
ohsn = defPath "/shakeIt/ohsn.wav"
addI "ohsn" $ sampler ohsn
addI "GOT" $ sampler $ defPath "/vox/GOT.wav"
addI "makeit" $ sampler $ defPath "/vox/makeit.wav"
addI "dum" $ sampler $ defPath "/vox/VoxDum.wav"
addI "AaH" $ sampler $ defPath "/vox/voxHiAaA.wav"
addI "OoH" $ sampler $ defPath "/vox/voxHiOO.wav"
addI "Oh" $ sampler $ defPath "/vox/voxPointOH.wav"
addI "Ao" $ sampler $ defPath "/vox/voxStabAO.wav"
shae = defPath "/ch/aeHAT.WAV"
addI "shae" $ sampler shae
tml = defPath "/percIt/808_TomMidLow.wav"
addI "808tml" $ sampler tml
tl1 = defPath "/percIt/808_tomLow.wav"
addI "808tl1" $ sampler tl1
kcs = defPath "/kicks/kickcsk.aif"
addI "kcs" $ sampler kcs
scs = defPath "/snares/snarecsk.aif"
addI "scs" $ sampler scs
shakecs = defPath "/shakeIt/shakercsk.aif"
addI "shakecs" $ sampler shakecs
shatcs = defPath "/shakeIt/shakeHatcsk.aif"
addI "shatcs" $ sampler shatcs
chcs = defPath "/ch/shortHatcsk.aif"
addI "chcs" $ sampler chcs
rSw = defPath "/fx/RevCym.wav"
addI "rSw" $ sampler rSw
cp808d = defPath "/808/808_clap_7_TL.wav"
addI "cp808d" $ sampler cp808d
cp808m = defPath "/808/808_clap_3_TL.wav"
addI "cp808m" $ sampler cp808m
fs n string | n <= 0 = [] | otherwise = string ++ " " ++ fs (n-1) string
csv1 = markPath ++ "/Test.csv"
:! clear

-- PERFORMANCE
----------------------


displayIns

displayTP


--PRACTICE

--pats
--

solo "ohsE"

cPat "eightN" "sh"
addC "kN" "jgk" jgk
addC "siS" "jgs"  jgs

cPat "downB" "cp808m"
addC "kN" "ukgk" ukgk
addC "r707" "ukgrs" ukgrs
addC "sh" "ukgch" ukgch
mapM_ p ["r707", "cp808m"]

addC "kN" "ir1ab" ir1abk

addC "siS" "ir1sn" ir1sn

addC "kN" "stdk3" stdbk3k
addC "snS" "stds3" stdbk3s




-------------------------
--
--
--
--

cT 134

addC "shae" "bou2" $ toTP  [0, 0.75, 2.5,2.75,3.25, 3.5]

addC "kp" "bou1" $ toTP  [0, 0.75, 2.5,3, 3.5]

addC "kbaSh" "bou" $ toTP [0, 0.75, 2.5, 3]

s "kbaSh"

p "kp"

cPat "fourFloor" "K909"
vol "K909" [Pd 0.7] keep
p "K909"

s "kp"

p "K909"
p "CH808"

s "ohsE"

solo "kp"

vol "K909" [Pd 0.8] keep

cPat "sixteenN" "shae"
p "shae"

p "rsW"

addC "rSw" "sw1" $ toTP [0.75, 2, 3.5]

params "rSw" [(keep, vol, [Pd 0.2]), (randomize, pan, [Pd 0.9, Pd 0.1]),(keep, rev, [Pd 0.9])]

del "rSw" [Pd 0.8] keep

dtdel 444
fbdel 0.9

cPat "sixteenN" "sh"

vol "sh" [Pd 0.7] keep
p "sh"

silence

params "sh" [(keep,vol,[Pd 0.25]), (keep, rev, [Pd 0.2])]

params "shae" [(keep, tune, [Pd 0.91]),(keep, vol,[Pd 0.3]),(keep,rev,[Pd 0.2])]
addC "shae" "snC" $ toTP [1, 3.75]

p "shae"

p "K909"


cPat "downB" "CP909"
vol "CP909" [Pd 0.7] keep
pan "CP909" [Pd 0, Pd 1] randomize
p "CP909"


addC "ohsE" "hatty" $ toTP [0.5, 1, 3, 3.5]

addC "ohsE" "hatty1" $ toTP $ (evolve 1) (interp1 4) [0.5, 1, 3, 3.5]
p "ohsE"

mapM_ p ["sh","ohsE"]

vol "ohsE" [Pd 0.15] keep

p "ohsE"

p "sh"

p "rSw"

solo "kbaSh"

addC "sj1" "oneS" $ toTP [3.5]
vol "sj1" [Pd 0.9] keep
p "sj1"

cPat "eightN" "303"
params "303" [(keep, dur, [Pd 0.22]),(keep,res,[Pd 4]), (keep, cf, [Pd 1666]),(keep, vol, [Pd 0.3]),(randomize, vol, (toPfD [0, 1, 1, 1, 1])),(randomize,freq, (toPfD $ [36, 38, 36, 36]))]

p "303"

solo "303"

-------------------------------------------------------------------
cPat "upFour" "CH808"

p "CH808"

p "K909"

vol "303" [Pd 0.5] keep
p "303"

solo "sSplash"
p "lz"
p "conMid"
p "lilShake"

cPat "downB" "CP909"


vol "K909" [Pd 1, Pd 0.8, Pd 0] randomize
pan "K909" [Pd 0, Pd 1] nextVal

del "CP909" [Pd 1, Pd 0.2] randomize


solo "cbE"

mapM_ p ["bs", "K909", "sh"]
