-- create Sampler instruments

defPath s = "/Users/leofltt/Desktop/KairosSamples" ++ s
sh = defPath "/ch/shortHat.wav"
addI "sh" $ sampler $ sh
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
fs n string | n <= 0 = [] | otherwise = string ++ " " ++ fs (n-1) string


-- PERFORMANCE
----------------------


displayIns

p "303"

vol "303" [Pd 0.5] keep

cPat "jGhost" "303"
freq "303" (toPfD [ 36, 40, 38, 38]) nextVal
dur "303" [Pd 0.4] keep
cf "303" [Pd 900] keep
res "303" [Pd 10] keep

vol "K909" [Pd 0.4] keep

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


vol "GOT" [Pd 1] keep

del "CP909" [Pd 1, Pd 0.2] randomize


solo "cbE"

mapM_ p ["bs", "K909", "sh"]



cps "bs" [Pd 1, Pd 1, Pd 1, Pd 0.8, Pd 1, Pd 1, Pd 1, Pd 0.5] nextVal
