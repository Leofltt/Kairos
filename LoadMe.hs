-- create Sampler instruments && CSVs

defPath s = "/Users/leofltt/Desktop/KairosSamples" ++ s
markPath s = "/Users/leofltt/Desktop/Kairos/MarkovTables" ++ s
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
rRev = defPath "/rim/HeavyRevRim.wav"
addI "rRev" $ sampler rRev
snS = defPath "/snares/SNSandy.wav"
addI "snS" $ sampler snS
sj1 = defPath "/snares/Snare4JungleMidHigh.wav"
addI "sj1" $ sampler sj1
sj2 = defPath "/snares/Snare4JungleMidLow.wav"
addI "sj2" $ sampler sj2
sSnap = defPath "/snares/EkaliShortSnare.wav"
addI "sSnap" $ sampler sSnap
siS = defPath "/snares/SNDry2.wav"
addI "siS" $ sampler siS
snMute = defPath "/snares/SNMuteAC.wav"
addI "snMute" $ sampler snMute
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
shtb = defPath "/shakeIt/LM-2_TAMB_1_TL.wav"
addI "shtb" $ sampler shtb
shnoi = defPath "/shakeIt/LM-2_SHAKER_1_TL.wav"
addI "shnoi" $ sampler shnoi
kOns = defPath "/kicks/KickOpen1_NateSmith.wav"
addI "kOns" $ sampler kOns
kCns = defPath "/kicks/KickTight1_NateSmith.wav"
addI "kCns" $ sampler kCns
kNns = defPath "/kicks/KickTight2_NateSmith.wav"
addI "kNns" $ sampler kNns
tl2 = defPath "/808/808_tom_4_TL.wav"
addI "808tl2" $ sampler tl2
ks808 = defPath "/808/808_kick_21_TL.wav"
addI "ks808" $ sampler ks808
hh808sh = defPath "/808/808_hi_hat_13_TL.wav"
addI "hh808sh" $ sampler hh808sh
sn808noi = defPath "/808/808_snare_29_TL.wav"
addI "sn808noi" $ sampler sn808noi
sn808cl = defPath "/808/808_snare_9_TL.wav"
addI "sn808cl" $ sampler sn808cl
sn808d = defPath "/808/808_snare_34_TL.wav"
addI "sn808d" $ sampler sn808hi
ride626 = defPath "/626/ride.wav"
shkr626 = defPath "/626/shaker.wav"
sn1626 = defPath "/626/snare1.wav"
sn2626 = defPath "/626/snare2.wav"
sn3626 = defPath "/626/snare3.wav"
tambo626 = defPath "/626/tambo.wav"
cb626 = defPath "/626/cowb.wav"
clv626 = defPath "/626/claves.wav"
colo626 = defPath "/626/conga-lo.wav"
comi626 = defPath "/626/conga-m.wav"
k1626 = defPath "/626/bd1.wav"
k2626 = defPath "/626/bd2.wav"
ri707 = defPath "/cym/707_Ride.wav"
kc1 = defPath "/kicks/kickCarlo1.wav"
kc2 = defPath "/kicks/kickCarlo2.wav"
fa1 = defPath "/fx/far1.aif"
addI "ride626" $ sampler ride626
addI "shkr626" $ sampler shkr626
addI "sn1626" $ sampler sn1626
addI "sn2626" $ sampler sn2626
addI "sn3626" $ sampler sn3626
addI "tambo626" $ sampler tambo626
addI "cb626" $ sampler cb626
addI "clv626" $ sampler clv626
addI "colo626" $ sampler colo626
addI "comi626" $ sampler comi626
addI "k1626" $ sampler k1626
addI "k2626" $ sampler k2626
addI "ri707" $ sampler ri707
addI "kc1" $ sampler kc1
addI "kc2" $ sampler kc2
addI "fa1" $ sampler fa1
alzir = defPath "/breaks/Alzir.wav"
back = defPath "/breaks/Back.wav"
bad = defPath "/breaks/Bad.wav"
blue = defPath "/breaks/Blue.wav"
boo = defPath "/breaks/Boo.wav"
booWorm = defPath "/breaks/BooWorm.wav"
bringingMe = defPath "/breaks/BringingMe.wav"
bulldozer = defPath "/breaks/Bulldozer.wav"
bulldozer2 = defPath "/breaks/Bulldozer2.wav"
control = defPath "/breaks/Control.wav"
dperc = defPath "/breaks/DetroitPercussion.wav"
essential = defPath "/breaks/Essential.wav"
heaven = defPath "/breaks/Heaven.wav"
hurtuso = defPath "/breaks/HurtUSo.wav"
johnny = defPath "/breaks/Johnny.wav"
liberty = defPath "/breaks/Liberty.wav"
massive = defPath "/breaks/Massive.wav"
peacesign = defPath "/breaks/PeaceSign.wav"
pressin = defPath "/breaks/Pressin.wav"
ricochet = defPath "/breaks/Ricochet.wav"
rise = defPath "/breaks/Rise.wav"
ror = defPath "/breaks/RollOnRoll.wav"
sovreign = defPath "/breaks/Sovreign.wav"
tear = defPath "/breaks/Tear.wav"
terrorist = defPath "/breaks/Terrorist.wav"
walk = defPath "/breaks/Walk.wav"
addI "alzir" $ stutter alzir
addI "back" $ stutter back
addI "bad" $ stutter bad
addI "blue" $ stutter blue
addI "boo" $ stutter boo
addI "booWorm" $ stutter booWorm
addI "bMe" $ stutter bringingMe
addI "bdoze" $ stutter bulldozer
addI "bdoze2" $ stutter bulldozer2
addI "control" $ stutter control
addI "dperc" $ stutter dperc
addI "essential" $ stutter essential
addI "heaven" $ stutter heaven
addI "hus" $ stutter hurtuso
addI "johnny" $ stutter johnny
addI "tear" $ stutter tear
addI "liberty" $ stutter liberty
addI "massive" $ stutter massive
addI "peacesign" $ stutter peacesign
addI "pressin" $ stutter pressin
addI "ricochet" $ stutter ricochet
addI "rise" $ stutter rise
addI "ror" $ stutter ror
addI "sovreign" $ stutter sovreign
addI "terrorist" $ stutter terrorist
addI "walk" $ stutter walk
er1k = defPath "/ER1/kick04.wav"
er1mhh = defPath "/ER1/MicroHat.wav"
er1cr1 = defPath "/ER1/CrashCymbal1.wav"
er1cr2 = defPath "/ER1/CrashCymbal2.wav"
er1r = defPath "/ER1/rim01.wav"
er1kd = defPath "/ER1/kick14.wav"
addI "er1k" $ sampler er1k
addI "er1mhh" $ sampler er1mhh
addI "er1cr1" $ sampler er1cr1
addI "er1cr2" $ sampler er1cr2
addI "er1r" $ sampler er1r
addI "er1kd" $ sampler er1kd
fs n string | n <= 0 = [] | otherwise = string ++ " " ++ fs (n-1) string
csv1 = markPath "/Test.csv"
csv2 = markPath "/Test2.csv"
csv3 = markPath "/Test3.csv"
adk = toTP [0, 1, 2,2.5,3,4,5,5.5,6,7,8,8.5,9,10,11,11.5]
addP "adk" adk
adb = toTP [0,1,2,3,4.5,5,6,7,8,9.5,10]
addP "adb" adb
params "lpFM" [ (keep, vol, [Pd 0.7]),(keep, rev, [Pd 0.5]),((percentNext 73), pitch, toPfD [53, 56, 58, 53, 48]),(keep, fmIndx, [Pd 12.3]), (keep, fmDepth, [Pd 987.23]), (keep, dist, [Pd 1.15]),(keep,cf,[Pd 3880]),( keep, rev, [Pd 0.6]),(keep, adRatio,[Pd 0.7]),(randomize,pan,toPfD[0.4,0.6,0.75,0.35])]
params "303" [ (keep,dur,toPfD[ 0.25]),(keep,res,[Pd 4]),(keep, vol, [Pd 0.5]),(keep, cf, [Pd 3500]),((runMarkov csv3), pitch, toPfD (withScale 41 dorian)),(keep, rev, [Pd 0.4])]
params "hov" [ (keep,dur,toPfD[ 1.2]),(keep, vol, [Pd 0.3]),(keep, cf, [Pd 800]),((runMarkov csv3), pitch, toPfD (withScale 41 dorian)),(keep, rev, [Pd 0.7])]
let plucky = cf "303" [Pd 5000] keep >> res "303" [Pd 6] keep >> dur "303" [Pd 0.17] keep
plucky
:! clear
