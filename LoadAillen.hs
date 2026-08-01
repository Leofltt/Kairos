-- Aillen Sample Loader Script
-- Grouped by instrument type across Aillen's 4 Sampler tracks:
-- Track 1: Kicks
-- Track 2: Snares & Claps
-- Track 3: Hats, Percussion, Vox & FX
-- Track 5: Breaks (Stutters)

defPath s = "/Users/leofltt/Desktop/KairosSamples" ++ s
markPath s = "/Users/leofltt/Desktop/Kairos/MarkovTables" ++ s
aillenPort = "8000"
k909 = defPath "/909/Kick-909.aif" -- TRACK 1: KICKS
addI "K909" $ aillenSampler 1 k909
kcj = defPath "/kicks/KickCymbJungle.wav"
addI "kcj" $ aillenSampler 1 kcj
kWd = defPath "/kicks/KickWoody.wav"
addI "kWd" $ aillenSampler 1 kWd
kp = defPath "/kicks/OrphansSonKick.wav"
addI "kp" $ aillenSampler 1 kp
kN = defPath "/kicks/EkaliKick.wav"
addI "kN" $ aillenSampler 1 kN
kbaSh = defPath "/kicks/basskickShallow.wav"
addI "kbaSh" $ aillenSampler 1 kbaSh
addI "kSw" $ aillenSampler 1 $ defPath "/kicks/KickSweepOd.wav"
kcs = defPath "/kicks/kickcsk.aif"
addI "kcs" $ aillenSampler 1 kcs
kOns = defPath "/kicks/KickOpen1_NateSmith.wav"
addI "kOns" $ aillenSampler 1 kOns
kCns = defPath "/kicks/KickTight1_NateSmith.wav"
addI "kCns" $ aillenSampler 1 kCns
kNns = defPath "/kicks/KickTight2_NateSmith.wav"
addI "kNns" $ aillenSampler 1 kNns
ks808 = defPath "/808/808_kick_21_TL.wav"
addI "ks808" $ aillenSampler 1 ks808
k1626 = defPath "/626/bd1.wav"
addI "k1626" $ aillenSampler 1 k1626
k2626 = defPath "/626/bd2.wav"
addI "k2626" $ aillenSampler 1 k2626
kc1 = defPath "/kicks/kickCarlo1.wav"
addI "kc1" $ aillenSampler 1 kc1
kc2 = defPath "/kicks/kickCarlo2.wav"
addI "kc2" $ aillenSampler 1 kc2
er1k = defPath "/ER1/kick04.wav"
addI "er1k" $ aillenSampler 1 er1k
er1k2 = defPath "/ER1/kick03.wav"
addI "er1k2" $ aillenSampler 1 er1k2
er1k3 = defPath "/ER1/kick14.wav"
addI "er1k3" $ aillenSampler 1 er1k3
er1kl = defPath "/ER1/kick08.wav"
addI "er1kl" $ aillenSampler 1 er1kl
cp909 = defPath "/909/Clap-909.aif" -- TRACK 2: SNARES & CLAPS
addI "CP909" $ aillenSampler 2 cp909
snS = defPath "/snares/SNSandy.wav"
addI "snS" $ aillenSampler 2 snS
sj1 = defPath "/snares/Snare4JungleMidHigh.wav"
addI "sj1" $ aillenSampler 2 sj1
sj2 = defPath "/snares/Snare4JungleMidLow.wav"
addI "sj2" $ aillenSampler 2 sj2
sSnap = defPath "/snares/EkaliShortSnare.wav"
addI "sSnap" $ aillenSampler 2 sSnap
siS = defPath "/snares/SNDry2.wav"
addI "siS" $ aillenSampler 2 siS
snMute = defPath "/snares/SNMuteAC.wav"
addI "snMute" $ aillenSampler 2 snMute
sSplash = defPath "/snares/snSplash.wav"
addI "sSplash" $ aillenSampler 2 sSplash
cphi = defPath "/clap/CLAPHi.wav"
addI "CPhi" $ aillenSampler 2 cphi
snap = defPath "/clap/Snap.wav"
addI "snap" $ aillenSampler 2 snap
scs = defPath "/snares/snarecsk.aif"
addI "scs" $ aillenSampler 2 scs
cp808d = defPath "/808/808_clap_7_TL.wav"
addI "cp808d" $ aillenSampler 2 cp808d
cp808m = defPath "/808/808_clap_3_TL.wav"
addI "cp808m" $ aillenSampler 2 cp808m
sn808noi = defPath "/808/808_snare_29_TL.wav"
addI "sn808noi" $ aillenSampler 2 sn808noi
sn808cl = defPath "/808/808_snare_9_TL.wav"
addI "sn808cl" $ aillenSampler 2 sn808cl
sn808d = defPath "/808/808_snare_34_TL.wav"
addI "sn808d" $ aillenSampler 2 sn808d
glsn = defPath "/snares/Snare Prism2.wav"
addI "glsn" $ aillenSampler 2 glsn
sn1626 = defPath "/626/snare1.wav"
addI "sn1626" $ aillenSampler 2 sn1626
sn2626 = defPath "/626/snare2.wav"
addI "sn2626" $ aillenSampler 2 sn2626
sn3626 = defPath "/626/snare3.wav"
addI "sn3626" $ aillenSampler 2 sn3626
er1r = defPath "/ER1/rim01.wav"
addI "er1r" $ aillenSampler 2 er1r
rimshock = defPath "/raveshaped/RimShock.wav"
addI "rimshock" $ aillenSampler 2 rimshock
wrdsn = defPath "/snares/weirdsnare1.wav"
addI "wrdsn" $ aillenSampler 2 wrdsn
snare34 = defPath "/snares/Snare34.wav"
addI "snare34" $ aillenSampler 2 snare34
snare41 = defPath "/snares/Snare41.wav"
addI "snare41" $ aillenSampler 2 snare41
sh = defPath "/ch/shortHat.wav" -- TRACK 3: HATS, PERCUSSION, VOX & FX
addI "sh" $ aillenSampler 3 sh 
r1 = defPath "/rim/HollowRim.wav"
addI "r1" $ aillenSampler 3 r1
rS = defPath "/rim/SmallRim.wav"
addI "rS" $ aillenSampler 3 rS
r707 = defPath "/rim/707_rim.wav"
addI "r707" $ aillenSampler 3 r707
rRev = defPath "/rim/HeavyRevRim.wav"
addI "rRev" $ aillenSampler 3 rRev
glass = defPath "/fracture/Glass1Dry.wav"
addI "glass" $ aillenSampler 3 glass
st1 = defPath "/stomps/stomp1.wav"
addI "st1" $ aillenSampler 3 st1
st2 = defPath "/stomps/stomp2.wav"
addI "st2" $ aillenSampler 3 st2
st3 = defPath "/stomps/stomp3.wav"
addI "st3" $ aillenSampler 3 st3
lz = defPath "/laser/gblzr1.wav"
addI "lz" $ aillenSampler 3 lz
llz = defPath "/laser/gblzr2.wav"
addI "llz" $ aillenSampler 3 llz
bs = defPath "/bass/sinFol.wav"
addI "bs" $ aillenSampler 3 bs
chp = defPath "/ch/pointHat.wav"
addI "chp" $ aillenSampler 3 chp
shSh = defPath "/ch/ShHihat.wav"
addI "shSh" $ aillenSampler 3 shSh
shPsh = defPath "/ch/shPShat.wav"
addI "shPsh" $ aillenSampler 3 shPsh
mtlh = defPath "/ch/veryMetalHihat.wav"
addI "mtlh" $ aillenSampler 3 mtlh
cr01 = defPath "/cym/Crash01.wav"
addI "cr01" $ aillenSampler 3 cr01
crH = defPath "/cym/crashHighPitch.wav"
addI "crH" $ aillenSampler 3 crH
addI "crHl" $ aillenSampler 3 $ defPath "/cym/crashHPLonger.wav"
addI "crPz" $ aillenSampler 3 $ defPath "/cym/CymbolL.wav"
brokWin = defPath "/fx/BrokenWindow.aif"
addI "brokWin" $ aillenSampler 3 brokWin
pop = defPath "/fx/pop.wav"
addI "pop" $ aillenSampler 3 pop
shut = defPath "/fx/shutter.wav"
addI "shut" $ aillenSampler 3 shut
addI "gtStab" $ aillenSampler 3 $ defPath "/Gt/AmGtChopStab.wav"
addI "orStab" $ aillenSampler 3 $ defPath "/Key/GShmOrganStab.wav"
ohlE = defPath "/oh/arohlong.wav"
addI "ohlE" $ aillenSampler 3 ohlE
ohsE = defPath "/oh/arohshort.wav"
addI "ohsE" $ aillenSampler 3 ohsE
addI "ohSk" $ aillenSampler 3 $ defPath "/oh/hhopenSnake.wav"
cbE = defPath "/percIt/arcbcl.wav"
addI "cbE" $ aillenSampler 3 cbE
conRhi = defPath "/percIt/CongaRimHi.aif"
addI "conRhi" $ aillenSampler 3 conRhi
conMid = defPath "/percIt/MidConga.wav"
addI "conMid" $ aillenSampler 3 conMid
slapWa = defPath "/percIt/Smakwa.aif"
addI "slapWa" $ aillenSampler 3 slapWa
tbish = defPath "/percIt/tablaish.wav"
addI "tbish" $ aillenSampler 3 tbish
lilShake = defPath "/shakeIt/lilShake.wav"
addI "lilShake" $ aillenSampler 3 lilShake
ohsn = defPath "/shakeIt/ohsn.wav"
addI "ohsn" $ aillenSampler 3 ohsn
addI "GOT" $ aillenSampler 3 $ defPath "/vox/GOT.wav"
addI "makeit" $ aillenSampler 3 $ defPath "/vox/makeit.wav"
addI "dum" $ aillenSampler 3 $ defPath "/vox/VoxDum.wav"
addI "AaH" $ aillenSampler 3 $ defPath "/vox/voxHiAaA.wav"
addI "OoH" $ aillenSampler 3 $ defPath "/vox/voxHiOO.wav"
addI "Oh" $ aillenSampler 3 $ defPath "/vox/voxPointOH.wav"
addI "Ao" $ aillenSampler 3 $ defPath "/vox/voxStabAO.wav"
shae = defPath "/ch/aeHAT.WAV"
addI "shae" $ aillenSampler 3 shae
t808tml = defPath "/percIt/808_TomMidLow.wav"
addI "808tml" $ aillenSampler 3 t808tml
t808tl1 = defPath "/percIt/808_tomLow.wav"
addI "808tl1" $ aillenSampler 3 t808tl1
shakecs = defPath "/shakeIt/shakercsk.aif"
addI "shakecs" $ aillenSampler 3 shakecs
shatcs = defPath "/shakeIt/shakeHatcsk.aif"
addI "shatcs" $ aillenSampler 3 shatcs
chcs = defPath "/ch/shortHatcsk.aif"
addI "chcs" $ aillenSampler 3 chcs
rSw = defPath "/fx/RevCym.wav"
addI "rSw" $ aillenSampler 3 rSw
shtb = defPath "/shakeIt/LM-2_TAMB_1_TL.wav"
addI "shtb" $ aillenSampler 3 shtb
shnoi = defPath "/shakeIt/LM-2_SHAKER_1_TL.wav"
addI "shnoi" $ aillenSampler 3 shnoi
t808tl2 = defPath "/808/808_tom_4_TL.wav"
addI "808tl2" $ aillenSampler 3 t808tl2
hh808sh = defPath "/808/808_hi_hat_13_TL.wav"
addI "hh808sh" $ aillenSampler 3 hh808sh
ride626 = defPath "/626/ride.wav"
addI "ride626" $ aillenSampler 3 ride626
shkr626 = defPath "/626/shaker.wav"
addI "shkr626" $ aillenSampler 3 shkr626
tambo626 = defPath "/626/tambo.wav"
addI "tambo626" $ aillenSampler 3 tambo626
cb626 = defPath "/626/cowb.wav"
addI "cb626" $ aillenSampler 3 cb626
clv626 = defPath "/626/claves.wav"
addI "clv626" $ aillenSampler 3 clv626
colo626 = defPath "/626/conga-lo.wav"
addI "colo626" $ aillenSampler 3 colo626
comi626 = defPath "/626/conga-m.wav"
addI "comi626" $ aillenSampler 3 comi626
h626 = defPath "/626/hihat.wav"
addI "h626" $ aillenSampler 3 h626
ri626 = defPath "/626/ride.wav"
addI "ri626" $ aillenSampler 3 ri626
ri707 = defPath "/cym/707_Ride.wav"
addI "ri707" $ aillenSampler 3 ri707
fa1 = defPath "/fx/far1.aif"
addI "fa1" $ aillenSampler 3 fa1
er1mhh = defPath "/ER1/MicroHat.wav"
addI "er1mhh" $ aillenSampler 3 er1mhh
er1cr1 = defPath "/ER1/CrashCymbal1.wav"
addI "er1cr1" $ aillenSampler 3 er1cr1
er1cr2 = defPath "/ER1/CrashCymbal2.wav"
addI "er1cr2" $ aillenSampler 3 er1cr2
ch886 = defPath "/raveshaped/CH886.wav"
addI "ch886" $ aillenSampler 3 ch886
chdb = defPath "/raveshaped/CHDigiboy.wav"
addI "chdb" $ aillenSampler 3 chdb
chroger = defPath "/raveshaped/CHRoger.wav"
addI "chroger" $ aillenSampler 3 chroger
ohmlfx = defPath "/raveshaped/OHMalefix.wav"
addI "ohmlfx" $ aillenSampler 3 ohmlfx
chxtan = defPath "/raveshaped/CHExtraTan.wav"
addI "chxtan" $ aillenSampler 3 chxtan
mp = defPath "/iclc/mp.wav"
addI "mp" $ aillenSampler 3 mp
dur "mp" [15] k
ez = defPath "/iclc/cart.wav"
addI "ez" $ aillenSampler 3 ez
dur "ez" [1.5] k
ec = defPath "/iclc/nic.wav"
addI "tril" $ aillenSampler 3 ec
dur "tril" [1] k
hhakubra = defPath "/ch/akubra.wav"
addI "hhakubra" $ aillenSampler 3 hhakubra
hat2 = defPath "/ch/hat2.wav"
addI "hat2" $ aillenSampler 3 hat2
hhbowler = defPath "/ch/bowler.wav"
addI "hhbowler" $ aillenSampler 3 hhbowler
hhdurag = defPath "/ch/durag.wav"
addI "hhdurag" $ aillenSampler 3 hhdurag
spaceperc1 = defPath "/spaceperc/spaceperc1.wav"
addI "spaceperc1" $ aillenSampler 3 spaceperc1
spaceperc2 = defPath "/spaceperc/spaceperc2.wav"
addI "spaceperc2" $ aillenSampler 3 spaceperc2
spaceperc3 = defPath "/spaceperc/spaceperc3.wav"
addI "spaceperc3" $ aillenSampler 3 spaceperc3
spaceperc4 = defPath "/spaceperc/spaceperc4.wav"
addI "spaceperc4" $ aillenSampler 3 spaceperc4
spaceperc5 = defPath "/spaceperc/spaceperc5.wav"
addI "spaceperc5" $ aillenSampler 3 spaceperc5
spaceperc6 = defPath "/spaceperc/spaceperc6.wav"
addI "spaceperc6" $ aillenSampler 3 spaceperc6
spaceperc7 = defPath "/spaceperc/spaceperc7.wav"
addI "spaceperc7" $ aillenSampler 3 spaceperc7
spaceperc8 = defPath "/spaceperc/spaceperc8.wav"
addI "spaceperc8" $ aillenSampler 3 spaceperc8
hit1 = defPath "/Hits/Hit1.wav"
addI "hit1" $ aillenSampler 3 hit1
hit2 = defPath "/Hits/Hit2.wav"
addI "hit2" $ aillenSampler 3 hit2
hit3 = defPath "/Hits/Hit3.wav"
addI "hit3" $ aillenSampler 3 hit3
hit4 = defPath "/Hits/Hit4.wav"
addI "hit4" $ aillenSampler 3 hit4
hit5 = defPath "/Hits/Hit5.wav"
addI "hit5" $ aillenSampler 3 hit5
hit6 = defPath "/Hits/Hit6.wav"
addI "hit6" $ aillenSampler 3 hit6
hit7 = defPath "/Hits/Hit7.wav"
addI "hit7" $ aillenSampler 3 hit7
ptc = defPath "/vox/ProgramTheComputer.wav"
addI "ptc" $ aillenSampler 3 ptc
wat = defPath "/vox/wat.wav"
addI "wat" $ aillenSampler 3 wat
bongo1 = defPath "/bongo/Bongo1.wav"
addI "bongo1" $ aillenSampler 3 bongo1
bongo2 = defPath "/bongo/Bongo2.wav"
addI "bongo2" $ aillenSampler 3 bongo2
bongo3 = defPath "/bongo/Bongo3.wav"
addI "bongo3" $ aillenSampler 3 bongo3
bongo4 = defPath "/bongo/Bongo4.wav"
addI "bongo4" $ aillenSampler 3 bongo4
bongo5 = defPath "/bongo/Bongo5.wav"
addI "bongo5" $ aillenSampler 3 bongo5
alzir = defPath "/breaks/Alzir.wav" -- TRACK 5: BREAKS & STUTTERS
addI "alzir" $ aillenStutter 5 alzir
back = defPath "/breaks/Back.wav"
addI "back" $ aillenStutter 5 back
bad = defPath "/breaks/Bad.wav"
addI "bad" $ aillenStutter 5 bad
blue = defPath "/breaks/Blue.wav"
addI "blue" $ aillenStutter 5 blue
boo = defPath "/breaks/Boo.wav"
addI "boo" $ aillenStutter 5 boo
booWorm = defPath "/breaks/BooWorm.wav"
addI "booWorm" $ aillenStutter 5 booWorm
bringingMe = defPath "/breaks/BringingMe.wav"
addI "bMe" $ aillenStutter 5 bringingMe
bulldozer = defPath "/breaks/Bulldozer.wav"
addI "bdoze" $ aillenStutter 5 bulldozer
bulldozer2 = defPath "/breaks/Bulldozer2.wav"
addI "bdoze2" $ aillenStutter 5 bulldozer2
control = defPath "/breaks/Control.wav"
addI "control" $ aillenStutter 5 control
dperc = defPath "/breaks/DetroitPercussion.wav"
addI "dperc" $ aillenStutter 5 dperc
essential = defPath "/breaks/Essential.wav"
addI "essential" $ aillenStutter 5 essential
heaven = defPath "/breaks/Heaven.wav"
addI "heaven" $ aillenStutter 5 heaven
hurtuso = defPath "/breaks/HurtUSo.wav"
addI "hus" $ aillenStutter 5 hurtuso
johnny = defPath "/breaks/Johnny.wav"
addI "johnny" $ aillenStutter 5 johnny
liberty = defPath "/breaks/Liberty.wav"
addI "liberty" $ aillenStutter 5 liberty
massive = defPath "/breaks/Massive.wav"
addI "massive" $ aillenStutter 5 massive
peacesign = defPath "/breaks/PeaceSign.wav"
addI "peacesign" $ aillenStutter 5 peacesign
pressin = defPath "/breaks/Pressin.wav"
addI "pressin" $ aillenStutter 5 pressin
ricochet = defPath "/breaks/Ricochet.wav"
addI "ricochet" $ aillenStutter 5 ricochet
rise = defPath "/breaks/Rise.wav"
addI "rise" $ aillenStutter 5 rise
ror = defPath "/breaks/RollOnRoll.wav"
addI "ror" $ aillenStutter 5 ror
rust = defPath "/breaks/Rust.wav"
addI "rust" $ aillenStutter 5 rust
samurai = defPath "/breaks/Samurai.wav"
addI "samurai" $ aillenStutter 5 samurai
sovreign = defPath "/breaks/Sovreign.wav"
addI "sovreign" $ aillenStutter 5 sovreign
tear = defPath "/breaks/Tear.wav"
addI "tear" $ aillenStutter 5 tear
terrorist = defPath "/breaks/Terrorist.wav"
addI "terrorist" $ aillenStutter 5 terrorist
walk = defPath "/breaks/Walk.wav"
addI "walk" $ aillenStutter 5 walk
bibop = defPath "/breaks/Bibop.wav"
addI "bibop" $ aillenStutter 5 bibop
creek = defPath "/breaks/Creek.wav"
addI "creek" $ aillenStutter 5 creek
orb = defPath "/breaks/Orb.wav"
addI "orb" $ aillenStutter 5 orb
karate = defPath "/breaks/Karate.wav"
addI "karate" $ aillenStutter 5 karate
wazo = defPath "/breaks/Wazo.wav"
addI "wazo" $ aillenStutter 5 wazo
horn = defPath "/breaks/HornBreak.wav"
addI "horn" $ aillenStutter 5 horn
jzy = defPath "/breaks/JazzyBreak9A.wav"
addI "jzy" $ aillenStutter 5 jzy
mys = defPath "/breaks/Mystique.wav"
addI "mys" $ aillenStutter 5 mys
jzn = defPath "/breaks/JazzNoteBreak2.wav"
addI "jzn" $ aillenStutter 5 jzn
ohbre = defPath "/breaks/OhBreak1A.wav"
addI "ohbre" $ aillenStutter 5 ohbre
pboi = defPath "/breaks/Playboy Break - 6B.wav"
addI "pboi" $ aillenStutter 5 pboi
beb = defPath "/breaks/Wheel Up 2 Bad.wav"
addI "beb" $ aillenStutter 5 beb
hoco = defPath "/breaks/horny_conga.wav"
addI "hoco" $ aillenStutter 5 hoco
letthebeat = defPath "/vox/LetTheBeat.wav"
addI "letthebeat" $ aillenStutter 5 letthebeat
cmksyn = defPath "/stabSynth/ChopCarmack.wav"
addI "cmksyn" $ aillenStutter 5 cmksyn
mpstut = defPath "/iclc/mp.wav"
addI "mpstut" $ aillenStutter 5 mpstut
dur "mpstut" [15] k
csv1 = markPath "/Test.csv" -- MARKOV TABLES & SCRIPTS
csv2 = markPath "/Test2.csv"
csv3 = markPath "/Test3.csv"
csvTechno = markPath "/Techno.csv"
csvAcid = markPath "/Acid.csv"
csvElectro = markPath "/Electro.csv"
csvJungle = markPath "/Jungle.csv"
csvIDM = markPath "/IDM.csv"
csvTrance = markPath "/Trance.csv"
csvBreakDnB = markPath "/Break_DnB.csv"
csvBreakJungle = markPath "/Break_Jungle.csv"
csvBreakVsnare = markPath "/Break_Vsnare.csv"
csvBreakAe = markPath "/Break_Ae.csv"
csvStutsDnB = markPath "/Stuts_DnB.csv"
csvStutsVsnare = markPath "/Stuts_Vsnare.csv"
csvStutsAe = markPath "/Stuts_Ae.csv"
csvKitT = markPath "/Kit_Techno.csv"
csvKitJ = markPath "/Kit_Jungle.csv"
:! clear
