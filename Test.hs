
-- run stuff from LoadMe.hs first



-- PERFORMANCE
----------------------


displayIns
displayTP


--
-- WONDERVILLE




params "hov" [ (keep, vol, [Pd 0.8]),(nextVal, cf, [Pd 900, Pd 1222]),((runMarkov csv2), freq, toPfD (withScale 30 aeolian)), (randomize, pan, (toPfD [0, 1, 0.8, 0.2])),(randomize,dur,toPfD[ 1.7,2,1.5]) ]
addC "hov" "tre" $ toTP $ tupleForBar 4 3
p "hov"

cPat "uno" "kbaSh"
p "kbaSh"











--
--


solo "ohsE"

cPat "eightN" "sh"
addC "kN" "jgk" jgk
addC "siS" "jgs"  jgs

cPat "downB" "cp808m"
addC "kN" "ukgk" ukgk

cPat"ukgrs" "r707"
cPat "ukgch" "sh"
mapM_ p ["r707", "sh"]



addC "r707" "irb" $ toTP $ evolve 2 (interp1 4) (fromTP ukgrs)

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
