-- EULERROOM EQUINOX 2020
--
-- @leofltt
-- @livecodenyc



addC "lpFM" "hi" $ textToTP 16 "hi"
p "lpFM"

cPat "sixteenN" "sh"
vol "sh" [Pd 0.7, Pd 0] (percentNext 88)
p "sh"

cPat "kpanc" "K909"

cPat "ukgch" "cp808d"

addC "cp808d" "ii" $ textToTP 12 "ii"

cPat "sixteenN" "303"
vol "303" [Pd 0.2, Pd 0, Pd 0.3] randomize
p "303"

s "r707"

cPat "bouncyk" "k1626"
vol "k1626" [Pd 0.4] keep
p "k1626"

cPat "ukgrs" "cp808d"
vol "cp808d" [Pd 0.3] keep
p "cp808d"


cT 147
fbrev 0.915
params "lpFM" [ (keep, vol, [Pd 0.7]),(keep, rev, [Pd 0.5]),((percentNext 80), pitch, toPfD [51, 54, 56, 51, 46]),(keep, fmIndx, [Pd 12.3]), (keep, fmDepth, [Pd 987.23]), (keep, dist, [Pd 1.15]),(keep,cf,[Pd 3880]),( keep, rev, [Pd 0.6]),(keep, adRatio,[Pd 0.7]),(randomize,pan,toPfD[0.4,0.6,0.75,0.35])]
params "303" [ (keep,dur,toPfD[ 0.25]),(keep,res,[Pd 4]),(keep, vol, [Pd 0.5]),(keep, cf, [Pd 3500]),((runMarkov csv3), pitch, toPfD (withScale 39 phrygian)),(keep, rev, [Pd 0.4])]
params "hov" [ (keep,dur,toPfD[ 1.2]),(keep, vol, [Pd 0.3]),(keep, cf, [Pd 800]),((runMarkov csv3), pitch, toPfD (withScale 39 phrygian)),(keep, rev, [Pd 0.8])  ]




























addC "cbE" "text1" $ textToTP 12 "kyy"

addC "CH808" "evolve2" $ evolve 1 (interp1 8)  upFour

addC "303" "evolve1" $ evolve 1 (interp1 12)  adb

addC "kcj" "tre" $ tupleForBar 8 3
