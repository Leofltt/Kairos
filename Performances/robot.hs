
------------------------------------
--                                --
-- roBOt Festival                 --
--                                --
--                                --
--                       @leofltt --
------------------------------------

cT 147
m_vol 0.85

addI "mc1" $ modelcycles 1
addI "mc2" $ modelcycles 2
addI "mcs3" $ modelcycles 3
addI "mc4" $ modelcycles 4
addI "mc5" $ modelcycles 5
addI "mc6" $ modelcycles 6

params "hov" [ (keep,durTS,toPfD[ 2]),(keep, vol, [Pd 0.5]),(keep, cf, [Pd 900]),(nextVal, pitch, toPfD ([35, 36, 35, 35])),(keep, rev, [Pd 0.8])  ]

scaleBass = withScale 35 harmMin

durTS "303" [Pd (1/4)] keep
pitch "303" (toPfD [45,40,40]) $ percentNext 70
cPat "sixteenN" "303"
cf "303" [Pd 6000] keep
wf02 "303" [Pd 0] keep



addC "mc1" "eu1" $ euclid (7,8) 0 16


params "hov" [ (keep,durTS,toPfD[ 1]),(keep, vol, [Pd 0.5]),(keep, cf, [Pd 900]),((runMarkovCSV csv3), pitch, toPfD (scaleBass)),(keep, rev, [Pd 0.8])  ]

tab = [[0.7, 0.15, 0.15],[0.8,0.2,0],[0.5,0.3,0.2]]
pitch "303" (toPfD [35,33,40]) $ runMarkov tab

-- back, blue, control, heaven, dperc

cPat "sixteenN" "heaven"
durTS "heaven" [Pd (1/16)] keep
stuts "heaven" [Pd 1] keep
divs "heaven" [Pd 16] keep
pick "heaven" (toPfD [0,1..15]) nextVal
vol "heaven" (toPfD [0.3]) keep


cPat "sixteenN" "back"

durTS "back" [Pd (1/16)] keep
stuts "back" [Pd 1] keep
divs "back" [Pd 16] keep

pick "back" (toPfD [0,1..15]) $ percentNext 77


vol "back" [Pd 0.4] keep

cPt at "eightN" "blue"
durTS "blue" [Pd (1/8)] keep
stuts "blue" [Pd 1] keep
divs "blue" [Pd 8] keep
pick "blue" (toPfD [0,1..7]) nextVal
vol "blue" (toPfD 0.7) keep


cPat "sixteenN" "control"

durTS "control" [Pd (1/16)] keep

stuts "control" [Pd 1, Pd 1, Pd 1, Pd 2] randomize

divs "control" [Pd 16] keep

pick "control" (toPfD [0,1..15]) $ percentNext 67

vol "control" [Pd 0.3] keep


let pat = interleave kpanb 

maybeAddC "mc2" "s1" =<< patternWithDensity 8 32 18

addC "back" "eu1" $ euclid (11,16) 0 16
