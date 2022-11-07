-- FLASHCRASH - June 4 2022

-- @leofltt

-- setup model:cycles

addI "mck" $ models csd1 1
addI "mcs" $ models csd1 2
addI "mcp" $ models csd1 3
addI "mcc" $ models csd1 4
addI "mct" $ models csd1 5
addI "mcb" $ models csd1 6

-- a comfy bpm

cT 131


-- s (2)
-- pats: s1, s2, s3, s4 

pitch "mcs" (toPfs [60, 65, 61, 59]) nv

addC "mcs" "s3" =<< patternWithDensity 12 12 55

-- b (6)
-- pats: b1

addC "mcb" "b1" $ textToTP 8 "hi"

pan "mcb" (toPfs [65]) k

pitch "mcb" (toPfs [60, 60, 60, 61, 59, 60, 60, 59]) $ rnd 

cPat "uno" "rimshock"
p "rimshock"

pan "rimshock" [Pd 0.3, Pd 0.5, Pd 0.7] rnd

vol "rimshock" [Pd 0.8] k
vol "rimshock" [Pd 0.8] k
rev "rimshock" [Pd 0.2] k
rev "rimshock" [Pd 0.2] k


addC "chroger" "r" $ shine 8 "~~~~~~*~"

pan "chroger" (toPfs [0.3, 0.15, 0.5, 0.8, 0.6]) rnd

cPat "downB" "chroger"

p "chroger"

addC "chdb" "db1" $ shine 4 "~~~~~**~~~~~~**~"

rev "chdb" (toPfs [0, 0.4, 0.7]) rnd
pan "chdb" (toPfs [0.3, 0.6, 0.5]) rnd

cPat "upFour" "ohmlfx" 
p "ohmlfx"

pan "mcc" [Pd 65] k
cPat "sixteenN" "mcc" 

