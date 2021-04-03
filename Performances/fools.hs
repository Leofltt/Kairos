


------------------------------------
--                                --
-- TOPLAP ITALIA                  --
-- April's Fools Streaming        --
--                                --
--                       @leofltt --
------------------------------------

cT 147


addI "mck" $ modelcycles 1
addI "mcp" $ modelcycles 2
addI "mcsn" $ modelcycles 3
addI "mcf" $ modelcycles 4
addI "mcb" $ modelcycles 5
addI "mch" $ modelcycles 6

params "hov" [ (keep,durTS,toPfD[ 2]),(keep, vol, [Pd 0.5]),(keep, cf, [Pd 900]),(nextVal, pitch, toPfD (35, 36, 35, 35)),(keep, rev, [Pd 0.8])  ]


pitch "mcp" (toPfD [61,58,57]) nextVal

maybeAddC "mcp" "p1" =<< patternWithDensity 4 8 45

let pat = interleave downB dbk
addC "hus" "pat" pat
durTS "hus" [Pd (1/8)] keep
stuts "hus" [Pd 1] keep
divs "hus" [Pd 8] keep
pick "hus" (toPfD [0,1..7]) nextVal

cPat "sixteenN" "dperc"
durTS "dperc" [Pd (1/16)] keep
stuts "dperc" [Pd 1] keep
divs "dperc" [Pd 16] keep

pick "dperc" (scramble $ toPfD [0,1..15]) (percentNext 55)

cPat "sixteenN" "control"
durTS "control" [Pd (1/16)] keep

stuts "control" (toPfD [1, 1, 1, 2, 1]) nextVal

divs "control" [Pd 16] keep

pick "control" (scramble $ toPfD [0,1..15]) (percentNext 77)


maybeAddC "mcb" "b1" =<< patternWithDensity 8 32 68

pitch "mcb" (scramble $ toPfD [59, 60, 59, 64]) $ percentNext 78

pitch "mcf"  (toPfD [59, 64])  $ percentNext 47 

cPat "sixteenN" "mch" >> p "mch"

maybeAddC "mcb" "b2" =<< patternWithDensity 4 16 66

maybeAddC "mck" "kk" =<< patternWithDensity 4 16 56 

addC  "mcsn" "0" [TP 1.25] >> p "mcsn"

cPat "upFour" "ohsE" >> p "ohsE"

vol "ohsE" [Pd 0.5] keep
