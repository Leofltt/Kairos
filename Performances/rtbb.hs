--RTBB.hs

-- v 0.1

{-
samples ::
   - kicks: er1k, er1k2
   - hats: CH808, sh, ohlE
   - snare:
   - clap: cp808d
   - perc: er1r,
   - breaks: ricochet, pressin, sovreign, bulldozer2 (2 bars)
   - synths:
   - modulars: bass
-}

//D Harm Minor, 156 bpm

-- setup for breaks

divs "ricochet" [Pd 16] keep
durTS "ricochet" [Pd (1/8)] keep
pick "ricochet" (toPfD [0,1..15]) nextVal
divs "pressin" [Pd 16] keep
durTS "pressin" [Pd (1/8)] keep
pick "pressin" (toPfD [0,1..15]) nextVal
divs "sovreign" [Pd 16] keep
durTS "sovreign" [Pd (1/8)] keep
pick "sovreign" (toPfD [0,1..15]) nextVal
divs "bdoze2" [Pd 16] keep
durTS "bdoze2" [Pd (1/16)] keep
pick "bdoze2" (toPfD [0,1..15]) nextVal

-- setup for modular bass

addI "euro" $ other 88 [(2,Pd 48),(3, Pd 0),(4, Pd 0),(5, Pd 0)]
let note l f = addPf "euro" 2 l f
let dura l f = addPf "euro"  3 l f

-- set tempo and scales

cT 156
let scaleBass = withScale 38 dorian
let scaleMelody = offset 12 scaleBass
fbrev 0.8
cfrev 15000
fbdel 0.6
delchorus 315

m_vol 0.6

pitch "hov" ( offset 7 [38,41,36,38,45,38]) $ randomize
addC "hov" "h1" =<< patternWithDensity 16 8 47 
durTS "hov" [ 1.5] keep

durTS "sovreign" [Pd (1/16)] keep
stuts "sovreign" (toPfD ([2,2] ++(take 10 [1,1..1]))) randomize

pick "ricochet" (toPfD [1,2..15]) $ percentNext 66


cf "hov" [Pd 2600] keep
vol "hov" [Pd 0.5] keep

cPat "dbk2" "er1k2"

vol "er1k2" [Pd 0.8] keep
addC "CP909" "cp1" (toTP [1, 2.75])

addC "sh" "hh1" =<<patternWithDensity 8 32 82

addC "r707" "rer" =<< patternWithDensity 8 32 22

cPat "upFour" "CH808"
vol "CH808" [Pd 0.45] keep
tuning "CH808" [Pd 0.991] keep
 
addC "CP909" "cp2" (toTP [1, 3.25])

rev "CP909" [Pd 0, Pd 0.5] nextVal
del "CP909" [Pd 0, Pd 0.1] randomize

cPat "uno" "ricochet"

durTS "ricochet" [Pd (1/16)] keep 

addC "bdoze2" "r3" =<< patternWithDensity 8 16 86

s "CH808"
s "er1k2"

cPat "uno" "ricochet"

durTS "pressin" [Pd (1/8)] keep 
addC "pressin" "r1" =<<patternWithDensity 8 16 20
vol "pressin" [Pd 0.8] keep

addC "phax" "px1" =<< patternWithDensity 16 32 30
durTS "phax" [Pd (1/8)] keep
pitch "phax" (toPfD [50, 48]) $ randomize 
vol "phax" [Pd 0.5] keep
res "phax" [Pd 1.1] keep
cf "phax" [Pd 7777] keep
sepP "phax" [Pd 1.1, Pd 0.77] nextVal
rev "phax" [Pd 0.25] keep
simP "phax" [Pd (-0.1), Pd (0.1)] nextVal
adP "phax" [Pd 0.1] keep
edP "phax" [Pd 0.8] keep

durTS "303" [Pd (1/16)] keep
pitch "303" (toPfD [38,37,38,46]) $ percentNext 70
cPat "sixteenN" "303"
cf "303" [Pd 8000] keep
wf02 "303" [Pd 10] keep

addC "pressin" "pr2" =<< patternWithDensity 8 32 85

solo "pressin"

pick "sovreign" (toPfD [4..15]) randomize

stuts "pressin" (toPfD $ [2,2,4,5] ++ (take 28 [1,1..1])) randomize

durTS "sovreign" [Pd (1/16)] keep
addC "sovreign" "pr1" =<< patternWithDensity 8 32 77

cPat "sixteenN" "pressin" 

pick "pressin" (toPfD [0..15]) $ percentNext 88




