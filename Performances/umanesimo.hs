displayIns





cT 147



addC "st1" "p1" =<< patternWithDensity 8 16 47

sample "st1" [Ps st1, Ps st2] randomize

p1 <- getTP "p1"
addC "st1" "p2" $ evolve 1 (interp1 8) p1

cPat "upFour" "CH808"
vol "CH808" [Pd 0.7] keep

cPat "irk1" "kc1"
p "kc1"

addC "tbish" "p5" =<< patternWithDensity 4 16 33
vol "tbish" [Pd 0.2] keep
pan "tbish" [Pd 0.7, Pd 0.3, Pd 0.4, Pd 0.6] randomize
p "tbish"

addC "lpFM" "p4" =<< patternWithDensity 8 5 40
