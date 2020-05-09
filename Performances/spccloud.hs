addC "pop" "t1" =<< patternWithDensity 8 16 37

pan "pop" [Pd 0.2, Pd 0.6, Pd 0.8, Pd 0.2, Pd 1] (percentNext 67)
vol "pop" [Pd 1.2, Pd 1, Pd 0.7, Pd 0.6] randomize


addC "CH808" "hh1" $ textToTP 4 "ii"
vol "CH808" [Pd 0.7] keep


addC "kN" "ke1" $ evolve 2 (interp1 4) dbk

cPat "irsn" "mtlh" >> vol "mtlh" [Pd 0.5] keep
