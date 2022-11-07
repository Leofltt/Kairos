cT 124

volchorus (Pd 0.2)
fbdel (Pd 0.44)
volrev (Pd 0.88)

vol "sSaw" [Pd 0.03] k
addC "sSaw" "sS" $ shine 4 "~*~~*~~*~~*~~*"
chorus "sSaw" [Pd 0.6] k
pitch "sSaw" (toPfs (withScale 47 phrygian)) $ runMarkovCSV csv3

cf "sSaw" [Pd 5000, Pd 800, Pd 2200, Pd 3200] $ np 40

del "sSaw" [Pd 0.3] k

detune "sSaw" [Pd 0.8, Pd 0.3, Pd 0.1] a


fbrev (Pd 0.9)


vol "glass" [Pd 0.11] k


rev "glass" [Pd 0.2] k

addC "cp808d" "tt" $ toTP [1]
rev "cp808d" (toPfs [0, 0.1 .. 0.4]) a

vol "303" [Pd 1] k
cf "303" [Pd 900] k

del "303" [Pd 0.8, Pd 0.7] rnd

durTS "303" [Pd (1/16)] k
pitch "303" [Pd 35, Pd 47, Pd 40, Pd 47,Pd 40, Pd 35,Pd 52, Pd 47] a
addC "303" "t" =<< patternWithDensity 16 64 40
addC "cp808d" "tt" $ toTP [1]
rev "cp808d" (toPfs [0, 0.1 .. 1]) a

cPat "sixteenN" "CH808" 

wf02 "303" [Pd 10] k

vol "CH808" (toPfs $ [0.4 .. 0.9] ++ [0.9, 0.6 .. 0.4]) a


