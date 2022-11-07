addI "mc1" $ models csd1 1
addI "mc2" $ models csd1 2
addI "mc3" $ models csd1 3
addI "mc4" $ models csd1 4
addI "mc5" $ models csd1 5
addI "mc6" $ models csd1 6


cT 124

volchorus (Pd 0.2)
fbdel (Pd 0.44)
volrev (Pd 0.88)

addC "mc5" "55" $ toTP [8.7, 17]

addC "mc5" "55" $ toTP [9.33]

addC "mc5" "5" $ toTP [5.33]

cPat "kpanb" "mc4" 

cPat "irsn" "mc1" 
p "mc1"

addC "mc2" "22" =<< patternWithDensity 64 128 44

pan "mc2" (toPfs [10, 50, 80, 110]) rnd

cPat "dubb" "cp808d"

pan "cp808d" [Pd 0.1, Pd 0.4, Pd 0.6, Pd 0.7] rnd

s "cp808m"


addC "cp808d" "ok" =<< patternWithDensity 16 64 55

vol "cp808d" [Pd 0.4] k

del "cp808d" (toPfs [0.2, 0.5, 0.3, 0]) $ percentNext 88

del "glass" (toPfs [0.2, 0.4, 0.5, 0]) rnd

rev "glass" [Pd 0.3,Pd 0.6, Pd 0.2] $ percentNext 56

addC "mc6" "b" $ shine 4 "~*~*~~*~~*~~"

addC "mc4" "t" $  shine 4 "**~~*~**~~**~*~*"

rev "cp808d" [Pd 0.3] k

durTS "sovreign" [Pd (1/16)] keep
pick "sovreign" (toPfD [0..15]) nv
stuts "sovreign" (toPfD [1]) k



addC "sovreign" "pr1" =<< patternWithDensity 8 32 77
