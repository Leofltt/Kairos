-- init model : samples

addI "ms1" $ models csd1 1
addI "ms2" $ models csd1 2
addI "ms3" $ models csd1 3
addI "ms4" $ models csd1 4
addI "ms5" $ models csd1 5
addI "ms6" $ models csd1 6


cT 133

cPat "fourFloor" "ms1"
p "ms1"

prms "sSaw" [(cf,k,toPfs[1900]),(durTS,k,toPfs[(1/16)]),(pitch,nv,toPfs[47,47,47,51,47,47]),(sawmix,np 70,toPfs[0.66, 0.8, 0.5]),(detune,k,[Pd 0.8])]
vol "sSaw" [Pd 0.4] k
durTS "sSaw" [Pd (1/16)] k

cPat "sixteenN" "sSaw" >> p "sSaw"


cPat "upFour" "ms4" >> p "ms4"



maybeAddC "ms1" "s1" =<< patternWithDensity 8 16 54


pitch "ms5" (toPfs [47, 47, 47, 46, 47, 47]) a

pitch "ms5" (toPfs [47, 35, 59]) a


pitch "sSaw" (toPfs [37, 37, 37, 37, 39) a
addC "sSaw" "saw1" $ shine 4 "~~~*~~~*~*~~~*"
vol "sSaw" [Pd 0.4] k
p "sSaw"

addC "sSaw" "p1" $ euclid (3,8) 0 8

p "sSaw" 

cf "sSaw" [Pd 10000] keep

detune "sSaw" [Pd 0.1] k


addC "sSaw" "s1" $ shine 12 "~~*~~~~~**~~**~~~~*~~~*~~~~~*~"

p "ms3"

pitch "sSaw" (toPfs [35,35,47,47,35,47,35]) $ a

addC "sSaw" "p3" $ shine 4 "~~**~*~*~~*~**~"

addC "CH808" "e1" $ euclid (7,11) 0 8

vol "sSaw" [Pd 0.2] keep
cPat "eightN" "CH808" >> p "CH808"
vol "CH808" [Pd 0.3] keep

rev "sSaw" [Pd 0.6] keep
pan "sSaw" ([Pd 0, Pd 1])rnd
durTS "sSaw" ([Pd (1/16)]) keep


params "hov" [ (durTS,k,toPfs[ 2]),(vol,k, [Pd 0.5]),(cf,k, [Pd 900]),(pitch,(runMarkov csv3), toPfs (withScale 37 phrygian)),(rev, k, [Pd 0.8])  ]


maybeAddC "hov" "i" =<< patternWithDensity 8 16 30
vol "hov" [Pd 0.6] keep
cf "hov" [Pd 1500] keep

pitch "hov" (toPfs [37, 36, 37, 39]) (np 88)

durTS "hov" [Pd (1/2)] k

addC "sSaw" "sS" $ shine 4 "~~*~~*~*~*~~*"
chorus "sSaw" [Pd 0.3] k
pitch "sSaw" (toPfs (withScale 47 phrygian)) $ runMarkovCSV csv3
cf "sSaw" [Pd 5000, Pd 800, Pd 2200, Pd 3200] $ np 40

durTS "alzir" [Pd (1/16)] keep
stuts "alzir" [Pd 1] keep
divs "alzir" [Pd 16] keep
pick "alzir" (toPfs [0,1..15]) a 
durTS "dperc" [Pd (1/16)] keep
stuts "dperc" [Pd 1] keep
divs "dperc" [Pd 16] keep

pick "dperc" (scramble $ toPfs [0,1..15]) (percentNext 55)

-- fxs

cPat "fourFloor" "del" 
delt "del" (toPfs [411]) k
delfb "del" (toPfs [0.7, 0, 0.3, 0.1, 0.2]) rnd
delvol "del" (toPfs [0.3, 0.8, 0.66]) nv
p "del"

cPat "dubb" "rev"
revfb "rev" (toPfs [0.7, 0.6, 0.3, 0.8, 0.2]) rnd
revcf "rev" (toPfs [8000, 11000, 700, 3000]) rnd
revvol "rev" (toPfs [0.3, 0.8, 0.66]) nv
p "rev"
