-- all the model$

addI "k1k" $ models "11000" 1
addI "nXz" $ models "11000" 2
addI "sYn" $ models "11000" 3
addI "r1d" $ models "11000" 4
addI "b2S" $ models "11000" 5 

addI "b4s" $ models "11000" 6

-- dr0nez4Liv3

addI "dr0" $ oscInstr 88 "11200" [(2,Pd 49),(3,Pd 1110),(4,Pd 0.9),(5, Pd 14),(6, Pd 0.5)] 
p1tch i list fun = addPf i 2 "p1tch" (toPfs list) fun  
p4n i list fun = addPf i 6 "p4n" (toPfs list) fun 
b34t i list fun = addPf i 5 "b34t" (toPfs list) fun 

--

cT 96

pitch "b4s" [57, 54, 51] $ runMarkov [[0.4,0.48, 0.12],[0.6,0.13, 0.17],[0.04, 0.36, 0.6]]

addC "b4s" "bb" $ toTP [1.7, 6.4, 13.8]

vol "cp808m" [0.7] k

p1tch "dr0" [53, 55, 49] $ runMarkov [[0.1,0.3,0.6],[0.1,0.6,0.3],[0.2,0.1,0.7]]

cPat "r12" "r1d"

pan "r1d" [10, 110, 67] rnd

p1tch "dr0" [49] k 


addC "r1d" "r2" =<< patternWithDensity 16 32 33

addC "k1k" "k1" $ toTP [0, 3.75, 8, 6.5]




