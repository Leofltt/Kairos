-- all the model$

addI "k1k" $ models "11000" 1
addI "nXz" $ models "11000" 2
addI "sNr" $ models "11000" 3
addI "r1d" $ models "11000" 4
addI "b2S" $ models "11000" 5 
addI "chd" $ models "11000" 6
 
-- Reson FM thingy

addI "p4l" $ oscInstr 88 "11100" [(new_pfId 2 "p1tch",Pd 49),(new_pfId 3 "dur",Pd 11110),(new_pfId 4 "vol",Pd 0.8),(new_pfId 5 "fuz3", Pd 0.25),(new_pfId 6 "p4n", Pd 0.7)]
p1tch i list fun = addPf i 2 "p1tch" (toPfs list) fun 
fuz3 i list fun = addPf i 5 "fuz3" (toPfs list) fun 
p4n i list fun = addPf i 6 "p4n" (toPfs list) fun 

-- Breakz

cPat "eightN" "bibop"
durTS "bibop" [(1/16)] k
stuts "bibop" [1, 1] nv
divs "bibop" [16] k
pick "bibop" [0,1..15] nv

cPat "eightN" "creek"

durTS "dperc" [(1/16)] k
stuts "dperc" [1, 1, 1, 1, 2] rnd
divs "dperc" [16] k
pick "dperc" [0,1..15] $ percentNext 88 
vol "dperc" (0, 0.5, 0.8, 0.2) rnd

cPat "eightN" "orb"
durTS "orb" [(1/8)] k
stuts "orb" [1, 1] nv
divs "orb" [16] k

pick "orb" [0,1..15] rnd 

cPat "eightN" "karate"
durTS "karate" [(1/8)] k
stuts "karate" [1, 1] nv
divs "karate" [16] k

pick "karate" [0,1..7] rnd 


-- PERF

cT 138

pitch "303" [49,49,49,45,49] $ rnd

durTS "303" [(1/8)] k
cf "303" [2000] k
wf02 "303" [10] k



addC "creek" "swww" =<< patternWithDensity 16 32 66
