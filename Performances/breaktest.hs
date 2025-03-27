
-- liberty , sovereign, tear

 
cPat "eightN" "bibop"
durTS "bibop" [(1/8)] k
stuts "bibop" [1, 1] nv
divs "bibop" [8] k
pick "bibop" [0,1..7] nv

cPat "eightN" "creek"
durTS "creek" [(1/8)] k
stuts "creek" [1, 1] nv
divs "creek" [8] k
pick "creek" [0,1..7] nv 

cPat "eightN" "orb"
durTS "orb" [(1/8)] k
stuts "orb" [1, 1] nv
divs "orb" [8] k
pick "orb" [0,1..7] nv 

cPat "eightN" "karate"
durTS "karate" [(1/8)] k
stuts "karate" [1, 1] nv
divs "karate" [8] k
pick "karate" [0,1..7] nv 

-- qxc 001 - tear, ch808, ohle, er1k, er1kd | mod: fracture texture | abl: razor

cT 140

-- forgot to write down the performance :(

-- qxc 002 - hus, broken ; sh, sj2, kN or johnny | live bass

cT 147

let pat = interleave downB dbk
addC "hus" "pat" pat
durTS "hus" [Pd (1/8)] keep
stuts "hus" [Pd 1] keep
divs "hus" [Pd 8] keep
pick "hus" (toPfD [0,1..7]) nextVal 

durTS "johnny" [Pd (1/16)] keep
addC "johnny" "b1" =<< patternWithDensity 8 32 88

pick "hus" (toPfD [2,1,3,4,1,6,2,2,4]) randomize
addC "hus" "c" c

vol "kN" [Pd 0.8] keep

cPat "eightN" "johnny"
durTS "johnny" [Pd (1/8)] keep
pick "johnny" (toPfD [0,1..7]) nextVal
divs "johnny" [Pd 8] keep
stuts "johnny" (toPfD ((take 8 [1,1..])++[2])) keep

cPat "eightN" "sh" 
vol "sh" [Pd 0.7] keep
p "sh"

vol "sj2" [Pd 0.7] keep
cPat "jGhost" "sj2"
addC "sj2" "e1" $ evolve 1 (interp1 4) jGhost

-- qxc 003 - 808s, dubby (hov maybe) 

cT 137 -- maybe ?







-- q x c 004 -- SLR , feat Cancer Singer | ftwrk

cT 160
m_vol 0.9

p3 = interleave fwk1 fwk2

cPat "fwk1" "kbaSh"
vol "kbaSh" [Pd 0.8] keep
p "kbaSh"


