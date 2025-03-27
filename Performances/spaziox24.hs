addI "mc1" $ models csd1 1
addI "mc2" $ models csd1 2
addI "mc3" $ models csd1 3
addI "mc4" $ models csd1 4
addI "mc5" $ models csd1 5
addI "mc6" $ models csd1 6

-- osc instrs

addI "qrk" $ oscInstr 88 "11100" [(new_pfId 2 "p1tch",Pd 38),(new_pfId 3 "dur",Pd 11110),(new_pfId 4 "vol",Pd 0.8),(new_pfId 5 "matx", Pd 0.25),(new_pfId 6 "maty", Pd 0.7)]
p1tch i list fun = addPf i 2 "p1tch" (toPfs list) fun 
matx i list fun = addPf i 5 "matx" (toPfs list) fun 
maty i list fun = addPf i 6 "maty" (toPfs list) fun 

addI "cel" $ oscInstr 89 "11100" [(new_pfId 2 "p1tch",Pd 38),(new_pfId 3 "dur",Pd 11110),(new_pfId 4 "vol",Pd 0.8),(new_pfId 5 "matx", Pd 0.25),(new_pfId 6 "maty", Pd 0.7)]

addI "xeno" $ oscInstr 90 "11100" [(new_pfId 2 "p1tch",Pd 38),(new_pfId 3 "dur",Pd 11110),(new_pfId 4 "vol",Pd 0.8),(new_pfId 5 "matx", Pd 0.25),(new_pfId 6 "maty", Pd 0.7)]

addC "xeno" "x1" [TP 4.25]
durTS "xeno" [1] $ k 
durTS "cel" [1] $ k 
durTS "qrk" [1] $ k
addC "qrk" "q1" [TP 2.25]
addC "cel" "c1" [TP 5.25]

-- hi y : bass, low x : hi-p reso 

matx "cel" [0.9] k 
maty "cel" [0.9] k

-- round : low y,  sharp: hi y

matx "xeno" [0.8] k
maty "xeno" [0.1] k


-- low x, hi y. Low y: bb  

matx "qrk" [0.1] k
maty "qrk" [0.9] k

pitch "mc3" [37, 37, 37, 37, 38, 37, 37] nv

cT 169
dtdel $ Pd 270


dtdel $ Pd 270
cPat "dbk" "mc1" >> p "mc1"

cPat "sixteenN" "mc2" >> p "mc2"


-- breaks : creek, booWorm, heaven, rust, wazo

cPat "sixteenN" "rust"
durTS "rust" [(1/16)] $ k
pick "rust" [0..31] $ nv
divs "rust" [32] $ k
stuts "rust" [1] $ k

cPat "eightN" "booWorm"
durTS "booWorm" [(1/8)] k
pick "booWorm" [0..15] nv
divs "booWorm" [16] k
stuts "booWorm" [1] k

cat "sixteenN" "wazo"
durTS "wazo" [(1/16)] k
pick "wazo" [0..31] nv
divs "wazo" [32] k
stuts "wazo" [1] k

pitch "mc3" [36, 35, 37] $ nv


cPat "ukgrs" "mc5" >> p "mc5"

cPat "kpanc" "cbE" >> p "cbE"
vol "cbE" [0.4] k


del "lilShake" [0.3, 0.4, 0.1, 0.0] $ rnd

addC "cp808m" "ls" =<< patternWithDensity 16 32 65

addC "mc4" "tr1"  =<< patternWithDensity 16 8 47 

addC "mc3" "ww" =<< patternWithDensity 8 16 70
pitch "mc3" [60] k


