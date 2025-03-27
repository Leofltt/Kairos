-- all the model$

addI "b2S" $ models "11000" 5 
addI "b4s" $ models "11000" 6
addI "k1k" $ models "11000" 1
addI "sYn" $ models "11000" 3
addI "r1d" $ models "11000" 4
addI "nXz" $ models "11000" 2
 
-- Reson FM thingy

addI "p4l" $ oscInstr 88 "11100" [(2,Pd 49),(3,Pd 11110),(4,Pd 0.8),(5, Pd 0.25),(6, Pd 0.7)]
p1tch i list fun = addPf i 2 "p1tch" (toPfs list) fun 
fuz3 i list fun = addPf i 5 "fuz3" (toPfs list) fun 
p4n i list fun = addPf i 6 "p4n" (toPfs list) fun 

-- 13* bpm sounds reasonable

cT 131

params "hov" [ (keep,durTS,toPfs[ 2]),(keep, vol, [Pd 0.5]),(keep, cf, [Pd 900]),((runMarkov csv3), pitch, toPfs (withScale 39 phrygian)),(keep, rev, [Pd 0.8])  ]


addC "hov" "intro" =<< patternWithDensity 8 16 30

vol "hov" [0.6] keep

cf "hov" [1500] keep
pitch "hov" ((withScale 36 harm)) $ runMarkov csv3 

addC "p4l" "ok" [TP 3.22] 


vol "rimshock" [0.6] k

addC "rimshock" "shock" [TP 5.6, TP 21.6]

cPat "upFour" "ohsE" 

p "k1k"

pitch "b4s" [51,49,48,49,49,51,49] $ rnd


addC "p4l" "swww" =<< patternWithDensity 16 64 40

p "r1d"

pan "K909" ([10, 100, 70, 50]) $ rnd 

addC "r1d" "ww" $ shine 12 "~~*~*~~*~*~*~~*~*~~*~~~*~*~*~*"


cPat "downB" "cp808d"

vol "cp808d" ([0.66]) k
del "cp808d" ([0.6, 0, 0, 0]) rnd


fbdel $ Pd 0.2

dtdel $ Pd 347

pitch "hov" ( offset 7 [38,41,36,38,45,38]) $ randomize
addC "hov" "h1" =<< patternWithDensity 16 8 47 
durTS "hov" [ Pd 1.5] keep

