-- ====== HAPPY BIRTHDAY TOPLAP IT ! ======

cT 143

-- FX DEFAULTS

fbdel $ Pd 0.3
dtdel $ Pd 227
divchorus $ Pd 4
delchorus $ Pd 44
volchorus $ Pd 0.4
volrev $ Pd 0.88
fbrev $ Pd 0.81
cfrev $ Pd 11888

-- Breaks Defaults (using Markov for dynamic slicing)

prms "horn" [(durTS, k, [(1 / 8)]), (stuts, k, [1]), (divs, k, [16]), (pick, runMarkovCSV csvBreakJungle, [0 .. 15]), (cps, k, [(-1)])]
prms "pboi" [(durTS, k, [(1 / 8)]), (stuts, k, [1]), (divs, k, [16]), (pick, runMarkovCSV csvBreakJungle, [0 .. 15]), (cps, k, [(-1)])]
prms "beb" [(durTS, k, [(1 / 16)]), (stuts, k, [1]), (divs, k, [16]), (pick, runMarkovCSV csvBreakDnB, [0 .. 15]), (cps, k, [(-1)])]
prms "hoco" [(durTS, k, [(1 / 8)]), (stuts, k, [1]), (divs, k, [16]), (pick, runMarkovCSV csvBreakJungle, [0 .. 15]), (cps, k, [(-1)])]
cPat "eightN" "pboi"
cPat "eightN" "beb"
cPat "eightN" "horn"
cPat "eightN" "hoco"

-- Drum Samples

-- KICKS : "er1k2" -> muted, "er1k3" -> impact
-- HATS : "er1mhh" -> micro hi end, "chxtan" -> noisy, short, ghosty, "scs" -> shaker, "ohLe" -> open long hat
-- SNARES : "er1r" -> rimshot, "glsn" -> very effected dubstep-type thing, "sj1" -> short, dry, low jungle snare, "cp808d" -> classic dry 808 clap
-- CYM : "ri707" -> ride, "er1cr1" -> crash 1, "er1cr2" -> crash 2 (thinner)
-- PERCS : "bongo3", "bongo4", "bongo5"


cPat "fourFloor" "er1k3"

cPat "upFour" "chxtan"
cPat "jgs" "sj1"

-- Synth Defaults

softHov "hov"


durTS "sSaw" [(1 / 8)] k
stabbySaw "sSaw"

durTS "sSaw" [(1 / 4)] k
brightSaw "sSaw"


deep303 "303"
-- electro303 "303"

pitch "303" (withScale 41 dorian) $ runMarkovCSV csvAcid

-- Main performance kit
-- 1-3: Kicks, 4-7: Snares/Claps, 8-11: Hats, 12-14: Percs, 15-17: Cymbals/FX

let mainKit = newKit [ (1, Ps "er1k3"), (2, Ps "er1k2"), (3, Ps "K909"), (4, Ps "sj1"), (5, Ps "cp808d"), (6, Ps "er1r"), (7, Ps "glsn"), (8, Ps "chxtan"), (9, Ps "er1mhh"), (10, Ps "scs"), (11, Ps "ohLe"), (12, Ps "bongo3"), (13, Ps "bongo4"), (14, Ps "bongo5"), (15, Ps "ri707"), (16, Ps "er1cr1"), (17, Ps "hit4") ]

let bongos = newKit [(1, Ps "bongo1"),(2, Ps "bongo2"),(3, Ps "bongo3"),(3, Ps "bongo3"),(4, Ps "bongo4"),(5, Ps "bongo5")]

-- Instantiate missing instruments

addI "sub" $ fmSub
addI "stabs" $ superSaw
addI "kit" $ sampler ""
addI "bgs" $ sampler ""
addI "glitch" $ stutter ""
addI "hov" $ hoover
addI "strings" $ stringPad




-- Scale Definition (D Dorian)
-- D3 = 50, D2 = 38

let dDorian = withScale 50 dorian
let d2Dorian = withScale 38 dorian 
scal = [38.0, 40.0, 41.0, 43.0, 45.0, 48.0]



-- Kit Markov Variations

let kitT = runKitMarkov "MarkovTables/Kit_Techno.csv" mainKit
let kitJ = runKitMarkov "MarkovTables/Kit_Jungle.csv" mainKit
let kitI = runKitMarkov "MarkovTables/IDM.csv" mainKit

-- =============================================================================
-- Bass 

pitch "303" [38] k

addC "303" "offbeat" $ toTP [0.5, 1.5, 2.5, 3.5]

pitch "303" [38, 38, 41, 38] nv

cPat "sixteenN" "303"

pitch "303" [38] k 
addC "303" "drone" [0]

-- Hoover: Subs & Low End Beds

pitch "hov" [41, 38] rnd >> durTS "hov" [2] k>> cf "hov" [2000] k >> res "hov" [10] k >> adRatio "hov" [0.8] k

addC "hov" "sub_bed" [TP 0.5]

pitch "hov" [38, 41, 43] nv >> cf "hov" [1800] k >> vol "hov" [0.6] k >> durTS "hov" [2,1.7] rnd
addC "hov" "low_roll" $ euclid (5, 16) 0 32

-- Leads

pitch "sSaw" [57, 60, 57] nv >> addC "sSaw" "qa" (toTP [0, 4.5, 7])

pitch "sSaw" [50, 53, 57, 53] nv >> cPat "sixteenN" "sSaw"

-- Kit 1: 4-Floor Drive

sample "kit" (toPfs [1..17]) kitT 
addC "kit" "t_kit" $ euclid (13, 16) 2 8

-- Kit 2: Syncopated

sample "kit" (toPfs [1..17]) kitT 
addC "kit" "t_sync" $ euclid (9, 16) 0 32

-- =============================================================================

-- Bass Two-Note Roll

pitch "sub" [38, 45, 38] nv 
addC "sub" "roll" $ toTP [0, 0.75, 1]

-- Bass  Dub Line

pitch "sub" [38, 45, 48] nv 
addC "sub" "dub"  $ toTP [0, 1.5, 2]

-- 808 Sub Boom (Long)

pitch "sub" [38, 43] nv 
addC "sub" "boom" $ toTP [0, 4]

-- Stabs: Reggae Skank (configured)

pitch "sSaw" [50, 53, 57, 60] k 

addC "sSaw" "skank" $ toTP [1, 3]

-- Kit 1: Amen-ish Markov

sample "kit" (toPfs [1..17]) kitJ 
addC "kit" "amen" $ euclid (3, 15) 0 8

-- =============================================================================

-- Strings: Ambience & Pads

pitch "strings" [62, 65, 69] rnd >> vol "strings" [0.4] k >> rev "strings" [0.8] k
addC "strings" "ambience" $ toTP [0, 4]

-- Strings: Soft Arps

pitch "strings" (withScale 62 dorian) nv >> dur "strings" [0.2] k
addC "strings" "soft_arp" $ euclid (11, 32) 0 32

-- Bass 1: Two-Step Sub


pitch "303" [38, 36] nv 
addC "303" "2step" (toTP [0, 2])

-- Bass: Rolling 8th 

pitch "303" [38, 38, 41, 43, 41, 38, 38] nv >> cPat "eightN" "303"

-- Lead: Arp (Rising)

pitch "sSaw" [62, 65, 69, 72] nv >> cPat "qa" "sSaw"

-- Lead 2: 

pitch "sSaw" [57, 57, 55, 53] nv 
addC "sSaw" "vocal" $ toTP [0, 0.5, 2, 2.5]
 
sample "glitch" (toPfs [1..17]) kitI 
addC "glitch" "id" $ textToTP 8 "adhejf" 

prms "glitch" [(stuts, rnd, [1, 2, 8, 16]), (durTS, k, [0.0625])]  
addC "glitch" "micro" $ euclid (13, 32) 4 32 

pitch "303" [0..7] (withS (withScale 41 aeolian) (rMkvCSV csvAcid))

sample "kit" [1..17] (withK' mainKit (rMkvCSV csvKitJ))

sample "bongos [1..4] (withK' mainKit () 
