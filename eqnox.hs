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

-- =============================================================================
-- PERFORMANCE SETUP (143 BPM / D Dorian)
-- =============================================================================

-- Main performance kit
-- 1-3: Kicks, 4-7: Snares/Claps, 8-11: Hats, 12-14: Percs, 15-17: Cymbals/FX

let mainKit = newKit [ (1, Ps "er1k3"), (2, Ps "er1k2"), (3, Ps "K909"), (4, Ps "sj1"), (5, Ps "cp808d"), (6, Ps "er1r"), (7, Ps "glsn"), (8, Ps "chxtan"), (9, Ps "er1mhh"), (10, Ps "scs"), (11, Ps "ohLe"), (12, Ps "bongo3"), (13, Ps "bongo4"), (14, Ps "bongo5"), (15, Ps "ri707"), (16, Ps "er1cr1"), (17, Ps "hit4") ]

-- Instantiate missing instruments

addI "sub" $ fmSub
addI "stabs" $ superSaw
addI "kit" $ sampler ""
addI "glitch" $ stutter ""

-- Scale Definition (D Dorian)
-- D3 = 50, D2 = 38

let dDorian = withScale 50 dorian
let d2Dorian = withScale 38 dorian

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

-- Leads

pitch "sSaw" [57, 60, 57] nv >> addC "sSaw" "qa" [0, 4.5, 7]

pitch "sSaw" [50, 53, 57, 53] nv >> cPat "sixteenN" "sSaw"

-- Kit 1: 4-Floor Drive

sample "kit" (toPfs [1..17]) kitT 
addC "kit" "t_kit" $ toTP [0..3]

-- Kit 2: Syncopated Techno

sample "kit" (toPfs [1..17]) kitT 
addC "kit" "t_sync" $ euclid (9, 16) 0 32

-- =============================================================================

-- Bass 1: Two-Note Roll (configured)
pitch "sub" [38, 45, 38] nv >> addC "sub" "roll" [0, 0.75, 1]

-- Bass 2: The Dub Line
pitch "sub" [38, 45, 48] nv >> addC "sub" "dub" [0, 1.5, 2]

-- Bass 3: 808 Sub Boom (Long)
pitch "sub" [38, 43] nv >> addC "sub" "boom" [0, 4]

-- Stabs: Reggae Skank (configured)
pitch "stabs" [50, 53, 57, 60] k >> addC "stabs" "skank" [1, 3]

-- Kit 1: Amen-ish Markov
sample "kit" (toPfs [1..17]) kitJ >> addC "kit" "amen" $ textToTP 16 "x-x--x-x-x--x-x-"

-- =============================================================================
-- SECTION 3: UKG / IDM VARIATIONS
-- =============================================================================

-- Bass 1: Two-Step Sub
pitch "sub" [38, 36] nv >> addC "sub" "2step" [0, 2]

-- Bass 2: Rolling 8th Note
pitch "sub" [38, 38, 41, 43, 41, 38, 38] nv >> cPat "eightN" "sub"

-- Lead 1: Euphoric Arp (Rising 16ths)
pitch "sSaw" [62, 65, 69, 72] nv >> cPat "sixteenN" "sSaw"

-- Lead 2: Chopped Vocal Hook
pitch "sSaw" [57, 57, 55, 53] nv >> addC "sSaw" "vocal" [0, 0.5, 2, 2.5]

-- Kit 1: Glitch / IDM Markov

sample "glitch" (toPfs [1..17]) kitI 
addC "glitch" "idm" $ textToTP 8 "adhejf" 


-- Kit 2: Micro Glitch
prms "glitch" [(stuts, rnd, [1, 2, 8, 16]), (durTS, k, [0.0625])] >> addC "glitch" "micro" $ euclid (13, 32) 4 32

