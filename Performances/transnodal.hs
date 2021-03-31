addC "lpFM" "hi" $ textToTP 16 "hi"
p1 <- getTP "p1"
addC "st1" "p2" $ evolve 1 (interp1 8) p1










-- HI EVERYONE!!
--

-- for today I wanted to try livecode my model:cycles
-- to add some knob twiddling into the mix
--
-- hope you'll enjoy!!



--- sorry about the connection
--
--not sure what's happening


-- sorry about that :(
--
--
--
-- v annoying but that's what LIVECODE is all about right??
--
-- Technical issues and having fun w/ friends




cT 127
m_vol 0.85
let scaleBass = withScale 40 harmMin
let scaleMelody = offset 12 scaleBass

-- instruments kcs, chcs, shatcs, shakecs, scs, 303, cp808, hov

durTS "303" [Pd (1/4)] keep
pitch "303" (toPfD [45,40,40]) $ percentNext 70
cPat "sixteenN" "303"
cf "303" [Pd 6000] keep
wf02 "303" [Pd 10] keep
params "hov" [ (keep,durTS,toPfD[ 1]),(keep, vol, [Pd 0.5]),(keep, cf, [Pd 900]),((runMarkov csv3), pitch, toPfD (scaleBass)),(keep, rev, [Pd 0.8])  ]


-- model:cycles chord, kick, ride, at, dbb


addI "mck" $ modelcycles 1
addI "mcstab" $ modelcycles 3
addI "mca" $ modelcycles 4
addI "mcdbb" $ modelcycles 5


pitch "mcdbb" (toPfD (scaleMelody)) $ runMarkov csv3 
addI "mcr" $ modelcycles 6
pitch "mck" [Pd 52] keep
pitch "mcstab" (toPfD (scaleMelody)) $ runMarkov csv3 
pitch "mca" (toPfD [57,52,52]) $ percentNext 70
pitch "mcr" [Pd 52] keep

-- breaks back, ricochet

divs "back" [Pd 16] keep
durTS "back" [Pd (1/16)] keep

pick "back" (toPfD [0,1..15]) $ percentNext 66

cPat "sixteenN" "back" 

stuts "back" (toPfD ((take 8 [1,1..])++[2])) keep
