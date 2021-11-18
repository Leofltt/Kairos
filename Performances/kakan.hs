-- full set 2 mins
-- 


cT 113

-- first min

-- hov, gradual intro of hihat and kick (from m:c ?)
 

-- Colours are black and white.

params "hov" [ (keep,durTS,toPfs[ 2]),(keep, vol, [Pd 0.5]),(keep, cf, [Pd 900]),((runMarkov csv3), pitch, toPfs (withScale 39 phrygian)),(keep, rev, [Pd 0.8])  ]


addC "hov" "intro" =<< patternWithDensity 8 16 30

vol "hov" [Pd 0.6] keep

cf "hov" [Pd 1500] keep
pitch "hov" (toPfs (withScale 36 harm)) $ runMarkov csv3 

-- second min



-- lighter "sparkly" synth (m:c chord machine ?)

-- sparse hihat (trappy) , swingy kick (hiphop feel?)


-- Colours will change to colourful.
-- I want to imagine bright colours, like a pleasure when I found a beautiful flower.
-- End quietly.

addI "mcstab" $ modelcycles 3
addI "mck" $ modelcycles 1
addI "mcr" $ modelcycles 6



--- phrygian to mixo 

--- root note = 39 or 51 ()
