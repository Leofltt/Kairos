import Kairos.Utilities

type Scale = [Double]

withScale :: Double -> Scale -> Scale
root `withScale` scale = transpose root scale


ionian :: Scale
ionian = [0,2,4,5,7,9,11]
dorian :: Scale
dorian = [0,2,3,5,7,9,10]
phrygian :: Scale
phrygian = [0,1,3,5,7,8,10]
lydian :: Scale
lydian = [0,2,4,6,7,9,11]
aeolian :: Scale
aeolian =  [0,2,3,5,7,8,10]
mixo :: Scale
mixo = [0,2,4,5,7,9,10]
locrian :: Scale
locrian =  [0,1,3,5,7,8,10]
blues :: Scale
blues = [1,3,5,6,7,10]
flamenco :: Scale
flamenco = [0,1,5,6,8,10,11]
