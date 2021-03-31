
addT $ newTS 100 3 0


let scale = aeolian

-- F aeolian

let bassScale = withScale 41 scale
let harmScale = offset 12 bassScale

let tqrize = filter (< (TP 3))
let fourFl2 = tqrize fourFloor
let upF2 = tqrize upFour

addC "ks808" "ff2" fourFl2

addC "hh808sh" "upf3" =<< patternWithDensity 6 12 77
addC "ohsE" "oh" upF2
vol "ohsE" [Pd 0.7] keep
p "ohsE"

m_vol 0.85

durTS "lpFM" [Pd (1/8)] keep
pitch "lpFM" (toPfD [53, 58,60,53, 51, 58, 53, 60]) $ percentNext 88

addC "lpFM" "lpfm1" =<< patternWithDensity 6 12 68

pitch "303" (toPfD bassScale) $ runMarkov csv3
