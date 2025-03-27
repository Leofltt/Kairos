addI "mc1" $ models csd1 1
addI "mc2" $ models csd1 2
addI "mc3" $ models csd1 3
addI "mc4" $ models csd1 4
addI "mc5" $ models csd1 5
addI "mc6" $ models csd1 6

cT 133



cPat "dbk" "mc1"

cPat "downB" "r707"

cPat "downB" "cp808d"

addC "mc2" "osh" =<< patternWithDensity 8 32 70



addC "lilShake" "shs" =<< patternWithDensity 8 16 34

addC "cp808d" "cp1" =<< patternWithDensity 8 16 34
