



rev "strPad" [Pd 0.8] k

cfrev (Pd 8000)

pitch "strPad" (toPfs [44, 42, 44, 44, 43]) a

durTS "strPad" (toPfs [2, 1.33, 1.75]) rnd


vol "strPad" (toPfs [0.8, 0.5, 0.6]) rnd

addC "strPad" "p" $ shine 8 "~~*~~~~~*~"


vol "CH808" [Pd 0.4] k

addC "CH808" "aa" $ shine 8 "~**~*~*~~**~~*~~*~**~~*"

p "CH808"


cPat "uno" "kc1"

p "kc1"

cPat "bouncyk" "kCns"
p "kCns"


maybeAddC "scs" "s" =<< patternWithDensity 8 16 22

p "scs"

cPat "upFour" "ohsE"
p "ohsE"


maybeAddC "tambo626" "tt" =<< patternWithDensity 8 16 33

p "tambo626"


cPat "fourFloor" "kbaSh" 
p "kbaSh"
