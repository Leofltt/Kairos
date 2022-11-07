addI "mod1" $ models "11000" 1

addI "modk" $ models "11000" 6
addI "modc" $ models "11000" 2


addC "mod1" "t" $ shine 4 "~~~~*~~~~~~~~~*~~"
addC "modc" "c" $ shine 4 "~*~~"

pat = jgk ++ ( map (+ TP 4) ukgrs) 
addC "modk" "pat"  pat

p "modk" 

cT 133

pitch "mod1" (toPfs [48, 51, 53, 51, 47, 48, 51]) a 
