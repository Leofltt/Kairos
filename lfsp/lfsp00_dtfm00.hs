

cT 147
addC "dtmf" "eu__" $ euclid (15,18) 3 8


dur "dtmf" [0.1] k 

adRatio "dtmf" [0.1, 0.7, 0.2] rnd

btn "dtmf" ["3", "5", "7", "#"] nv 

chorus "dtmf" [0.01, 0.3, 0.6, 0.8] rnd

rev "dtmf" [0.3, 0.6, 0.8, 0.1] rnd 

divchorus $ Pd 10

delchorus $ Pd 333 

p "dtmf"
