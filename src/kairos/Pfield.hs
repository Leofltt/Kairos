{-# LANGUAGE FlexibleInstances #-}

module Kairos.Pfield where 

import Kairos.Base


instance Show Pfield where
  show (Ps s) = show s
  show (Pd d) = show d

class PfAble a where 
    toPf :: a -> Pfield 
    fromPf :: Pfield -> a 

instance PfAble Double where 
    toPf = Pd 
    fromPf (Pd x) = x 

instance PfAble String where 
    toPf = Ps 
    fromPf (Ps x) = x 

toPfs :: PfAble a => [a] -> [Pfield]
toPfs x = map toPf x