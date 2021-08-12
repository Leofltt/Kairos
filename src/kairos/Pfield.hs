{-# LANGUAGE FlexibleInstances #-}

module Kairos.Pfield where 

import Data.Typeable ( Typeable )
import Control.Concurrent.STM ( TVar )
import qualified Data.Map.Strict as M

-- a single Pfield
data Pfield  = Ps { pString :: String }
             | Pd { pDouble :: Double } deriving (Eq, Ord, Typeable)

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

-- pattern of pfields and related update function
data PfPat = PfPat { pfNum :: Int                  -- id of the pfield
                   , pat  :: TVar [Pfield]         -- the string of possible values (or only value, depends on what the updater needs)
                   , updater :: PfPat -> IO Pfield -- the function that decides which value to take
                   }

-- Map of Pfields and their IDs
type PfMap = M.Map Int Pfield

pfToString :: [Pfield] -> String
pfToString ps = unwords $ map show ps

