{-# LANGUAGE FlexibleInstances #-}

module Kairos.Pfield where

import Data.Typeable ( Typeable )
import qualified Data.Map.Strict as M

-- | a single Pfield
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
    fromPf (Ps _) = error "pfield is a string, not a double"

instance PfAble String where
    toPf = Ps
    fromPf (Ps x) = x
    fromPf (Pd x) = show x

toPfs :: PfAble a => [a] -> [Pfield]
toPfs = map toPf

fromPfsD :: [Pfield] -> [Double ]
fromPfsD = map fromPf 

-- | Map of Pfields and their IDs
type PfMap = M.Map Int Pfield

pfToString :: [Pfield] -> String
pfToString ps = unwords $ map show ps

