{-# LANGUAGE FlexibleInstances #-}

module Kairos.Pfield where

import Data.Typeable ( Typeable )
import Control.Concurrent.STM ( TVar )
import qualified Data.Map.Strict as M
import Data.Either ()

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

-- | pfield Id containing the pfield number and it's name
data PfId = Either Int String deriving (Eq, Show, Ord) 

idInt :: PfId -> Int 
idInt (Either x _) = x

idString :: PfId -> String 
idString (Either _ y) = y 

-- | pattern of pfields and related update function
data PfPat = PfPat { pfId :: PfId                  -- ^ id of the pfield
                   , pat  :: TVar [Pfield]         -- ^ the string of possible values (or only value, depends on what the updater needs)
                   , updater :: PfPat -> IO Pfield -- ^ the function that decides which value to take
                   }

-- | Map of Pfields and their IDs
type PfMap = M.Map Int Pfield

pfToString :: [Pfield] -> String
pfToString ps = unwords $ map show ps

