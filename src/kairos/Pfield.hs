{-# LANGUAGE FlexibleInstances #-}

module Kairos.Pfield where
import Data.Either ()
import Data.Typeable ( Typeable )
import qualified Data.Map.Strict as M

-- | pfield Id containing the pfield number and it's name
data PfId = Either Int String deriving (Eq, Show, Ord) 

new_pfId :: Int -> String -> PfId
new_pfId x y = Either x y

idInt :: PfId -> Int 
idInt (Either x _) = x

pfIdInt :: Int -> PfId
pfIdInt x = Either x "" 

idString :: PfId -> String 
idString (Either _ y) = y 

pfIdString :: String -> PfId
pfIdString y = Either (-1) y

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
type PfMap = M.Map PfId Pfield

pfToString :: [Pfield] -> String
pfToString ps = unwords $ map show ps

