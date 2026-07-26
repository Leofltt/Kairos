{-# LANGUAGE FlexibleInstances #-}

module Kairos.Pfield where
import Data.Either ()
import Data.Typeable ( Typeable )
import qualified Data.Map.Strict as M

-- | pfield Id containing the pfield number and it's name
data PfId = Either Int String deriving (Eq, Show, Ord)

newPfId :: Int -> String -> PfId
newPfId = Either

idInt :: PfId -> Int
idInt (Either x _) = x

pfIdInt :: Int -> PfId
pfIdInt x = Either x ""

idString :: PfId -> String
idString (Either _ y) = y

pfIdString :: String -> PfId
pfIdString = Either (-1)

-- | a single Pfield
data Pfield  = Ps { pString :: String }
             | Pd { pDouble :: Double }
             | Pl { pList :: [Pfield] } deriving (Eq, Ord, Typeable)

instance Show Pfield where
  show (Ps s) = show s
  show (Pd d) = show d
  show (Pl l) = "[" ++ unwords (map show l) ++ "]"

class PfAble a where
    toPf :: a -> Pfield
    fromPf :: Pfield -> a

instance PfAble Double where
    toPf = Pd
    fromPf (Pd x) = x
    fromPf (Ps _) = error "pfield is a string, not a double"
    fromPf (Pl _) = error "pfield is a list, not a double"

instance PfAble Int where
    toPf = Pd . fromIntegral
    fromPf (Pd x) = round x
    fromPf (Ps _) = error "pfield is a string, not an int"
    fromPf (Pl _) = error "pfield is a list, not an int"

instance PfAble Integer where
    toPf = Pd . fromIntegral
    fromPf (Pd x) = round x
    fromPf (Ps _) = error "pfield is a string, not an integer"
    fromPf (Pl _) = error "pfield is a list, not an integer"

instance PfAble String where
    toPf = Ps
    fromPf (Ps x) = x
    fromPf (Pd x) = show x
    fromPf (Pl x) = show x

instance PfAble [Pfield] where
    toPf = Pl
    fromPf (Pl x) = x
    fromPf _ = error "pfield is not a list"

instance PfAble [Double] where
    toPf = Pl . map Pd
    fromPf (Pl x) = map fromPf x
    fromPf _ = error "pfield is not a list of doubles"

instance PfAble [Int] where
    toPf = Pl . map (Pd . fromIntegral)
    fromPf (Pl x) = map fromPf x
    fromPf _ = error "pfield is not a list of ints"

instance PfAble Pfield where
    toPf = id
    fromPf = id

toPfs :: PfAble a => [a] -> [Pfield]
toPfs = map toPf

fromPfsD :: [Pfield] -> [Double ]
fromPfsD = map fromPf

-- | Map of Pfields and their IDs
type PfMap = M.Map PfId Pfield

pfToString :: [Pfield] -> String
pfToString ps = unwords $ map show ps

