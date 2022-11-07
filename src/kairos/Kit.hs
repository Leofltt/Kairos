module Kairos.Kit where
    
import Kairos.Pfield ( Pfield )
import qualified Data.Map.Strict as M

type KitF a b = M.Map a b

type Kit = KitF String Pfield

fromKit :: (Ord a, Show a) => KitF a b -> a -> b
fromKit kit name = case M.lookup name kit of
  Just pf -> pf
  Nothing -> error $ "fromKit: no such field: " ++ show name

newKit :: (Ord a, Show a) => [(a, b)] -> KitF a b
newKit = M.fromList


-- | convert a list of keys to a list of values given a kit
fk :: (Ord a, Show a) => KitF a b -> [a] -> [b]
fk k = map $ fromKit k
