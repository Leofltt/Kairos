{-# OPTIONS_GHC -Wno-type-defaults #-}

module Kairos.Kit where

import Data.Map.Strict qualified as M
import Kairos.Pfield (Pfield)

type KitF a b = M.Map a b

type Kit = KitF Integer Pfield

fromKit :: (Ord a, Show a) => KitF a b -> a -> b
fromKit kit name = case M.lookup name kit of
  Just pf -> pf
  Nothing -> error $ "fromKit: no such field: " ++ show name

newKit :: (Ord a, Show a) => [(a, b)] -> KitF a b
newKit = M.fromList

-- | convert a list of keys to a list of values given a kit
fk :: Kit -> [Integer] -> [Pfield]
fk k = map $ fromKit k

showKit :: Kit -> IO ()
showKit kit = putStrLn $ unlines $ map formatEntry $ M.toList kit
  where
    formatEntry (k, v) = show k ++ " - " ++ show v
