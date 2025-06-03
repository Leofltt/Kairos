{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Kairos.Parser.Rhythm.Spark where

import Kairos.Parser.Rhythm.L (LList (E, L))

type Spark = Char

sparkToDouble :: Spark -> Double
sparkToDouble '*' = 1
sparkToDouble '~' = 0
sparkToDouble _ = error "sparkToDouble: invalid spark"

lLfromSps :: [Spark] -> [LList Spark]
lLfromSps [] = []
lLfromSps (x : xs)
  | (&&) (x /= '[') (x /= ']') = E x : lLfromSps xs
  | x == '[' = L (mkSubList xs) : lLfromSps (dropWhile (/= ']') xs)
  | x == ']' = lLfromSps xs

mkSubList :: [Spark] -> [Spark]
mkSubList = takeWhile (/= ']')