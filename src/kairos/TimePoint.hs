module Kairos.TimePoint where

import Kairos.Base
import Kairos.Utilities
import Data.Map.Strict as M
import Data.Maybe
import Control.Concurrent.STM
import Control.Applicative (liftA2)


instance Applicative TimePointf where
  pure t = TP t
  (<*>)(TP first)(TP second) = TP (first + second)

instance (Num a) => Num (TimePointf a) where
  (+) = liftA2 (+)
  (*) = liftA2 (*)
  abs = fmap abs
  signum = fmap signum
  fromInteger = pure . fromInteger
  negate = fmap negate

wrapBar :: TimeSignature -> TimePoint -> TimePoint
wrapBar ts tp = fmap (doubleRem (beatInMsr ts)) tp

doubleRem bar beat = beat - (bar * (fromIntegral $ floor (beat/bar)))

toTP :: [Double] -> [TimePoint]
toTP times = Prelude.map pure times

getTimePoint :: Performance -> String -> IO [Double]
getTimePoint perf s = do
  Just t <- lookupMap (timePs perf) s
  return $ fromTP  t

fromTP :: [TimePoint] -> [Double]
fromTP (x:xs) = (start x):(fromTP xs)
fromTP [] = []

evolve ::  Int -> ([Double] -> [Double]) -> [Double] -> [Double]
evolve n rule xs  | n <= 0 = xs | otherwise = evolve (n-1) rule (rule xs)

-- rules :: [Double] -> [Double]

interp1 :: Double -> [Double] -> [Double]
interp1 total (x:[]) = (x:[((x + total)/2)])
interp1 total (x:y:xs) = x:((x+y)/2):(interp1 total (y:xs))

interp2 :: Double -> [Double] -> [Double]
interp2 tot (x:y:[]) = [((x+y)/2)]
interp2 tot (x:[]) = [(x + tot)/2]
interp2 tot (x:y:xs) = ((x+y)/2):(interp2 tot xs)
