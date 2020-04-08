module Kairos.TimePoint where

import Kairos.Base
import Kairos.Utilities
import Data.Map.Strict as M
import Data.Maybe
import Control.Concurrent.STM
import Control.Applicative (liftA2)
import Data.Time.Clock.POSIX (getPOSIXTime)

instance Applicative TimePointf where
  pure t = TP t
  (<*>)(TP f)(TP second) = TP (f second)

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

tpD :: Double -> TimePoint
tpD = pure

getTimePoint :: Performance -> String -> IO [TimePoint]
getTimePoint perf s = do
  Just t <- lookupMap (timePs perf) s
  return $ t

fromTP :: [TimePoint] -> [Double]
fromTP (x:xs) = (start x):(fromTP xs)
fromTP [] = []

-- functions to create TimePoint patterns -------------------------------

tupleForBar b t = toTP $ takeWhile (<b) $ Prelude.map (+(b/(t*b))) [(0/t*b), (1/t*b) ..]


patternWithDensity :: Double -> Double -> Int  -> IO [TimePoint]
patternWithDensity b sub dens = do
  seed <- (round . (* 1000)) <$> getPOSIXTime
  let bar = tupleForBar b sub
  let vals = genNRandomValues (length bar) seed ::[Int]
  let newBar = tempF bar vals dens
  return $ newBar

tempF :: [TimePoint] -> [Int] -> Int -> [TimePoint]
tempF (x:[]) (v:vs) d | v <= d = [x]
                      | otherwise = []

tempF (x:xs) (v:vs) d | v <= d = x:(tempF xs (vs++[v]) d)
                      | otherwise = tempF xs (vs++[v]) d


textToTP :: Double -> String -> [TimePoint]
textToTP maxbeats t = toTP $ Prelude.map (*maxbeats) $ numSeqFromText t

binToTP :: Double -> Double -> [TimePoint]
binToTP maxbeats b = toTP $ Prelude.map (*maxbeats) $ numSeqFromBin b

evolve ::  Int -> ([Double] -> [Double]) -> [TimePoint] -> [TimePoint]
evolve n rule xs  | n <= 0 = xs | otherwise = evolve (n-1) rule (toTP $ rule $ fromTP xs)

-- rules :: [Double] -> [Double]

interp1 :: Double -> [Double] -> [Double]
interp1 total (x:[]) = (x:[((x + total)/2)])
interp1 total (x:y:xs) = x:((x+y)/2):(interp1 total (y:xs))

interp2 :: Double -> [Double] -> [Double]
interp2 tot (x:y:[]) = [((x+y)/2)]
interp2 tot (x:[]) = [(x + tot)/2]
interp2 tot (x:y:xs) = ((x+y)/2):(interp2 tot xs)
