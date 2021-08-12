{-# LANGUAGE DeriveFunctor #-}

module Kairos.TimePoint where

import Kairos.Clock ( TimeSignature(beatInMsr), Beats )
import Kairos.Utilities
    ( genNRandomValues,
      intToDouble,
      numSeqFromText,
      numSeqFromBin )
import Kairos.Euclidean ( euclidean )
import Control.Applicative (liftA2)
import Data.Time.Clock.POSIX (getPOSIXTime)

-- a point in time
newtype TimePointf a = TP { whenTP :: a
                       } deriving (Eq, Ord, Functor, Show)

type TimePoint = TimePointf Beats

instance Applicative TimePointf where
  pure = TP
  TP f <*> x = fmap f x

instance Monad TimePointf where
  TP x >>= f = f x

instance (Num a) => Num (TimePointf a) where
  (+) = liftA2 (+)
  (*) = liftA2 (*)
  abs = fmap abs
  signum = fmap signum
  fromInteger = pure . fromInteger
  negate = fmap negate

wrapBar :: TimeSignature -> TimePoint -> TimePoint
wrapBar ts = fmap (doubleRem (beatInMsr ts))

doubleRem :: RealFrac a => a -> a -> a
doubleRem bar beat = beat - (bar * fromIntegral (floor (beat/bar)))

toTP :: [Double] -> [TimePoint]
toTP = Prelude.map pure

tpD :: Double -> TimePoint
tpD = pure

fromTP :: [TimePoint] -> [Double]
fromTP = map whenTP

-- functions to create TimePoint patterns -------------------------------

-- create tuples of t elements to fill up b number of beats
tupleForBar :: Double -> Double -> [TimePoint]
tupleForBar b t = toTP $ takeWhile (<b) $ Prelude.map (+(b/(t*b))) [(0/t*b), (1/t*b) ..]

-- Given total length in beats, the beat subdivision wanted and the % of beats, generate a time pattern
patternWithDensity :: Double -> Double -> Int  -> IO (Maybe [TimePoint])
patternWithDensity b sub dens = do
  seed <- round . (* 1000) <$> getPOSIXTime
  let bar = tupleForBar b sub
  let vals = genNRandomValues (length bar) seed ::[Int]
  let newBar = tempF bar vals dens
  return $ notEmpty newBar

-- return Nothing if list is Empty, otherwise Just list
notEmpty :: [a] -> Maybe [a]
notEmpty [] = Nothing
notEmpty l  = Just l

tempF :: [TimePoint] -> [Int] -> Int -> [TimePoint]
tempF [x] (v:vs) d | v <= d = [x]
                      | otherwise = []
tempF (x:xs) (v:vs) d | v <= d = x:tempF xs (vs++[v]) d
                      | otherwise = tempF xs (vs++[v]) d

-- Given a tuple, a rotation shift and a number of beats returns an euclidean rhythm TP
euclid :: (Int,Int) -> Int -> Double -> [TimePoint]
euclid (x,y) shift maxbeats = toTP $ Prelude.map (*(maxbeats/intToDouble y)) $ Prelude.map (+ (-1)) $ Prelude.filter (/=0) $ zipWith (*) (Prelude.map intToDouble $ euclidean (x,y) shift) [1,2..]




-- Given total length in beats, take a string of text and converts it into a time pattern
textToTP :: Double -> String -> [TimePoint]
textToTP maxbeats t = toTP $ Prelude.map (*maxbeats) $ numSeqFromText t

-- Given total length in beats, take a binary number and converts it into a time pattern
binToTP :: Double -> Double -> [TimePoint]
binToTP maxbeats b = toTP $ Prelude.map (*maxbeats) $ numSeqFromBin b

evolve ::  Int -> ([Double] -> [Double]) -> [TimePoint] -> [TimePoint]
evolve n rule xs  | n <= 0 = xs | otherwise = evolve (n-1) rule (toTP $ rule $ fromTP xs)

-- rules :: [Double] -> [Double]

interp1 :: Double -> [Double] -> [Double]
interp1 total [x] = x:[(x + total)/2]
interp1 total (x:y:xs) = x:((x+y)/2):interp1 total (y:xs)

interp2 :: Double -> [Double] -> [Double]
interp2 _ [x, y] = [(x+y)/2]
interp2 tot [x] = [(x + tot)/2]
interp2 tot (x:y:xs) = ((x+y)/2):interp2 tot xs

--- default Patterns ----------------------------------------

-- a few time patterns in 4/4
downB = [TP 1.0,TP 3.0]
dbk = toTP [0,2.5]
dbk2 = toTP [0, 1.75, 2.5]
upFour = toTP $ takeWhile (< 4) [0.5,1.5..]
fourFloor = toTP $ takeWhile (< 4) [0,1..]
eightN = toTP $ takeWhile (< 4) [0,0.5..]
sixteenN = toTP $ takeWhile (< 4) [0,0.25..]
dubb = toTP [2.25,2.75]
jGhost = toTP [1.75,2.25,5.75]
jGhost1 = toTP [1.75,2.25,5.75,6.25,7.75]
sixBar = tupleForBar 4 6
jgk = toTP [0, 0.5, 2.5, 4.5,4.75,6.5]
jgs = toTP [1,1.75,2.25,3.5,4.25,5,5.75,6.25,7.5]
ukgrs = toTP [0.25,1.75,3.25,5.75,7.25]
ukgch = toTP [0.5,0.75, 1.5, 2.5,3.5,3.75,4.5,5.5,6.5,7.5]
bouncyk = toTP [0,0.5,2,2.5,4,4.25,6,6.5]
ir1k = toTP [0,0.5,0.75,1.5,2.5,4,4.5,5.75,6.5]
stdbkk = toTP [0,0.5,1.5,2.5]
stdbks = toTP [1,1.75,2.5,3]
irsn = toTP [1,1.75,3,3.75,5,7,7.75]
uno = [TP 0.0]
kpanb = toTP [0, 0.75,1.5,2.5,3]
kpanc = toTP [0,0.25,1.5,1.75,2,2.25,3.5,3.75,4,4.5,4.75,5.25,5.5,6,7.5]
kpanbox = toTP [0,1.5,2,3.5,4,4.5,4.75,5.25,5.5,6,7.5]
b2 = toTP [0,0.75,1,2,2.5]
bou2 = toTP [0,1.5,2,3.5,4,5.5,6,7.75]
fwk1 = toTP [0,0.75,1.5,2,2.75,3.5]
fwk2 = toTP [0,0.75,1.25,1.75,2,2.75]
adk = toTP [0, 1, 2,2.5,3,4,5,5.5,6,7,8,8.5,9,10,11,11.5]
adb = toTP [0,1,2,3,4.5,5,6,7,8,9.5,10]
