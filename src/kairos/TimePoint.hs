module Kairos.TimePoint where

import Kairos.Base
import Data.Map.Strict as M
import Control.Applicative (liftA2)


instance Applicative TimePointf where
  pure t = TP t
  (<*>)(TP first)(TP second) = TP (first second)

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
