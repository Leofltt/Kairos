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

-- -- the time point with max start and min end
-- sect :: TimePoint -> TimePoint -> TimePoint
-- sect (TP ioif endf) (TP iois ends) = TP (max ioif iois) (min endf ends)
--
-- -- the timespan given from the intersection of two IOIs
-- timeSpanInters :: TimePoint -> TimePoint -> Maybe TimePoint
-- timeSpanInters o@(TP s e) t@(TP s' e') | and [s < e, b == q, b == e] = Nothing
--                                  | and [s' < e', b == q, b == e'] = Nothing
--                                  | b <= q = Just (TP b q)
--                                  | otherwise = Nothing
--                                  where (TP b q) = sect o t
--
-- beatToTPBar :: Beats -> TimePoint
-- beatToTPBar b = (TP (thisBar b) (nextBar b))
--
-- barsInTP :: Integral a => TimePoint -> [a]
-- barsInTP (TP s e) | s > e = []
--                     | s == e = [floor s]
--                     | otherwise = [floor s .. ((ceiling e)-1)]
--
-- barsTPinTP :: TimePoint -> [TimePoint]
-- barsTPinTP = Prelude.map (beatToTPBar . realToFrac) . barsInTP
