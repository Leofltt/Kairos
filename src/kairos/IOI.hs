{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}

module Kairos.IOI where

import Kairos.Clock
import Kairos.Instrument
import Data.Map.Strict as M
import Control.Applicative (liftA2)

data IOIf a = IOI { st :: a
                  , end :: a
                  } deriving (Eq, Ord, Functor, Show)

type IOI = IOIf Beats

instance Applicative IOIf where
  pure t = IOI t t
  (<*>)(IOI so eo)(IOI st et) = IOI (so st) (eo et) 

instance (Num a) => Num (IOIf a) where
  (+) = liftA2 (+)
  (*) = liftA2 (*)
  abs = fmap abs
  signum = fmap signum
  fromInteger = pure . fromInteger
  negate = fmap negate

-- the IOI with max start and min end
sect :: IOI -> IOI -> IOI
sect (IOI so eo) (IOI st et) = IOI (max so st) (min eo et)

-- the timespan given from the intersection of two IOIs
timeSpanIOI :: IOI -> IOI -> Maybe IOI
timeSpanIOI o@(IOI s e) t@(IOI i f) | and [s < e, b == q, b == e] = Nothing
                                 | and [i < f, b == q, b == f] = Nothing
                                 | b <= q = Just (IOI b q) 
                                 | otherwise = Nothing
                                 where (IOI b q) = sect o t

timeToIOIBar :: Beats -> IOI
timeToIOIBar b = (IOI (thisBar b) (nextBar b))

barsInIOI :: Integral a => IOI -> [a]
barsInIOI (IOI s e) | s > e = []
                    | s == e = [floor s]
                    | otherwise = [floor s .. ((ceiling e)-1)]  

barsIOIinIOI :: IOI -> [IOI]
barsIOIinIOI = Prelude.map (timeToIOIBar . realToFrac) . barsInIOI

data Eventf t a = Event { wholE :: t
                        , partE :: t
                        , action :: a
                        } deriving (Functor, Ord, Eq, Show)  
 
 

