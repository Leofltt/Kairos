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

data Eventf t a = Event { startE :: t
                        , endE :: t
                        , action :: a
                        } deriving (Functor, Ord, Eq, Show)  

type Event a = Eventf (IOIf Double) a
 
--toDur :: Clock -> IOI -> M.Map Int Pfields -> M.Map Int Pfields
 

