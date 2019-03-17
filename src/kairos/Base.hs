{-# LANGUAGE DeriveFunctor, FlexibleContexts #-}

module Kairos.Base where

import Control.Concurrent
import Control.Concurrent.STM
import qualified Data.Map.Strict as M
import Data.Typeable

-- the Performance is the scope of the composition
data Performance = P { orc :: Orchestra
                     , clock :: Clock
                     , timePs :: TVar (M.Map [Char] [TimePoint])
                     }

-- clock
data Clock = Clock { startAt :: Time
                   , timeSig :: TVar [TimeSignature] }

-- time signature
data TimeSignature = TS { beatInMsr :: Double
                        , bpm :: Double
                        , startTime :: Time } deriving (Show,Eq)

--  Performance seconds
type Time = Double

-- measureNumber.currPhase ( ex. 4.1 == measure 4 beat 2 )
type Beats = Double

-- Orchestra
type Orchestra = TVar (M.Map [Char] Instr)

-- Instrument
data Instr = I { insN :: Int
               , pf :: TVar PfMap
               , status :: Status
               , toPlay :: Maybe TimePoint
               , pats :: TVar (M.Map [Char] PfPat)
               , timeF :: String }

-- is the instrument Playing ?
data Status = Playing | Stopped | Stopping deriving (Show)

-- Map of Pfields
type PfMap = M.Map Int Pfield

-- pattern of pfields and related update function
data PfPat = PfPat { pfNum :: Int                           -- id of the pfield
                   , pat  :: TVar [Pfield]                  -- the string of possible values (or only value, depends on what the replacer needs)
                   , updater :: PfPat -> IO Pfield          -- the function that decides which value to take
                   }

-- a single Pfield
data Pfield  = Ps { pString :: String }
             | Pd { pDouble :: Double } deriving (Eq, Ord, Typeable)

instance Show Pfield where
  show (Ps s) = show s
  show (Pd d) = show d

-- a point in the Bar
data TimePointf a = TP { start :: a
                       } deriving (Eq, Ord, Show, Functor)

type TimePoint = TimePointf Beats
