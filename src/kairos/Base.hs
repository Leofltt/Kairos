{-# LANGUAGE DeriveFunctor, FlexibleContexts #-}

module Kairos.Base where

import Control.Concurrent.STM
import qualified Data.Map.Strict as M
import Data.Typeable

-- the Performance is the scope of the composition
data Performance = P { orc :: Orchestra
                     , clock :: Clock
                     , timePs :: TVar (M.Map [Char] [TimePoint]) -- a map of time patterns with their names
                     }

-- clock
data Clock = Clock { startAt :: Time
                   , timeSig :: TVar [TimeSignature]
                   }

-- time signature
data TimeSignature = TS { beatInMsr :: Double
                        , bpm :: Double
                        , startTime :: Time
                        } deriving (Show,Eq)

--  Performance seconds
type Time = Double

-- measureNumber.currPhase ( ex. 4.1 == measure 4 beat 2 )
type Beats = Double

-- Orchestra
type Orchestra = TVar (M.Map [Char] Instr)

-- Instrument
data Instr = I { insN :: InstrumentID 
               , pf :: TVar PfMap 
               , status :: Status
               , toPlay :: Maybe TimePoint
               , pats :: TVar (M.Map Int PfPat) -- Patterns of Parameters and their IDs
               , timeF :: String                -- Name of the time function to refer to
               , kind :: MessageTo
               , itype :: InstrType
               }

-- Instrument Name: an integer number
type InstrumentID = Int

-- is the instrument Active ?
data Status = Init | Active | Inactive | Stopping deriving (Show, Eq)

-- where are we sending the data
data MessageTo = Csound | OSC deriving (Show, Eq)

-- instrument or effect ?
data InstrType = Instrument | Effect deriving (Show, Eq)

-- Map of Pfields and their IDs
type PfMap = M.Map Int Pfield

-- pattern of pfields and related update function
data PfPat = PfPat { pfNum :: Int                  -- id of the pfield
                   , pat  :: TVar [Pfield]         -- the string of possible values (or only value, depends on what the updater needs)
                   , updater :: PfPat -> IO Pfield -- the function that decides which value to take
                   }

-- a single Pfield
data Pfield  = Ps { pString :: String }
             | Pd { pDouble :: Double } deriving (Eq, Ord, Typeable)

-- a point in time
data TimePointf a = TP { time :: a
                       } deriving (Eq, Ord, Functor, Show)

type TimePoint = TimePointf Beats
