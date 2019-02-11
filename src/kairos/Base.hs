module Kairos.Base where

import Control.Concurrent
import Control.Concurrent.STM
import qualified Data.Map.Strict as M
import Data.Typeable


-- clock
data Clock = Clock { startAt :: Time, timeSig :: TVar [TimeSignature] }

-- time signature
data TimeSignature = TS { beatInMsr :: Double, bpm :: Double, startTime :: Time } deriving (Show,Eq)

--  Performance seconds
type Time = Double

-- measureNumber.currPhase ( ex. 4.0 == measure 4 beat 1 )
type Beats = Double

-- Orchestra
type Orchestra = TVar (M.Map [Char] Instr)

-- Instrument
data Instr = I { insN :: Int, pf :: TVar PfMap, status :: Status, toPlay :: Maybe Beats }

-- is the instrument Playing ?
data Status = Playing | Stopped | Paused

-- Map of Pfields
type PfMap = M.Map Int Pfield

-- a single Pfield
data Pfield  = Ps { pString :: String } | Pd { pDouble :: Double } deriving (Eq, Ord, Typeable)

instance Show Pfield where
  show (Ps s) = show s
  show (Pd d) = show d
