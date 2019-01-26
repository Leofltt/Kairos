module Kairos.Clock where

import Control.Concurrent
import Control.Concurrent.STM
import Data.Time.Clock.POSIX
import Kairos.Base

data Clock = Clock { startAt :: Double, timeSig :: TVar [TimeSignature] } 

data TimeSignature = TS { beat :: Double, bpm :: Double, startBeat :: Double } deriving (Show)

getNow :: IO Double
getNow = do x <- getPOSIXTime; return $ realToFrac x

timeElapsed :: Clock -> IO Double
timeElapsed clock = let
  s = startAt clock in
  do 
  x <- getNow
  return (x - s)

defaultClock :: IO Clock
defaultClock  = do
  s <- getNow
  let timesig = TS { bpm = 120
                   ,  beat = 4
                   ,  startBeat = 0 
                   }
  ts <-  newTVarIO $ [timesig]
  return $ Clock { startAt = s
                 , timeSig = ts 
                 }

newTS :: Double -> Double -> Double -> TimeSignature
newTS tmp msr strt = 
  TS { bpm = tmp
     , beat = msr
     , startBeat = strt
     } 

lastTS :: Clock -> IO TimeSignature
lastTS c = do
   tms <-  readTVarIO $ timeSig c 
   return $ head tms

-- this is wrong at the moment, need to consider the current beat
currentTempo :: Clock -> IO Double
currentTempo c = do
  cts <- lastTS c 
  return $ bpm $ cts

addTS :: Clock -> TimeSignature -> IO [TimeSignature] 
addTS c t = do 
  ts <-  readTVarIO $ timeSig c
  atomically $ writeTVar (timeSig c) (t:ts)
  tim <- readTVarIO $ timeSig c
  return $ tim       
 
--currentTime :: Clock -> IO Time

-- currentBeat :: Clock -> Double
-- get last timesig change starting bar
-- check for elapsed time since then
-- add to Time

