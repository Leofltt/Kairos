module Kairos.Clock where

import Control.Concurrent
import Control.Concurrent.STM
import Data.Time.Clock.POSIX
import Kairos.Base

data Clock = Clock { startAt :: Double, timeSig :: TVar [TimeSignature] } 

data TimeSignature = TS { beat :: Double, bpm :: Double, startTime :: Double } deriving (Show)

getNow :: IO Double
getNow = do x <- getPOSIXTime; return $ realToFrac x

timeD :: Clock -> IO Double
timeD clock = let
  s = startAt clock in
  do 
  x <- getNow
  return (x - s)

defaultClock :: IO Clock
defaultClock  = do
  s <- getNow
  let timesig = TS { bpm = 120
                   ,  beat = 4
                   ,  startTime = 0 -- this is actually the time delta from s to now, in Doubles 
                   }
  ts <-  newTVarIO $ [timesig]
  return $ Clock { startAt = s
                 , timeSig = ts 
                 }

-- the strt parameter represents after how many measures.currPhase you want the TS to start
newTS :: Double -> Double -> Double -> TimeSignature
newTS tmp msr strt = 
  TS { bpm = tmp
     , beat = msr
     , startTime = strt 
     } 

currentTempo :: Clock -> IO Double
currentTempo c = do
  cts <- currentTS c 
  return $ bpm $ cts

-- given a TS, corrects the time so that it will start after the previous one and then wirtes it to the TVar
addTS :: Clock -> TimeSignature -> IO [TimeSignature] 
addTS c t = do
  now <- timeD c
  ts <- readTVarIO $ timeSig c
  atomically $ writeTVar (timeSig c) ((newTS (bpm t) (beat t) ((later now (startTime t)) + (beatToTime (startTime t) (bpm t) (beat t)))):ts)
  tim <- readTVarIO $ timeSig c
  return $ tim       

later :: Double -> Double -> Double
later n s | s >= n = s
          | n > s = n

currentTS :: Clock -> IO TimeSignature
currentTS c = do
  now <- timeD c
  tms <- readTVarIO $ timeSig c
  return $ checkTimeSig now tms

checkTimeSig :: Double -> [TimeSignature] ->  TimeSignature
checkTimeSig now tms = head $ possible tms now

possible :: [TimeSignature] -> Double -> [TimeSignature] 
possible (t:ts) now  
  | ((startTime t) == (head $ filter (<= now) (starts (t:ts)))) = t : (possible ts now)
  | otherwise = possible ts now 
possible [] now = [] 
 
starts :: [TimeSignature] -> [Double]
starts (t:ts) = (startTime t):starts ts
starts [] = []

-- display the time in Measure.CurrPhase
currentBeat :: Clock -> IO Double
currentBeat c = do
  now <- timeD c  
  ts <- currentTS c
  return $ now * ((bpm ts)/ 60.00) / (beat ts)

-- given an amount of measure.currPhase, bpm and beatsPerMeasure, gives a Double back representing the length in s
beatToTime :: Double -> Double -> Double -> Double
beatToTime x bpm beatPerMeasure = (x * beatPerMeasure) * (60.00 / bpm)

