module Kairos.Clock where 

import Kairos.Instrument
import Control.Concurrent
import Control.Concurrent.STM
import Data.Time.Clock.POSIX
import Control.Monad.IO.Class

data Clock = Clock { startAt :: Time, timeSig :: TVar [TimeSignature] } 

data TimeSignature = TS { beat :: Double, bpm :: Double, startTime :: Time } deriving (Show,Eq)

-- seconds 
type Time = Double

-- measureNumber.currPhase ( ex. 4.0 == measure 4 beat 1 ) 
type Beats = Double

data Transport = T { clock :: Clock
                   ,  orc :: Orchestra
                   ,  curBeat :: Clock -> Beats
                   } 

getNow :: IO Time
getNow = liftIO $ fmap realToFrac getPOSIXTime

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
newTS :: Double -> Double -> Beats -> TimeSignature
newTS tmp msr strt = 
  TS { bpm = tmp
     , beat = msr
     , startTime = strt 
     } 

currentTempo :: Clock -> IO Double
currentTempo c = do
  cts <- currentTS c 
  return $ bpm $ cts

-- given a clock and a TS, prepends the TS to the list of current ts in the clock, correcting the start time appropriately
addTS :: Clock -> TimeSignature -> IO [TimeSignature] 
addTS c t = do
  now <- timeD c
  ts <- readTVarIO $ timeSig c
  curBeat <- currentBeat c
  curTS <- currentTS c
  beatCurTs <- beatAt c (startTime curTS)
  atomically $ writeTVar (timeSig c) ((newTS (bpm t) (beat t) ((max ((beatToTime ((thisBar curBeat) - beatCurTs) (bpm curTS) (beat curTS)) + (startTime curTS)) (startTime $ head ts)) + (beatToTime (startTime t) (bpm t) (beat t)))):ts)
  tim <- readTVarIO $ timeSig c
  return $ tim       

currentTS :: Clock -> IO TimeSignature
currentTS c = do
  now <- timeD c
  tms <- atomically $ readTVar $ timeSig c
  return $ checkTimeSig now tms

checkTimeSig :: Time -> [TimeSignature] ->  TimeSignature
checkTimeSig now tms = head $ possible tms now

possible :: [TimeSignature] -> Time -> [TimeSignature] 
possible (t:ts) now  
  | ((startTime t) == (head $ filter (<= now) (starts (t:ts)))) = t : (possible ts now)
  | otherwise = possible ts now 
possible [] now = [] 
 
starts :: [TimeSignature] -> [Time]
starts (t:ts) = (startTime t):starts ts
starts [] = []

-- display the time in Measure.CurrPhase
currentBeat :: Clock -> IO Beats
currentBeat c = do
  now <- timeD c  
  result <- beatAt c now
  return $ result

beatAt :: Clock -> Time -> IO Beats
beatAt c time = do
  tms <- atomically $ readTVar $ timeSig c
  return $ timeDelta (possible tms time) (time:(starts (possible tms time)))

-- return the bar number from Beats
thisBar :: Beats -> Beats
thisBar = fromIntegral . (floor :: Beats ->  Int) 

nextBar :: Beats -> Beats
nextBar = (+ 1) . thisBar
-- given an amount of measure.currPhase, bpm and beatsPerMeasure, gives a Double back representing the length in s
beatToTime :: Beats -> Double -> Double -> Double
beatToTime x bpm beatPerMeasure = (x * beatPerMeasure) * (60.00 / bpm) 

-- given a time delta and a TS, return the amount of beats in that timesignature
timeToBeat :: Time -> TimeSignature -> Beats
timeToBeat delta ts = delta * ((bpm ts)/ 60.00) / (beat ts)

timeDelta :: [TimeSignature] -> [Double] -> Double
timeDelta (x:xs) (now:sts) = (timeToBeat (now  - (head sts)) x) + (timeDelta xs sts) 
timeDelta [] _ = 0

-- return the current beat in a bar
deltaBar :: Clock -> IO Double
deltaBar c = do
  cb <- currentBeat c
  ts <- currentTS c
  return $ (cb - (thisBar cb)) * (beat ts)

-- return the current phase (current beat in the bar 0 - 1) in the bar where the beast happens
deltaBeats :: Beats -> Beats
deltaBeats b = b - (thisBar b)
