module Kairos.Clock where

import Control.Monad ( when )
import Control.Concurrent ( threadDelay )
import Control.Concurrent.STM
    ( atomically, newTVarIO, readTVarIO, writeTVar, TVar )
import Data.Time.Clock.POSIX ( getPOSIXTime )
import Control.Monad.IO.Class ( MonadIO(..) )

-- | clock
data Clock = Clock { startAt :: Time
                   , timeSig :: TVar [TimeSignature]
                   }

-- | time signature
data TimeSignature = TS { beatInMsr :: Double
                        , bpm :: Double
                        , startTime :: Time
                        } deriving (Show,Eq)

-- | Performance seconds
type Time = Double

-- | measureNumber.currPhase ( ex. 4.1 == measure 4 beat 2 )
type Beats = Double

displayClock :: Clock -> IO [Char]
displayClock c = do
  ts   <- currentTS c
  cb   <- currentBeat c
  beat <- beatInBar c
  return $ "clock's bar: " ++ show (thisBar cb) ++ ", beat: " ++ take 4 (show beat) ++ ", at tempo: " ++ show (bpm ts) ++" bpm."

getNow :: MonadIO m => m Time
getNow = liftIO $ fmap realToFrac getPOSIXTime

timeD :: MonadIO m => Clock -> m Time
timeD clock = let
  s = startAt clock in
  do
  x <- getNow
  return (x - s)

waitUntil :: MonadIO m => Clock -> Time -> m ()
waitUntil c t = waitT . (t -) =<< timeD c

waitT :: (MonadIO m, RealFrac a) => a -> m ()
waitT t = when (t > 0) (liftIO (threadDelay(floor (t * 1000000))))

defaultClock :: IO Clock
defaultClock  = do
  s <- getNow
  let timesig = TS { bpm = 120
                   ,  beatInMsr = 4
                   ,  startTime = 0 -- this is actually the time delta from s to now, in Doubles
                   }
  ts <-  newTVarIO [timesig]
  return $ Clock { startAt = s
                 , timeSig = ts
                 }

-- | new time signature: the strt parameter represents after how many measures.currPhase you want the TS to start
newTS :: Double -> Double -> Beats -> TimeSignature
newTS tmp msr strt =
  TS { bpm = tmp
     , beatInMsr = msr
     , startTime = strt
     }

currentTempo :: Clock -> IO Double
currentTempo c = do
  cts <- currentTS c
  return $ bpm cts

changeTempo :: Clock -> Double -> IO ()
changeTempo c t = do
  cts <- currentTS c
  tss <- addTS c $ newTS t (beatInMsr cts) 1 -- 1: tempo is changed on the next bar
  putStrLn $ "Current bpm: " ++ show (bpm $ head tss)

-- | given a clock and a TS, prepends the TS to the list of current ts in the clock, correcting the start time appropriately
addTS :: Clock -> TimeSignature -> IO [TimeSignature]
addTS c t = do
  ts <- readTVarIO $ timeSig c
  curBeat <- currentBeat c
  curTS <- currentTS c
  beatCurTs <- beatAt c (startTime curTS)
  atomically $ writeTVar (timeSig c) (newTS (bpm t) (beatInMsr t) (max (beatToTime (thisBar curBeat - beatCurTs) (bpm curTS) (beatInMsr curTS) + startTime curTS) (startTime $ head ts) + beatToTime (startTime t) (bpm t) (beatInMsr t)):ts)
  readTVarIO $ timeSig c

currentTS :: Clock -> IO TimeSignature
currentTS c = do
  now <- timeD c
  tms <- readTVarIO $ timeSig c
  return $ checkTimeSig now tms

checkTimeSig :: Time -> [TimeSignature] ->  TimeSignature
checkTimeSig now tms = head $ possible tms now

possible :: [TimeSignature] -> Time -> [TimeSignature]
possible (t:ts) now
  | startTime t == head (filter (<= now) (starts (t:ts))) = t : possible ts now
  | otherwise = possible ts now
possible [] _ = []

starts :: [TimeSignature] -> [Time]
starts = map startTime

-- display the time in Measure.CurrPhase
currentBeat :: Clock -> IO Beats
currentBeat c = do
  now <- timeD c
  beatAt c now

beatAt :: Clock -> Time -> IO Beats
beatAt c time = do
  tms <- readTVarIO $ timeSig c
  return $ timeDelta (possible tms time) (time:starts (possible tms time))

-- return the bar number from Beats
thisBar :: Beats -> Beats
thisBar = fromIntegral . (floor :: Beats ->  Int)

nextBar :: Beats -> Beats
nextBar = (+ 1) . thisBar

-- given an amount of currPhase, bpm and beatsPerMeasure, gives a Double back representing the length in s
beatToTime :: Beats -> Double -> Double -> Double
beatToTime x bpm_val beatPerMeasure = (x * beatPerMeasure) * (60.00 / bpm_val)

-- given a time delta and a TS, return the amount of beats in that timesignature
timeToBeat :: Time -> TimeSignature -> Beats
timeToBeat delta ts = delta * (bpm ts/ 60.00) / beatInMsr ts

timeDelta :: [TimeSignature] -> [Time] -> Beats
timeDelta (x:xs) (now:sts) = timeToBeat (now  - head sts) x + timeDelta xs sts
timeDelta [] _ = 0
timeDelta _ [] = 0 -- this should never happen tbh 

-- return the current beat in a bar
beatInBar :: Clock -> IO Double
beatInBar c = do
  cb <- currentBeat c
  ts <- currentTS c
  return $ deltaBeats cb * beatInMsr ts

timeAtBeat :: Clock -> Beats -> IO Time
timeAtBeat c b = do
  ts <- currentTS c
  ob <- beatAt c (startTime ts)
  return $ startTime ts + beatToTime (b-ob) (bpm ts) (beatInMsr ts)

-- return the current phase (current beat in the bar 0 - 1) in the bar where the beast happens
deltaBeats :: Beats -> Beats
deltaBeats b = b - thisBar b
