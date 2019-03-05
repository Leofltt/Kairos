module Kairos.Clock where

import Kairos.Base
import Control.Monad
import Control.Concurrent
import Control.Concurrent.STM
import Data.Time.Clock.POSIX
import Control.Monad.IO.Class


displayClock c = do
  ts   <- currentTS c
  cb   <- currentBeat c
  beat <- beatInBar c
  return $ "clock's bar: " ++ (show $ thisBar cb) ++ ", beat: " ++ (take 4 $ show $ beat) ++ ", at tempo: " ++ (show $ bpm ts) ++" bpm."

getNow :: MonadIO m => m Time
getNow = liftIO $ fmap realToFrac getPOSIXTime

timeD :: MonadIO m => Clock -> m Time
timeD clock = let
  s = startAt clock in
  do
  x <- getNow
  return (x - s)

waitUntil :: MonadIO m => Clock -> Time -> m ()
waitUntil c t = waitT . (t -) =<< (timeD c)

waitT :: (MonadIO m, RealFrac a) => a -> m ()
waitT t = when (t > 0) (liftIO (threadDelay(floor (t * 1000000))))

defaultClock  = do
  s <- getNow
  let timesig = TS { bpm = 120
                   ,  beatInMsr = 4
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
     , beatInMsr = msr
     , startTime = strt
     }

currentTempo :: Clock -> IO Double
currentTempo c = do
  cts <- currentTS c
  return $ bpm $ cts

changeTempo :: Clock -> Double -> IO ()
changeTempo c t = do
  cts <- currentTS c
  tss <- addTS c $ newTS t (beatInMsr cts) 0
  putStrLn $ "Current bpm: " ++ show (bpm $ head tss)

-- given a clock and a TS, prepends the TS to the list of current ts in the clock, correcting the start time appropriately
addTS :: Clock -> TimeSignature -> IO [TimeSignature]
addTS c t = do
  ts <- readTVarIO $ timeSig c
  curBeat <- currentBeat c
  curTS <- currentTS c
  beatCurTs <- beatAt c (startTime curTS)
  atomically $ writeTVar (timeSig c) ((newTS (bpm t) (beatInMsr t) ((max ((beatToTime ((thisBar curBeat) - beatCurTs) (bpm curTS) (beatInMsr curTS)) + (startTime curTS)) (startTime $ head ts)) + (beatToTime (startTime t) (bpm t) (beatInMsr t)))):ts)
  tim <- readTVarIO $ timeSig c
  return $ tim

currentTS :: Clock -> IO TimeSignature
currentTS c = do
  now <- timeD c
  tms <- readTVarIO $ timeSig c
  return $ checkTimeSig now tms

checkTimeSig :: Time -> [TimeSignature] ->  TimeSignature
checkTimeSig now tms = head $ possible tms now

possible :: [TimeSignature] -> Time -> [TimeSignature]
possible (t:ts) now
  | ((startTime t) == (head $ filter (<= now) (starts (t:ts)))) = t : (possible ts now)
  | otherwise = possible ts now
possible [] now = []

starts :: [TimeSignature] -> [Time]
starts ts = map startTime ts

-- display the time in Measure.CurrPhase
currentBeat :: Clock -> IO Beats
currentBeat c = do
  now <- timeD c
  result <- beatAt c now
  return $ result

beatAt :: Clock -> Time -> IO Beats
beatAt c time = do
  tms <- readTVarIO $ timeSig c
  return $ timeDelta (possible tms time) (time:(starts (possible tms time)))

-- return the bar number from Beats
thisBar :: Beats -> Beats
thisBar = fromIntegral . (floor :: Beats ->  Int)

nextBar :: Beats -> Beats
nextBar = (+ 1) . thisBar

-- given an amount of currPhase, bpm and beatsPerMeasure, gives a Double back representing the length in s
beatToTime :: Beats -> Double -> Double -> Double
beatToTime x bpm beatPerMeasure = (x * beatPerMeasure) * (60.00 / bpm)

-- given a time delta and a TS, return the amount of beats in that timesignature
timeToBeat :: Time -> TimeSignature -> Beats
timeToBeat delta ts = delta * ((bpm ts)/ 60.00) / (beatInMsr ts)

timeDelta :: [TimeSignature] -> [Double] -> Double
timeDelta (x:xs) (now:sts) = (timeToBeat (now  - (head sts)) x) + (timeDelta xs sts)
timeDelta [] _ = 0

--elapsedSinceTSC
-- return the current beat in a bar
beatInBar :: Clock -> IO Double
beatInBar c = do
  cb <- currentBeat c
  ts <- currentTS c
  return $ (cb - (thisBar cb)) * (beatInMsr ts)

timeAtBeat :: Clock -> Beats -> IO Time
timeAtBeat c b = do
  ts <- currentTS c
  ob <- beatAt c (startTime ts)
  return $ (startTime ts) + (beatToTime (b-ob) (bpm ts) (beatInMsr ts))

-- return the current phase (current beat in the bar 0 - 1) in the bar where the beast happens
deltaBeats :: Beats -> Beats
deltaBeats b = b - (thisBar b)
