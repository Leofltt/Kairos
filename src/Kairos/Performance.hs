{-# OPTIONS_GHC -Wno-missing-fields #-}
module Kairos.Performance where

import Kairos.Instrument ( Instr(pats), Orchestra, defaultOrc )
import Kairos.Clock
    ( TimeSignature(bpm, beatInMsr), Clock, defaultClock, currentTS )
import Kairos.TimePoint ( TimePoint, notEmpty, defaultTPMap )
import Kairos.Pfield
import Kairos.PfPat
import Kairos.Utilities ( addToMap, lookupMap, stringToDouble )
import Control.Concurrent.STM ( newTVarIO, readTVarIO, TVar )
import qualified Data.Map.Strict as M
import Data.Maybe (fromJust, isNothing)

-- | the Performance is the scope of the composition
data Performance = P { orc :: Orchestra
                     , clock :: Clock
                     , timePs :: TVar (M.Map [Char] [TimePoint]) -- a map of time patterns with their names
                     }

-- | create a default performance
defaultPerformance :: IO Performance
defaultPerformance = do
  o <- defaultOrc
  c <- defaultClock
  t <- defaultTPMap
  return $ P { orc = o
             , clock = c
             , timePs = t
             }

-- function to create a PfPat
createPfPat :: Int -> String -> [Pfield] -> (PfPat -> IO Pfield) -> IO PfPat
createPfPat num name pfields updtr = do
  ptrn <- newTVarIO pfields
  return $ PfPat { pfId = Either num name
                 , pat = ptrn
                 , updater = updtr
                 }

addPfPath :: Instr -> Int -> PfPat -> IO ()
addPfPath i num pfPat = addToMap (pats i) (num,pfPat)

addPfPath' :: Performance -> [Char] -> Int -> PfPat -> IO ()
addPfPath' e insname num pfPat = do
  Just i <- lookupMap (orc e) insname
  addPfPath i num pfPat

displayInstruments :: Performance -> IO String
displayInstruments perf = do
   ins <- readTVarIO (orc perf)
   return $ unwords $ M.keys ins

withTimeSignature :: Performance -> [Pfield] -> IO [Pfield]
withTimeSignature perf l = do
  ts <- currentTS $ clock perf
  let oneSecond = 60/bpm ts
  let oneBarSecond = beatInMsr ts * oneSecond
  let beatsInSeconds = map (*oneBarSecond) (stringToDouble $ map show l)
  return $ toPfs beatsInSeconds

-- to add instruments
addInstrument :: Performance -> String -> Instr -> IO ()
addInstrument perf name instr = addToMap (orc perf) (name,instr)

getTimePoint :: Performance -> String -> IO [TimePoint]
getTimePoint perf s = do
  Just t <- lookupMap (timePs perf) s
  return t

-- add a named pattern of timepoints to a performance
addTPf :: Performance -> String -> [TimePoint] -> IO ()
addTPf e n ts = addToMap (timePs e) (n,ts)

maybeAddTPf :: Performance -> String -> [TimePoint] -> IO ()
maybeAddTPf e n ts | isNothing mts = putStrLn "Pattern is empty"
                   | otherwise = addTPf e n $ fromJust mts
                   where mts = notEmpty ts