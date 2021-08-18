module Kairos.Performance where

import Kairos.Instrument ( Instr(pats), Orchestra )
import Kairos.Clock
import Kairos.TimePoint ( TimePoint )
import Kairos.Pfield
import Kairos.Utilities ( addToMap, lookupMap, stringToDouble )
import Control.Concurrent.STM ( newTVarIO, readTVarIO, TVar )
import qualified Data.Map.Strict as M
import Data.Maybe (fromJust, isNothing)

-- the Performance is the scope of the composition
data Performance = P { orc :: Orchestra
                     , clock :: Clock
                     , timePs :: TVar (M.Map [Char] [TimePoint]) -- a map of time patterns with their names
                     }

-- function to create a PfPat
createPfPat :: Int -> [Pfield] -> (PfPat -> IO Pfield) -> IO PfPat
createPfPat num pfields updtr = do
  ptrn <- newTVarIO pfields
  return $ PfPat { pfNum = num
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

maybeAddTPf :: Performance -> String -> Maybe [TimePoint] -> IO ()
maybeAddTPf e n ts | isNothing ts = putStrLn "Pattern is empty"
                   | otherwise = addTPf e n $ fromJust ts