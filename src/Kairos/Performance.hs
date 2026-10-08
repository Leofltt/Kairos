{-# OPTIONS_GHC -Wno-missing-fields #-}

module Kairos.Performance where

import Control.Concurrent.STM (TVar, newTVarIO, readTVarIO)
import Data.List (sortOn)
import Data.Map.Strict qualified as M
import Data.Maybe (fromJust, isNothing)
import Kairos.Clock
  ( Clock,
    TimeSignature (beatInMsr, bpm),
    currentTS,
    defaultClock,
  )
import Kairos.Instrument (Instr (..), MessageTo (..), Orchestra, aillenOrc)
import Kairos.PfPat
import Kairos.Pfield
import Kairos.TimePoint (TimePoint, defaultTPMap, notEmpty)
import Kairos.Utilities (addToMap, lookupMap, stringToDouble)

-- | the Performance is the scope of the composition
data Performance = P
  { orc :: Orchestra,
    clock :: Clock,
    timePs :: TVar (M.Map [Char] [TimePoint]) -- a map of time patterns with their names
    -- TODO: gotta add Musical Key in here
  }

-- | create a default performance
defaultPerformance :: IO Performance
defaultPerformance = do
  o <- aillenOrc
  c <- defaultClock
  t <- defaultTPMap
  return $
    P
      { orc = o,
        clock = c,
        timePs = t
      }

-- | print the parameters of an instrument in a performance
displayParams :: Performance -> String -> IO ()
displayParams perf name = do
  orch <- readTVarIO (orc perf)
  case M.lookup name orch of
    Nothing -> putStrLn "Instrument not found"
    Just i -> do
      pfs <- readTVarIO (pf i)
      putStrLn $ "--- Parameters for " ++ name ++ " (Instr " ++ show (insN i) ++ ") ---"
      let sortedPfs = M.toAscList pfs
      mapM_ (\(pid, val) -> putStrLn $ "  p" ++ show (idInt pid) ++ " [" ++ idString pid ++ "]: " ++ show val) sortedPfs

-- function to create a PfPat
createPfPat :: Int -> String -> [Pfield] -> (PfPat -> IO Pfield) -> IO PfPat
createPfPat num name pfields updtr = do
  ptrn <- newTVarIO pfields
  return $
    PfPat
      { pfId = Either num name,
        pat = ptrn,
        updater = updtr
      }

addPfPath :: Instr -> Int -> PfPat -> IO ()
addPfPath i num pfPat = addToMap (pats i) (num, pfPat)

addPfPath' :: Performance -> [Char] -> Int -> PfPat -> IO ()
addPfPath' e insname num pfPat = do
  Just i <- lookupMap (orc e) insname
  addPfPath i num pfPat

displayInstruments :: Performance -> IO String
displayInstruments perf = do
  ins <- readTVarIO (orc perf)
  return $ unwords $ M.keys ins

-- | Helper to extract sample path info from an instrument
getSampleInfo :: Instr -> IO String
getSampleInfo i = do
  pfields <- readTVarIO (pf i)
  case M.lookup (newPfId 29 "sample") pfields of
    Just (Ps path) | not (null path) -> do
      let filename = reverse $ takeWhile (/= '/') $ reverse path
      return $ if null filename then "" else " [" ++ filename ++ "]"
    _ -> return ""

-- | Human-readable track labels for Aillen tracks
aillenTrackLabel :: Int -> String
aillenTrackLabel 0 = "Track 0 (TwoOp Synth)"
aillenTrackLabel 1 = "Track 1 (Sampler: Kicks)"
aillenTrackLabel 2 = "Track 2 (Sampler: Snares & Claps)"
aillenTrackLabel 3 = "Track 3 (Sampler: Hats, Perc, Vox & FX)"
aillenTrackLabel 4 = "Track 4 (Resonator / Karplus-Strong)"
aillenTrackLabel 5 = "Track 5 (Sampler: Breaks & Stutters)"
aillenTrackLabel 6 = "Track 6 (Synth303: Acid Bass)"
aillenTrackLabel 7 = "Track 7 (SynthHubass: Hyper-Bass)"
aillenTrackLabel 8 = "Track 8 (SWAVE Synth: SuperWave)"
aillenTrackLabel 550 = "Track 550 (Return Reverb)"
aillenTrackLabel 551 = "Track 551 (Return Delay)"
aillenTrackLabel 999 = "Track 999 (Master Mixer)"
aillenTrackLabel n = "Track " ++ show n

-- | Render grouped tracks and instruments into a tree
renderAillenTree :: [(Int, [(String, String)])] -> [String]
renderAillenTree [] = ["Aillen Orchestra: (no Aillen instruments found)"]
renderAillenTree tracks = "Aillen Orchestra" : concatMap renderTrack (withLast tracks)
  where
    withLast [] = []
    withLast [x] = [(x, True)]
    withLast (x : xs) = (x, False) : withLast xs

    renderTrack ((trNum, items), isLastTrack) =
      let (tPrefix, cPrefix) =
            if isLastTrack
              then ("└── ", "    ")
              else ("├── ", "│   ")
          tHeader = tPrefix ++ aillenTrackLabel trNum
          itemLines = map (renderItem cPrefix) (withLast items)
       in tHeader : itemLines

    renderItem cPrefix ((name, info), isLastItem) =
      let iPrefix = if isLastItem then "└── " else "├── "
       in cPrefix ++ iPrefix ++ name ++ info

-- | Return a string representing the Aillen instruments organized in a track tree
displayAillenInstrumentsStr :: Performance -> IO String
displayAillenInstrumentsStr perf = do
  orch <- readTVarIO (orc perf)
  let aillenOnly = M.filter (\i -> case kind i of Aillen _ -> True; _ -> False) orch
  pairsWithInfo <-
    mapM
      ( \(n, i) -> do
          sInfo <- getSampleInfo i
          return (insN i, (n, sInfo))
      )
      (M.toList aillenOnly)
  let trackMap = M.fromListWith (++) [(trNum, [item]) | (trNum, item) <- pairsWithInfo]
  let sortedTracks = [(trNum, sortOn fst items) | (trNum, items) <- M.toAscList trackMap]
  return $ unlines $ renderAillenTree sortedTracks

-- | Display Aillen instruments organised by track in a tree view
displayAillenInstruments :: Performance -> IO ()
displayAillenInstruments perf = do
  s <- displayAillenInstrumentsStr perf
  putStr s

withTimeSignature :: Performance -> [Pfield] -> IO [Pfield]
withTimeSignature perf l = do
  ts <- currentTS $ clock perf
  let oneSecond = 60 / bpm ts
  let oneBarSecond = beatInMsr ts * oneSecond
  let beatsInSeconds = map (* oneBarSecond) (stringToDouble $ map show l)
  return $ toPfs beatsInSeconds

-- to add instruments
addInstrument :: Performance -> String -> Instr -> IO ()
addInstrument perf name instr = addToMap (orc perf) (name, instr)

getTimePoint :: Performance -> String -> IO [TimePoint]
getTimePoint perf s = do
  Just t <- lookupMap (timePs perf) s
  return t

-- add a named pattern of timepoints to a performance
addTPf :: Performance -> String -> [TimePoint] -> IO ()
addTPf e n ts = addToMap (timePs e) (n, ts)

maybeAddTPf :: Performance -> String -> [TimePoint] -> IO ()
maybeAddTPf e n ts
  | isNothing mts = putStrLn "Pattern is empty"
  | otherwise = addTPf e n $ fromJust mts
  where
    mts = notEmpty ts

-- | resolve instrument name to its sample path (p29)
resolvePfield :: Performance -> Pfield -> IO Pfield
resolvePfield perf (Ps name) = do
  orch <- readTVarIO (orc perf)
  case M.lookup name orch of
    Nothing -> return (Ps name)
    Just i -> do
      pfields <- readTVarIO (pf i)
      return $ M.findWithDefault (Ps name) (newPfId 29 "sample") pfields
resolvePfield _ pf = return pf
