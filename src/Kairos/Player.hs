{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}

module Kairos.Player where

import Control.Concurrent (forkIO)
import Control.Concurrent.STM (TVar, readTVarIO)
import Control.Exception (SomeException, catch)
import Control.Monad (void, when)
import Data.Map.Strict qualified as M
import Data.Maybe (fromJust, isJust)
import Kairos.Clock
  ( TimeSignature (beatInMsr),
    beatInBar,
    currentBeat,
    currentTS,
    nextBar,
    thisBar,
    timeAtBeat,
    timeD,
    waitT,
  )
import Kairos.Instrument
  ( Instr (insN, itype, kind, pats, pf, status, timeF, toPlay),
    InstrType (Effect, Instrument),
    MessageTo (Csound, OSC, Aillen),
    Status (..),
    getPort,
    notEffectOrc,
  )
import Kairos.Network (UDPPort, sendEvent, sendOSC, setChan, sendAillenSynthNote, sendAillenSamplerSelect, sendAillenSamplerLoad, sendAillenSamplerNote, sendAillenParam)
import Data.List (isPrefixOf)
import Kairos.Performance (Performance (clock, orc, timePs))
import Kairos.PfPat (PfPat (pfId, updater))
import Kairos.Pfield (PfId, PfMap, Pfield(..), idString, pfToString)
import Kairos.TimePoint
  ( TimePoint,
    TimePointf (TP, whenTP),
    fromTP,
    nextBeat,
    wrapBar,
  )
import Kairos.Utilities (addToMap, lookupMap, sameConstructor)

setChannel :: UDPPort -> String -> Pfield -> IO ()
setChannel port chanName val = do
  let m = chanName ++ " " ++ show val
  setChan port m

makeTupleFromPfields :: Instr -> IO [(PfId, Pfield)]
makeTupleFromPfields i = do
  pfpats <- readTVarIO (pats i)
  pfields <- readTVarIO (pf i)
  return $ zip (map pfId $ M.elems pfpats) $ M.elems pfields

playChannel :: Instr -> IO ()
playChannel ins = do
  theList <- makeTupleFromPfields ins
  mapM_ (setChanny (getPort (kind ins))) theList

setChanny :: UDPPort -> (PfId, Pfield) -> IO ()
setChanny p (s, v) = setChannel p (idString s) v

playInstr :: Instr -> IO ()
playInstr instr = do
  pfields <- readTVarIO $ pf instr
  let pfs = M.elems pfields
  if sameConstructor (kind instr) (Csound "")
    then do
      -- send to Csound
      if itype instr == Instrument
        then do
          -- send note + parameters
          let pfieldList = pfToString pfs
          let pfds = "i" ++ show (insN instr) ++ " 0 " ++ pfieldList
          sendEvent (getPort (kind instr)) pfds
        else
          if itype instr == Effect
            then do
              -- send effect parameters to appropriate channels
              playChannel instr
            else do
              error "Error: Unknown instrument type"
    else
      if sameConstructor (kind instr) (OSC "")
        then do
          -- send to OSC target
          sendOSC (getPort (kind instr)) (insN instr) pfs
        else
          if sameConstructor (kind instr) (Aillen "")
            then do
              playAillen instr
            else do
              error "Error: Unknown instrument destination kind"

playAillen :: Instr -> IO ()
playAillen instr = do
  pfields <- readTVarIO $ pf instr
  let trackId = insN instr
      port = getPort (kind instr)
      
      -- Find pfield by its name
      lookupByName name = case filter (\(k, _) -> idString k == name) (M.toList pfields) of
        ((_, v):_) -> Just v
        _ -> Nothing

      -- Send standard parameter mappings
      sendParamIfPresent name addr = case lookupByName name of
        Just v -> sendAillenParam port addr v
        Nothing -> return ()

  -- Send standard parameters if it's an instrument
  when (itype instr == Instrument) $ do
    sendParamIfPresent "vol" ("/track/" ++ show trackId ++ "/volume")
    sendParamIfPresent "pan" ("/track/" ++ show trackId ++ "/pan")
    sendParamIfPresent "del" ("/track/" ++ show trackId ++ "/send/delay")

  -- Send custom parameters starting with "/"
  mapM_ (\(k, v) -> when ("/" `isPrefixOf` idString k) (sendAillenParam port (rewriteTrackAddr trackId (idString k)) v)) (M.toList pfields)

  when (itype instr == Instrument) $ do
    let lookupDouble name def = case lookupByName name of
          Just (Pd x) -> x
          _ -> def
        lookupString name def = case lookupByName name of
          Just (Ps x) -> x
          _ -> def
        vol = lookupDouble "vol" 0.5
        dur = lookupDouble "dur" 1.0
        pitchVal = lookupDouble "pitch" 60.0
        samplePath = lookupString "sample" ""
        cpsVal = lookupDouble "cps" 0.0
        durMs = dur * 1000.0
        midiToHz m = 440.0 * (2.0 ** ((m - 69.0) / 12.0))
        freq = if pitchVal <= 127.0 then midiToHz pitchVal else pitchVal
    if trackId `elem` [1, 2, 3, 5]
      then do
        -- Sampler track
        when (not (null samplePath)) $ do
          let prefix = "/Users/leofltt/Desktop/KairosSamples/"
          if prefix `isPrefixOf` samplePath
            then sendAillenSamplerSelect port trackId (drop (length prefix) samplePath)
            else sendAillenSamplerLoad port trackId samplePath
        let samplerFreq = if cpsVal > 0.0 then (if cpsVal <= 127.0 then midiToHz cpsVal else cpsVal) else 261.63
        sendAillenSamplerNote port trackId samplerFreq vol
      else do
        -- Synth track
        sendAillenSynthNote port trackId freq durMs vol

rewriteTrackAddr :: Int -> String -> String
rewriteTrackAddr trackId addr =
  if "/track/" `isPrefixOf` addr
    then case drop 7 addr of
           ('/':_) -> "/track/" ++ show trackId ++ drop 7 addr
           (c:_) | c >= '0' && c <= '9' -> addr -- already has track ID
           remainder -> "/track/" ++ show trackId ++ "/" ++ remainder
    else addr

playOne :: Performance -> Instr -> TimePoint -> IO ()
playOne perf i tp = catch (do
  ts <- currentTS (clock perf)
  cb <- currentBeat (clock perf)
  now <- timeD (clock perf)
  let toBePlayed = (whenTP tp / beatInMsr ts) + thisBar cb
  if toBePlayed > cb
    then do
      nextT <- timeAtBeat (clock perf) toBePlayed
      let toWait = realToFrac (floor ((nextT - now) * 10000)) / 10000
      waitT toWait
      playOne perf i tp
    else
      if cb - toBePlayed <= 0.020
        then do
          updatePfields i
          playInstr i
        else do
          return ())
  (\e -> putStrLn $ "Error in playOne for instr " ++ show (insN i) ++ ": " ++ show (e :: SomeException))

-- | play an instrument once immediately
playNow :: Performance -> String -> IO ()
playNow perf i = do
  tp <- beatInBar (clock perf)
  mIns <- lookupMap (orc perf) i
  case mIns of
    Just p' -> playOne perf p' (pure tp)
    Nothing -> putStrLn $ "Error: Instrument " ++ i ++ " not found"

-- | start the play loop of an instrument
-- inspired by Conductive, R. Bell https://lac.linuxaudio.org/2011/papers/35.pdf
play :: Performance -> String -> IO ()
play perf pn =
  let checkStatus _ Inactive = void (forkIO $ playLoop perf pn Inactive)
      checkStatus _ Stopping = void (forkIO $ playLoop perf pn Stopping)
      checkStatus _ Init = void (forkIO $ playLoop perf pn Init)
      checkStatus _ Active = putStrLn $ "the instrument " ++ pn ++ " is already Active!"
   in do
        mIns <- lookupMap (orc perf) pn
        case mIns of
          Nothing -> putStrLn $ "Error: Instrument " ++ pn ++ " not found"
          Just i -> checkStatus i $ status i

-- | play loop callBack
playLoop :: Performance -> String -> Status -> IO ()
playLoop perf pn Active = catch (do
  mP <- lookupMap (orc perf) pn
  case mP of
    Nothing -> putStrLn $ "Error: Instrument " ++ pn ++ " disappeared from orchestra"
    Just p -> do
      now <- timeD (clock perf)
      cb <- currentBeat (clock perf)
      ts <- currentTS (clock perf)
      if timeF p == ""
        then do
          changeStatus perf pn Stopping
          mP' <- lookupMap (orc perf) pn
          case mP' of
            Just p' -> playLoop perf pn $ status p'
            Nothing -> return ()
        else do
          case toPlay p of
            Nothing -> do
               putStrLn $ "Warning: toPlay is Nothing for " ++ pn ++ " in Active state. Resetting..."
               changeStatus perf pn Init
               playLoop perf pn Init
            Just tp -> do
               mts <- lookupMap (timePs perf) (timeF p)
               case mts of
                 Nothing -> do
                    putStrLn $ "Error: Time Pattern " ++ timeF p ++ " not found for " ++ pn
                    changeStatus perf pn Stopping
                    playLoop perf pn Stopping
                 Just timeString -> do
                    if null timeString
                      then do
                        putStrLn $ "Error: Time Pattern " ++ timeF p ++ " is empty for " ++ pn
                        changeStatus perf pn Stopping
                        playLoop perf pn Stopping
                      else do
                        let nb = nextBeat tp timeString
                        let nextToPlay
                              | whenTP nb > whenTP tp = (whenTP (wrapBar ts nb) / beatInMsr ts) + thisBar cb + (fromIntegral . floor $ whenTP nb / beatInMsr ts - whenTP tp / beatInMsr ts)
                              | whenTP nb <= whenTP tp = (whenTP nb / beatInMsr ts) + nextBar cb
                              | otherwise = error "This shouldn't be happening"
                        nextTime <- timeAtBeat (clock perf) nextToPlay
                        _ <- forkIO $ playOne perf p (wrapBar ts tp)
                        updateToPlay perf pn (Just nb)
                        let toWait = realToFrac (floor ((nextTime - now) * 10000)) / 10000
                        waitT toWait
                        mP'' <- lookupMap (orc perf) pn
                        case mP'' of
                          Just p'' -> playLoop perf pn $ status p''
                          Nothing -> return ()
  ) (\e -> putStrLn $ "Error in playLoop for " ++ pn ++ ": " ++ show (e :: SomeException))
playLoop perf p Inactive = do
  changeStatus perf p Init
  playLoop perf p Init
playLoop perf i Init = catch (do
  mP <- lookupMap (orc perf) i
  case mP of
    Nothing -> putStrLn $ "Error: Instrument " ++ i ++ " not found in Init"
    Just p -> do
      if (timeF p == "")
        then do
          changeStatus perf i Stopping
          mP' <- lookupMap (orc perf) i
          case mP' of
            Just p' -> playLoop perf i $ status p'
            Nothing -> return ()
        else do
          mts <- lookupMap (timePs perf) (timeF p)
          case mts of
            Nothing -> do
              putStrLn $ "Error in Init: Time Pattern " ++ timeF p ++ " not found for " ++ i
              changeStatus perf i Stopping
              playLoop perf i Stopping
            Just timeString -> do
              if null timeString
                then do
                  putStrLn $ "Error in Init: Time Pattern " ++ timeF p ++ " is empty for " ++ i
                  changeStatus perf i Stopping
                  playLoop perf i Stopping
                else do
                  let nb = head timeString
                  updateToPlay perf i (Just nb)
                  changeStatus perf i Active
                  mP'' <- lookupMap (orc perf) i
                  case mP'' of
                    Just p'' -> playLoop perf i $ status p''
                    Nothing -> return ()
  ) (\e -> putStrLn $ "Error in playLoop (Init) for " ++ i ++ ": " ++ show (e :: SomeException))
playLoop perf p Stopping = do
  changeStatus perf p Inactive
  putStrLn $ "instrument " ++ p ++ " is now Inactive."
  return ()

-- | stop an instrument
stop :: Performance -> String -> IO ()
stop perf i = do
  mP <- lookupMap (orc perf) i
  case mP of
    Just p -> when (status p == Active) $ void $ changeStatus perf i Stopping
    Nothing -> return ()

-- | stop all instruments that are not effects
stopAll :: Performance -> IO ()
stopAll perf = do
  o <- readTVarIO (orc perf)
  mapM_ (stop perf) . M.keys $ notEffectOrc o

-- | plays all instruments that are not effects
playAll :: Performance -> IO ()
playAll perf = do
  o <- readTVarIO (orc perf)
  mapM_ (play perf) . M.keys $ notEffectOrc o

-- | solo an instrument
soloIns :: Performance -> String -> IO ()
soloIns perf i = do
  o <- readTVarIO (orc perf)
  mapM_ (stop perf) . filter (/= i) . M.keys $ notEffectOrc o

-- | display all Time Patterns names and their content
displayTPat :: Performance -> IO ()
displayTPat perf = do
  tpats <- readTVarIO (timePs perf)
  mapM_ (\(name, pat) -> putStrLn $ show name ++ " - " ++ show (fromTP pat)) (M.toList tpats)

updateInstrument :: Performance -> String -> (Instr -> Instr) -> IO ()
updateInstrument perf k f = do
  mI <- lookupMap (orc perf) k
  case mI of
    Just i -> addToMap (orc perf) (k, f i)
    Nothing -> return ()

updatePfields :: Instr -> IO ()
updatePfields i = do
  pfpats <- readTVarIO (pats i)
  mapM_ (updateonepfield (pf i)) (M.elems pfpats)

updateonepfield :: TVar PfMap -> PfPat -> IO ()
updateonepfield pfmap patts = do
  newVal <- updater patts patts
  addToMap pfmap (pfId patts, newVal)

changeStatus :: Performance -> String -> Status -> IO ()
changeStatus e k newS = updateInstrument e k (\x -> x {status = newS})

changeTimeF :: Performance -> String -> String -> IO ()
changeTimeF e k newF = do
  pl <- lookupMap (orc e) k
  case pl of
    Nothing -> putStrLn "Instrument not found"
    Just _ -> do
      tp <- lookupMap (timePs e) newF
      case tp of
        Nothing -> putStrLn "Time Pattern not found"
        Just ts -> do
          val <- closertoNow e k ts
          updateToPlay e k (Just val)
          updateInstrument e k (\x -> x {timeF = newF})

closertoNow :: Performance -> String -> [TimePoint] -> IO TimePoint
closertoNow e k ts = do
  mPl <- lookupMap (orc e) k
  case mPl of
    Nothing -> return (TP 0) -- Should not happen
    Just pl ->
      if isJust (toPlay pl)
        then do
          let toP = fromJust (toPlay pl)
          let tp
                | null ts = toP
                | not (any (<= toP) ts) = head ts
                | otherwise = last $ filter (<= toP) ts
          return tp
        else do
          now <- beatInBar (clock e)
          let tp
                | null ts = TP now
                | not (any (<= TP now) ts) = head ts
                | otherwise = last $ filter (<= TP now) ts
          return tp

updateToPlay :: Performance -> String -> Maybe TimePoint -> IO ()
updateToPlay e k newTP = updateInstrument e k (\x -> x {toPlay = newTP})
