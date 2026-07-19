module Kairos.Clock where

import Control.Monad ( when )
import Control.Concurrent ( threadDelay, forkIO )
import Control.Concurrent.STM
    ( atomically, newTVarIO, readTVarIO, writeTVar, TVar )
import Data.Time.Clock.POSIX ( getPOSIXTime )
import Control.Monad.IO.Class ( MonadIO(..) )
import Kairos.Utilities ( safeHead )
import Network.Socket
    ( getAddrInfo,
      connect,
      socket,
      defaultProtocol,
      AddrInfo(addrFamily, addrAddress),
      SocketType(Stream),
      Socket )
import Network.Socket.ByteString ( sendAll, recv )
import qualified Data.ByteString.Char8 as C
import Control.Exception ( catch, SomeException )
import System.IO.Unsafe ( unsafePerformIO )

-- | Ableton Link State
data LinkState = LinkState
  { linkBpm       :: !Double
  , linkBeatNow   :: !Double
  , linkLocalTime :: !Double
  } deriving (Show, Eq)

-- | clock
data Clock = Clock { startAt :: Time
                   , timeSig :: TVar [TimeSignature]
                   , linkState :: Maybe (TVar LinkState, Socket)
                   }

instance Show Clock where
  show c = unsafePerformIO $ do
    case linkState c of
      Nothing -> do
        ts <- readTVarIO (timeSig c)
        let latestTS = safeHead (TS 4 120 0) ts
        return $ "Clock {mode = LocalMode, bpm = " ++ show (bpm latestTS) ++ ", beatInMsr = " ++ show (beatInMsr latestTS) ++ "}"
      Just (var, _) -> do
        state <- readTVarIO var
        ts <- readTVarIO (timeSig c)
        let latestTS = safeHead (TS 4 120 0) ts
        return $ "Clock {mode = LinkMode, bpm = " ++ show (linkBpm state) ++ ", beatInMsr = " ++ show (beatInMsr latestTS) ++ "}"




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
  let modeStr = case linkState c of
                  Nothing -> "Local"
                  Just _  -> "Link"
  return $ "clock (" ++ modeStr ++ ") bar: " ++ show (thisBar cb) ++ ", beat: " ++ take 4 (show beat) ++ ", at tempo: " ++ show (bpm ts) ++" bpm."


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
                 , linkState = Nothing
                 }

linkClock :: IO Clock
linkClock = do
  s <- getNow
  let timesig = TS { bpm = 120
                   ,  beatInMsr = 4
                   ,  startTime = 0
                   }
  ts <- newTVarIO [timesig]
  addrinfos <- getAddrInfo Nothing (Just "127.0.0.1") (Just "17000")
  case addrinfos of
    (serveraddr:_) -> do
      sock <- socket (addrFamily serveraddr) Stream defaultProtocol
      connect sock (addrAddress serveraddr)
      lstateVar <- newTVarIO LinkState
        { linkBpm = 120.0
        , linkBeatNow = 0.0
        , linkLocalTime = s
        }
      _ <- forkIO $ catch (carabinerListener sock lstateVar)
                         (\e -> putStrLn $ "Carabiner listener error: " ++ show (e :: SomeException))
      return Clock { startAt = s
                   , timeSig = ts
                   , linkState = Just (lstateVar, sock)
                   }
    [] -> error "Could not resolve address for Carabiner (127.0.0.1:17000)"

carabinerListener :: Socket -> TVar LinkState -> IO ()
carabinerListener sock var = loop ""
  where
    loop acc = do
      chunk <- recv sock 4096
      if C.null chunk
        then putStrLn "Carabiner connection closed."
        else do
          let combined = acc ++ C.unpack chunk
              (linesList, remaining) = splitLines combined
          mapM_ (handleMessage var) linesList
          loop remaining

    splitLines :: String -> ([String], String)
    splitLines s = go s []
      where
        go xs acc = case break (== '\n') xs of
          (line, "") -> (reverse acc, line)
          (line, _:rest) -> go rest (line : acc)

handleMessage :: TVar LinkState -> String -> IO ()
handleMessage var line = do
  case parseCarabinerStatus line of
    Just (bpmVal, beatVal) -> do
      now <- getNow
      atomically $ writeTVar var LinkState
        { linkBpm = bpmVal
        , linkBeatNow = beatVal
        , linkLocalTime = now
        }
    Nothing -> return ()

parseCarabinerStatus :: String -> Maybe (Double, Double)
parseCarabinerStatus msg = do
  let ws = words msg
  bpmVal <- lookupVal ":bpm" ws
  beatVal <- lookupVal ":beat" ws
  return (bpmVal, beatVal)
  where
    lookupVal key (k:v:xs)
      | k == key = safeRead (clean v)
      | otherwise = lookupVal key (v:xs)
    lookupVal _ _ = Nothing

    clean = filter (`notElem` ("{}," :: String))

    safeRead s = case reads s of
      [(val, "")] -> Just val
      _ -> Nothing


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
changeTempo c t = case linkState c of
  Nothing -> do
    cts <- currentTS c
    tss <- addTS c $ newTS t (beatInMsr cts) 1 -- 1: tempo is changed on the next bar
    putStrLn $ "Current bpm: " ++ show (bpm $ safeHead cts tss)
  Just (_, sock) -> do
    sendAll sock $ C.pack $ "bpm " ++ show t ++ "\n"

-- | given a clock and a TS, prepends the TS to the list of current ts in the clock, correcting the start time appropriately
addTS :: Clock -> TimeSignature -> IO [TimeSignature]
addTS c t = do
  ts <- readTVarIO $ timeSig c
  curBeat <- currentBeat c
  curTS <- currentTS c
  beatCurTs <- beatAt c (startTime curTS)
  atomically $ writeTVar (timeSig c) (newTS (bpm t) (beatInMsr t) (max (beatToTime (thisBar curBeat - beatCurTs) (bpm curTS) (beatInMsr curTS) + startTime curTS) (startTime $ safeHead curTS ts) + beatToTime (startTime t) (bpm t) (beatInMsr t)):ts)
  readTVarIO $ timeSig c

currentTS :: Clock -> IO TimeSignature
currentTS c = case linkState c of
  Nothing -> do
    now <- timeD c
    tms <- readTVarIO $ timeSig c
    return $ checkTimeSig now tms
  Just (var, _) -> do
    state <- readTVarIO var
    tms <- readTVarIO $ timeSig c
    let baseTS = checkTimeSig 0 tms
    return baseTS { bpm = linkBpm state }

checkTimeSig :: Time -> [TimeSignature] ->  TimeSignature
checkTimeSig now tms = case possible tms now of
  [] -> TS 4 120 0 -- default fallback
  (x:_) -> x

possible :: [TimeSignature] -> Time -> [TimeSignature]
possible (t:ts) now
  | startTime t == safeHead (startTime t) (filter (<= now) (starts (t:ts))) = t : possible ts now
  | otherwise = possible ts now
possible [] _ = []

starts :: [TimeSignature] -> [Time]
starts = map startTime

-- display the time in Measure.CurrPhase
currentBeat :: Clock -> IO Beats
currentBeat c = case linkState c of
  Nothing -> do
    now <- timeD c
    beatAt c now
  Just (var, _) -> do
    state <- readTVarIO var
    now <- getNow
    ts <- currentTS c
    let deltaSecs = now - linkLocalTime state
        beatsSecs = linkBpm state / 60.0
        elapsedBeats = deltaSecs * beatsSecs
        totalRawBeats = linkBeatNow state + elapsedBeats
    return $ totalRawBeats / beatInMsr ts

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
timeDelta (x:xs) (now:sts) = timeToBeat (now  - safeHead now sts) x + timeDelta xs sts
timeDelta [] _ = 0
timeDelta _ [] = 0 -- this should never happen tbh 

-- return the current beat in a bar
beatInBar :: Clock -> IO Double
beatInBar c = do
  cb <- currentBeat c
  ts <- currentTS c
  return $ deltaBeats cb * beatInMsr ts

timeAtBeat :: Clock -> Beats -> IO Time
timeAtBeat c b = case linkState c of
  Nothing -> do
    ts <- currentTS c
    ob <- beatAt c (startTime ts)
    return $ startTime ts + beatToTime (b-ob) (bpm ts) (beatInMsr ts)
  Just (var, _) -> do
    state <- readTVarIO var
    ts <- currentTS c
    let targetRawBeat = b * beatInMsr ts
        rawBeatDiff = targetRawBeat - linkBeatNow state
        secDiff = rawBeatDiff / (linkBpm state / 60.0)
        targetPOSIX = linkLocalTime state + secDiff
    return $ targetPOSIX - startAt c


-- return the current phase (current beat in the bar 0 - 1) in the bar where the beast happens
deltaBeats :: Beats -> Beats
deltaBeats b = b - thisBar b
