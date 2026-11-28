{-# LANGUAGE OverloadedStrings #-}

module Kairos.Telemetry
  ( TelemetryHandle (..),
    startTelemetry,
    startTelemetryWith,
    stopTelemetry,
    autoStartTelemetry,
  )
where

import Control.Concurrent (ThreadId, forkIO, killThread, threadDelay)
import Control.Concurrent.STM
  ( TBQueue,
    atomically,
    newTBQueueIO,
    readTBQueue,
    readTVarIO,
    writeTVar,
  )
import Control.Exception (SomeException, catch)
import Control.Monad (forever)
import Data.ByteString qualified as B
import Data.ByteString.Builder (Builder, char7, doubleDec, intDec, stringUtf8, toLazyByteString)
import Data.ByteString.Lazy qualified as BL
import Data.Char (ord)
import Data.List (intersperse)
import Data.Map.Strict qualified as M
import Data.Maybe (fromMaybe)
import Kairos.Clock (Clock (linkState), TimeSignature (beatInMsr, bpm), beatInBar, currentBeat, currentTS, thisBar)
import Kairos.Instrument (Instr (..), InstrType (Instrument), Status (Inactive))
import Kairos.Performance (NoteEvent (..), Performance (..))
import Kairos.PfPat (PfPat (pfId))
import Kairos.Pfield (PfId, PfMap, Pfield (..), idInt, idString)
import Kairos.TimePoint (TimePoint, fromTP, whenTP)
import Network.Socket
  ( AddrInfo (addrAddress, addrFamily),
    Socket,
    SocketType (Datagram),
    close,
    connect,
    defaultProtocol,
    getAddrInfo,
    socket,
  )
import Network.Socket.ByteString (sendAll)
import Numeric (showHex)
import System.Environment (lookupEnv)
import Text.Read (readMaybe)

data TelemetryHandle = TelemetryHandle
  { thPerf :: Performance,
    thSocket :: Socket,
    thStateThread :: ThreadId,
    thNoteThread :: ThreadId
  }

-- | Start UDP JSON telemetry on 127.0.0.1:<port> at 25 Hz.
startTelemetry :: Performance -> Int -> IO TelemetryHandle
startTelemetry perf port = startTelemetryWith perf "127.0.0.1" port 25

-- | Start UDP JSON telemetry with custom host, port, and state refresh rate (Hz).
startTelemetryWith :: Performance -> String -> Int -> Int -> IO TelemetryHandle
startTelemetryWith perf host port rateHz = do
  addrinfos <- getAddrInfo Nothing (Just host) (Just (show port))
  case addrinfos of
    [] -> error $ "Kairos.Telemetry: could not resolve " ++ host ++ ":" ++ show port
    (serveraddr : _) -> do
      sock <- socket (addrFamily serveraddr) Datagram defaultProtocol
      connect sock (addrAddress serveraddr)
      q <- newTBQueueIO 256
      atomically $ writeTVar (noteQueue perf) (Just q)
      let delayUs = max 10000 (1000000 `div` max 1 rateHz)
      sTid <- forkIO $ stateLoop perf sock delayUs
      nTid <- forkIO $ noteLoop q sock
      return $
        TelemetryHandle
          { thPerf = perf,
            thSocket = sock,
            thStateThread = sTid,
            thNoteThread = nTid
          }

-- | Stop telemetry threads, clear the note queue, and close the UDP socket.
stopTelemetry :: TelemetryHandle -> IO ()
stopTelemetry th = do
  atomically $ writeTVar (noteQueue (thPerf th)) Nothing
  killThread (thStateThread th)
  killThread (thNoteThread th)
  close (thSocket th)

-- | Auto-start telemetry if KAIROS_TELEMETRY_PORT is set in the environment.
autoStartTelemetry :: Performance -> IO (Maybe TelemetryHandle)
autoStartTelemetry perf = do
  mPortStr <- lookupEnv "KAIROS_TELEMETRY_PORT"
  case mPortStr >>= readMaybe of
    Just port | port > 0 -> Just <$> startTelemetry perf port
    _ -> return Nothing

stateLoop :: Performance -> Socket -> Int -> IO ()
stateLoop perf sock delayUs = forever $ do
  catch
    ( do
        payload <- buildStateJson perf
        sendAll sock payload
    )
    (\(_ :: SomeException) -> return ())
  threadDelay delayUs

noteLoop :: TBQueue NoteEvent -> Socket -> IO ()
noteLoop q sock = forever $ do
  ev <- atomically $ readTBQueue q
  catch
    ( do
        let payload = BL.toStrict $ toLazyByteString (encodeNoteEvent ev)
        sendAll sock payload
    )
    (\(_ :: SomeException) -> return ())

buildStateJson :: Performance -> IO B.ByteString
buildStateJson perf = do
  let clk = clock perf
  ts <- currentTS clk
  cb <- currentBeat clk
  bib <- beatInBar clk
  orch <- readTVarIO (orc perf)
  tpats <- readTVarIO (timePs perf)
  let barNum = floor (thisBar cb) :: Int
      modeStr = case linkState clk of
        Nothing -> "Local"
        Just _ -> "Link"
      activeInstruments = M.filter (\i -> itype i == Instrument && status i /= Inactive) orch
  instrBuilders <- mapM (encodeInstrument tpats) (M.toAscList activeInstruments)
  return . BL.toStrict . toLazyByteString $
    obj
      [ ("type", jStr "state"),
        ( "clock",
          obj
            [ ("bar", intDec barNum),
              ("beat_in_bar", jDouble bib),
              ("bpm", jDouble (bpm ts)),
              ("beats_per_bar", jDouble (beatInMsr ts)),
              ("mode", jStr modeStr)
            ]
        ),
        ("instruments", arr instrBuilders)
      ]

encodeInstrument :: M.Map String [TimePoint] -> (String, Instr) -> IO Builder
encodeInstrument tpats (name, instr) = do
  pfs <- readTVarIO (pf instr)
  patMap <- readTVarIO (pats instr)
  let tpatBeats = fromMaybe [] (fromTP <$> M.lookup (timeF instr) tpats)
      toPlayVal = case toPlay instr of
        Nothing -> char7 'n' <> stringUtf8 "ull"
        Just tp -> jDouble (whenTP tp)
      patternedIds = map (pfIdLabel . pfId) (M.elems patMap)
  return $
    obj
      [ ("name", jStr name),
        ("ins_n", intDec (insN instr)),
        ("status", jStr (show (status instr))),
        ("time_f", jStr (timeF instr)),
        ("tpat", arr (map jDouble tpatBeats)),
        ("to_play", toPlayVal),
        ("pfields", encodePfMap pfs),
        ("patterned_pfs", arr (map jStr patternedIds))
      ]

encodeNoteEvent :: NoteEvent -> Builder
encodeNoteEvent ev =
  obj
    [ ("type", jStr "note"),
      ("instr", jStr (neInstr ev)),
      ("beat", jDouble (neBeat ev)),
      ("pfields", encodePfMap (nePfields ev))
    ]

encodePfMap :: PfMap -> Builder
encodePfMap pfs =
  obj [(pfIdLabel k, encodePfield v) | (k, v) <- M.toAscList pfs]

pfIdLabel :: PfId -> String
pfIdLabel pid =
  let s = idString pid
   in if null s then "p" ++ show (idInt pid) else s

encodePfield :: Pfield -> Builder
encodePfield (Ps s) = jStr s
encodePfield (Pd d) = jDouble d
encodePfield (Pl xs) = arr (map encodePfield xs)

-- Minimal JSON builders

obj :: [(String, Builder)] -> Builder
obj fields =
  char7 '{'
    <> mconcat (intersperse (char7 ',') [jStr k <> char7 ':' <> v | (k, v) <- fields])
    <> char7 '}'

arr :: [Builder] -> Builder
arr xs = char7 '[' <> mconcat (intersperse (char7 ',') xs) <> char7 ']'

jDouble :: Double -> Builder
jDouble d
  | isNaN d || isInfinite d = stringUtf8 "null"
  | otherwise = doubleDec d

jStr :: String -> Builder
jStr s = char7 '"' <> mconcat (map escapeChar s) <> char7 '"'
  where
    escapeChar '"' = stringUtf8 "\\\""
    escapeChar '\\' = stringUtf8 "\\\\"
    escapeChar '\n' = stringUtf8 "\\n"
    escapeChar '\r' = stringUtf8 "\\r"
    escapeChar '\t' = stringUtf8 "\\t"
    escapeChar c
      | ord c < 0x20 =
          let hex = showHex (ord c) ""
              padded = replicate (4 - length hex) '0' ++ hex
           in stringUtf8 ("\\u" ++ padded)
      | otherwise = stringUtf8 [c]
