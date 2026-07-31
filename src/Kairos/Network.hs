{-# LANGUAGE OverloadedStrings #-}

module Kairos.Network where

import Kairos.Pfield ( Pfield(Ps, Pd, Pl) )
import Kairos.Utilities ( doubleToFloat )
import Network.Socket
    ( getAddrInfo,
      connect,
      socket,
      close,
      defaultProtocol,
      AddrInfo(addrFamily, addrAddress),
      SocketType(Datagram) )
import Network.Socket.ByteString (sendAll,send)
import qualified Data.ByteString.Char8 as C
import Data.ByteString.Internal as B ( packChars )
import Vivid.OSC as V
    ( encodeOSC, OSC(..), OSCDatum(OSC_S, OSC_F, OSC_I) )
import Data.List (isSuffixOf)

type UDPPort = String 

sendMsg :: UDPPort -> String -> IO ()
sendMsg m n = do
  addrinfos <- getAddrInfo Nothing (Just "127.0.0.1") (Just m)
  case addrinfos of
    (serveraddr:_) -> do
      sock <- socket (addrFamily serveraddr) Datagram defaultProtocol
      connect sock (addrAddress serveraddr)
      sendAll sock $ C.pack n
      close sock
    [] -> putStrLn $ "Error: Could not resolve address for port " ++ m

sendCsound :: UDPPort -> String -> IO ()
sendCsound = sendMsg

sendScore :: UDPPort -> String -> IO ()
sendScore p n = let m = "$ " ++ n in
  sendCsound p m

sendEvent :: UDPPort -> String -> IO ()
sendEvent p n = let m = "& " ++ n in
  sendCsound p m

setChan :: UDPPort -> String -> IO ()
setChan p n = let m = "@" ++ n in
  sendCsound p m

-- | send an OSC messagge to port p
sendOSC :: UDPPort -> Int -> [Pfield] -> IO ()
sendOSC p i l = sendMsgOSC p $ createOSC i l

sendMsgOSC :: UDPPort -> OSC -> IO ()
sendMsgOSC m n = do
  addrinfos <- getAddrInfo Nothing (Just "127.0.0.1") (Just m)
  case addrinfos of
    (serveraddr:_) -> do
      sock <- socket (addrFamily serveraddr) Datagram defaultProtocol
      connect sock (addrAddress serveraddr)
      _ <- send sock $ V.encodeOSC n
      close sock
    [] -> putStrLn $ "Error: Could not resolve address for port " ++ m

pfieldToOSCDatum :: Pfield -> OSCDatum
pfieldToOSCDatum (Pd x) = OSC_F $ doubleToFloat x
pfieldToOSCDatum (Ps x) = OSC_S $ B.packChars x
pfieldToOSCDatum (Pl _) = error "Pl lists are not supported for standard flat pfields"

pfieldsToOSCs :: [Pfield] -> [OSCDatum]
pfieldsToOSCs = map pfieldToOSCDatum

createOSC :: Int -> [Pfield] -> OSC
createOSC i l = V.OSC (B.packChars $ "/" ++ show i) $ pfieldsToOSCs l

sendAillenSynthNote :: UDPPort -> Int -> Double -> Double -> Double -> IO ()
sendAillenSynthNote port trackId freq durMs vol =
  sendMsgOSC port $ V.OSC (B.packChars $ "/track/" ++ show trackId ++ "/note")
    [ OSC_F $ doubleToFloat freq
    , OSC_F $ doubleToFloat durMs
    , OSC_F $ doubleToFloat vol
    ]

sendAillenSamplerSelect :: UDPPort -> Int -> String -> IO ()
sendAillenSamplerSelect port trackId relPath =
  sendMsgOSC port $ V.OSC (B.packChars $ "/track/" ++ show trackId ++ "/sample/select")
    [ OSC_S $ B.packChars relPath ]

sendAillenSamplerLoad :: UDPPort -> Int -> String -> IO ()
sendAillenSamplerLoad port trackId path =
  sendMsgOSC port $ V.OSC (B.packChars $ "/track/" ++ show trackId ++ "/sample/load")
    [ OSC_S $ B.packChars path ]

sendAillenSamplerNote :: UDPPort -> Int -> Double -> Double -> IO ()
sendAillenSamplerNote port trackId freq vol =
  sendMsgOSC port $ V.OSC (B.packChars $ "/track/" ++ show trackId ++ "/note/on")
    [ OSC_F $ doubleToFloat freq
    , OSC_F $ doubleToFloat vol
    ]

sendAillenParam :: UDPPort -> String -> Pfield -> IO ()
sendAillenParam port addr (Pl val) = do
  let datums = zipWith (pfieldToOSCDatumForAddrIndexed addr) [0..] val
  sendMsgOSC port $ V.OSC (B.packChars addr) datums
sendAillenParam port addr (Ps val) = do
  let wordsList = words val
      datums = zipWith (wordToDatum addr) [0..] wordsList
  sendMsgOSC port $ V.OSC (B.packChars addr) datums
sendAillenParam port addr (Pd val) =
  sendMsgOSC port $ V.OSC (B.packChars addr) [pfieldToOSCDatumForAddrIndexed addr 0 (Pd val)]

pfieldToOSCDatumForAddrIndexed :: String -> Int -> Pfield -> OSCDatum
pfieldToOSCDatumForAddrIndexed addr idx (Pd x) =
  case (addr, idx) of
    ("/track/7/hubass/osc/unison", 0) -> OSC_I $ round x
    ("/track/7/hubass/osc/unison", 3) -> OSC_I $ round x
    ("/track/7/hubass/osc/sub", 0) -> OSC_I $ round x
    ("/track/7/hubass/osc/sub", 1) -> OSC_I $ round x
    ("/track/7/hubass/drive/mode", 0) -> OSC_I $ round x
    ("/track/7/hubass/filter/mode", 0) -> OSC_I $ round x
    ("/track/7/hubass/lfo/1", 0) -> OSC_I $ round x
    ("/track/6/303/pwm/params", _) -> OSC_F $ doubleToFloat x
    _ -> if isIntAddr addr
           then OSC_I $ round x
           else OSC_F $ doubleToFloat x
pfieldToOSCDatumForAddrIndexed _ _ (Ps x) = OSC_S $ B.packChars x
pfieldToOSCDatumForAddrIndexed _ _ (Pl _) = error "Nested lists in OSC parameters are not supported"

isIntAddr :: String -> Bool
isIntAddr addr =
  "/mode" `isSuffixOf` addr ||
  "/waveform" `isSuffixOf` addr ||
  "/mute" `isSuffixOf` addr ||
  "/source" `isSuffixOf` addr ||
  "/sidechain" `isSuffixOf` addr ||
  "/drop" `isSuffixOf` addr ||
  "/outof" `isSuffixOf` addr ||
  "/select" `isSuffixOf` addr ||
  "/count" `isSuffixOf` addr ||
  "/stutter" `isSuffixOf` addr ||
  "/density" `isSuffixOf` addr ||
  "/pingpong" `isSuffixOf` addr ||
  "/stretch" `isSuffixOf` addr ||
  "/overlap" `isSuffixOf` addr

wordToDatum :: String -> Int -> String -> OSCDatum
wordToDatum addr idx word =
  case (addr, idx) of
    ("/track/7/hubass/osc/unison", 0) -> OSC_I $ parseI word
    ("/track/7/hubass/osc/unison", 3) -> OSC_I $ parseI word
    ("/track/7/hubass/osc/sub", 0) -> OSC_I $ parseI word
    ("/track/7/hubass/osc/sub", 1) -> OSC_I $ parseI word
    ("/track/7/hubass/drive/mode", 0) -> OSC_I $ parseI word
    ("/track/7/hubass/filter/mode", 0) -> OSC_I $ parseI word
    ("/track/7/hubass/lfo/1", 0) -> OSC_I $ parseI word
    ("/track/6/303/pwm/params", _) -> OSC_F $ parseF word
    _ -> if isIntAddr addr || (not (null word) && all (`elem` ("0123456789-" :: String)) word)
           then OSC_I $ parseI word
           else OSC_F $ parseF word
  where
    parseI w = case reads w of
      [(i, "")] -> i
      _ -> 0
    parseF w = case reads w of
      [(f, "")] -> doubleToFloat f
      _ -> 0.0
