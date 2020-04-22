{-# LANGUAGE OverloadedStrings #-}

module Kairos.Network where

import Kairos.Base
import Kairos.Instrument
import Kairos.Utilities
import Network.Socket hiding (recv)
import Network.Socket.ByteString (recv, sendAll,send)
import qualified Data.ByteString.Char8 as C
import Data.ByteString.Internal as B
import Control.Concurrent
import Vivid.OSC as V


-- UDP network to connect to Csound on port 11000

sendMsg :: String -> String -> IO ()
sendMsg m n = do
  addrinfos <- getAddrInfo Nothing (Just "127.0.0.1") (Just m)
  let serveraddr = head addrinfos
  sock <- socket (addrFamily serveraddr) Datagram defaultProtocol
  connect sock (addrAddress serveraddr)
  sendAll sock $ C.pack n
  close sock

sendCsound = sendMsg "11000"

sendScore :: String -> IO ()
sendScore n = let m = "$ " ++ n in
  sendCsound m

sendEvent :: String -> IO ()
sendEvent n = let m = "& " ++ n in
  sendCsound m

setChan :: String -> IO ()
setChan n = let m = "@" ++ n in
  sendCsound m

-- send an OSC messagge to port 11100
sendOSC i l = sendMsgOSC "11100" $ createOSC i l

sendMsgOSC :: String -> OSC -> IO ()
sendMsgOSC m n = do
  addrinfos <- getAddrInfo Nothing (Just "127.0.0.1") (Just m)
  let serveraddr = head addrinfos
  sock <- socket (addrFamily serveraddr) Datagram defaultProtocol
  connect sock (addrAddress serveraddr)
  send sock $ V.encodeOSC $ n
  close sock

pfieldToOSCDatum :: Pfield -> OSCDatum
pfieldToOSCDatum (Pd x) = OSC_F $ doubleToFloat x
pfieldToOSCDatum (Ps x) = OSC_S $ B.packChars x

pfieldsToOSCs :: [Pfield] -> [OSCDatum]
pfieldsToOSCs = map pfieldToOSCDatum

createOSC :: Int -> [Pfield] -> OSC
createOSC i l = V.OSC (B.packChars $ "/" ++ show i) $ pfieldsToOSCs l
