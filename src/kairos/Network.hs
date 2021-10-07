{-# LANGUAGE OverloadedStrings #-}

module Kairos.Network where

import Kairos.Pfield ( Pfield(Ps, Pd) )
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
    ( encodeOSC, OSC(..), OSCDatum(OSC_S, OSC_F) )

-- UDP network to connect to Csound on port 11000

sendMsg :: String -> String -> IO ()
sendMsg m n = do
  addrinfos <- getAddrInfo Nothing (Just "127.0.0.1") (Just m)
  let serveraddr = head addrinfos
  sock <- socket (addrFamily serveraddr) Datagram defaultProtocol
  connect sock (addrAddress serveraddr)
  sendAll sock $ C.pack n
  close sock

sendCsound :: String -> String -> IO ()
sendCsound = sendMsg

sendScore :: String -> String -> IO ()
sendScore p n = let m = "$ " ++ n in
  sendCsound p m

sendEvent :: String -> String -> IO ()
sendEvent p n = let m = "& " ++ n in
  sendCsound p m

setChan :: String -> String -> IO ()
setChan p n = let m = "@" ++ n in
  sendCsound p m

-- send an OSC messagge to port 11100
sendOSC :: String -> Int -> [Pfield] -> IO ()
sendOSC p i l = sendMsgOSC p $ createOSC i l

sendMsgOSC :: String -> OSC -> IO ()
sendMsgOSC m n = do
  addrinfos <- getAddrInfo Nothing (Just "127.0.0.1") (Just m)
  let serveraddr = head addrinfos
  sock <- socket (addrFamily serveraddr) Datagram defaultProtocol
  connect sock (addrAddress serveraddr)
  _ <- send sock $ V.encodeOSC n
  close sock

pfieldToOSCDatum :: Pfield -> OSCDatum
pfieldToOSCDatum (Pd x) = OSC_F $ doubleToFloat x
pfieldToOSCDatum (Ps x) = OSC_S $ B.packChars x

pfieldsToOSCs :: [Pfield] -> [OSCDatum]
pfieldsToOSCs = map pfieldToOSCDatum

createOSC :: Int -> [Pfield] -> OSC
createOSC i l = V.OSC (B.packChars $ "/" ++ show i) $ pfieldsToOSCs l
