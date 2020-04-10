module Kairos.Network where

import Kairos.Base
import Network.Socket hiding (recv)
import Network.Socket.ByteString (recv, sendAll)
import qualified Data.ByteString.Char8 as C
import Control.Concurrent

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

sendOther = sendMsg "11100"

sendScore :: String -> IO ()
sendScore n = let m = "$ " ++ n in
  sendCsound m

sendEvent :: String -> IO ()
sendEvent n = let m = "& " ++ n in
  sendCsound m

setChan :: String -> IO ()
setChan n = let m = "@" ++ n in
  sendCsound m
