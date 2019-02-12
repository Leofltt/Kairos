module Kairos.Network where

import Network.Socket hiding (recv)
import Network.Socket.ByteString (recv, sendAll)
import qualified Data.ByteString.Char8 as C 
import Control.Concurrent

-- UDP network to connect to Csound on port 10000

sendMsg :: String -> IO ()
sendMsg n = do
  addrinfos <- getAddrInfo Nothing (Just "127.0.0.1") (Just "10000")
  let serveraddr = head addrinfos
  sock <- socket (addrFamily serveraddr) Datagram defaultProtocol
  connect sock (addrAddress serveraddr)
  sendAll sock $ C.pack n   
  close sock

sendNote :: String -> IO ()
sendNote n = let m = "$ " ++ n in
  sendMsg m

sendEvent :: String -> IO ()
sendEvent n = let m = "& " ++ n in
  sendMsg m
 
