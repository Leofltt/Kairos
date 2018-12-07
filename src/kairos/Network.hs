module Kairos.Network where

import Kairos.Base
import Network.Socket hiding (recv)
import Network.Socket.ByteString (recv, sendAll)
import qualified Data.ByteString.Char8 as C 

-- need UDP network to connect to Csound on port 10000

sendNote :: String -> IO ()
sendNote n = do
  addrinfos <- getAddrInfo Nothing (Just "127.0.0.1") (Just "10000")
  let serveraddr = head addrinfos
  let m = "$ " ++ n
  sock <- socket (addrFamily serveraddr) Datagram defaultProtocol
  connect sock (addrAddress serveraddr)
  sendAll sock $ C.pack m   
  close sock
