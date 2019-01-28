module Kairos.Player where

import Kairos.Clock
import Kairos.Instrument
import Kairos.Network
import Control.Concurrent
import Control.Concurrent.STM
import qualified Data.Map.Strict as M

playInstr :: IO Instr -> IO ()
playInstr i = do
  instr <- i
  pfields <- readTVarIO $ pf instr
  let pfs = M.elems pfields
  let pfieldList = pfToString pfs
  let pfds = "i" ++ (show (insN instr)) ++ " 0 " ++ pfieldList 
  sendNote pfds
