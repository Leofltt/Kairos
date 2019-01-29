module Kairos.Player where

import Kairos.Clock
import Kairos.Instrument
import Kairos.Network
import Control.Concurrent
import Control.Concurrent.STM
import qualified Data.Map.Strict as M

playInstr :: Instr -> IO (String)
playInstr instr = do
  pfields <- readTVarIO $ pf instr
  let pfs = M.elems pfields
  let pfieldList = pfToString pfs
  let pfds = "i" ++ (show (insN instr)) ++ " 0 " ++ pfieldList 
  sendNote pfds
  return $ pfds
