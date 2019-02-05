module Kairos.Player where

import Kairos.IOI
import Kairos.Clock
import Kairos.Instrument
import Kairos.Network
import Control.Concurrent
import Control.Concurrent.STM
import qualified Data.Map.Strict as M


playInstr :: Instr -> IO ()
playInstr instr = do
  pfields <- readTVarIO $ pf instr
  let pfs = M.elems pfields
  let pfieldList = pfToString pfs
  let pfds = "i" ++ (show (insN instr)) ++ " 0 " ++ pfieldList 
  sendNote pfds

-- given an IOI, checks if it's in the future. If not, plays it

playAt :: Clock -> Instr -> IOI -> IO ()
playAt clock instr iOI = do
  ts <- currentTS clock
  cb <- currentBeat clock 
  if ((st iOI) > cb) 
     then do let diff = (st iOI) - cb
             let dfs = beatToTime diff (bpm ts) (beat ts)
             threadDelay $ truncate $ dfs * 1000000
             playAt clock instr iOI
  else if (cb > (end iOI))
     then return ()
  else if (cb > (st iOI) && cb < (end iOI)) 
     then playInstr instr
  else return ()
