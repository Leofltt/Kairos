module Kairos.Player where

import Kairos.TimePoint
import Kairos.Clock
import Kairos.Instrument
import Kairos.Network
import Kairos.Pattern
import Control.Concurrent
import Control.Concurrent.STM
import qualified Data.Map.Strict as M


playInstr :: Instr -> IO ([Char])
playInstr instr = do
  pfields <- readTVarIO $ pf instr
  let pfs = M.elems pfields
  let pfieldList = pfToString pfs
  let pfds = "i" ++ (show (insN instr)) ++ " 0 " ++ pfieldList 
  sendNote pfds
  return $ show $ pfds

-- given an TP, checks if it's in the future. If not, plays it

playAt :: Clock -> Instr -> TimePoint -> IO ()
playAt clock instr tp = do
  ts <- currentTS clock
  cb <- currentBeat clock 
  if ((ioi tp) > cb) 
     then do let diff = (ioi tp) - cb
             let dfs = beatToTime diff (bpm ts) (beatInMsr ts)
             threadDelay $ truncate $ dfs * 1000000
             playAt clock instr tp
  else if (cb > (end tp))
     then return ()
  else if (cb > (ioi tp) && cb < (end tp)) 
     then playInstr instr >> return ()
  else return ()

--playWhen :: (Beats -> Bool) -> Pattern a -> Pattern a
--playWhen f p = p { query = (filter (f . (st . wholE)) . query p)}

--playDelta :: Beats -> Beats -> Pattern a -> Pattern a
--playDelta s e = playWhen (\t -> and [ t >= s, t< e])


