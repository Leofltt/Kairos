module Kairos.Player where

import Kairos.Base
import Kairos.TimePoint
import Kairos.Clock
import Kairos.Instrument
import Kairos.Network
import Kairos.Transport
import Control.Concurrent
import Control.Concurrent.STM
import qualified Data.Map.Strict as M

-- the environment is the scope of the composition
data Environment = E { orc :: Orchestra
                     , clock :: Clock
                     , timePs :: TVar (M.Map [Char] [TimePoint])
                     }

defaultEnvironment :: IO Environment
defaultEnvironment = do
  o <- defaultOrc
  c <- defaultClock
  t <- defaultTPMap
  return $ E { orc = o
             , clock = c
             , timePs = t
             }


playInstr :: Instr -> IO ([Char])
playInstr instr = do
  pfields <- readTVarIO $ pf instr
  let pfs = M.elems pfields
  let pfieldList = pfToString pfs
  let pfds = "i" ++ (show (insN instr)) ++ " 0 " ++ pfieldList
  forkIO $ sendNote pfds
  return $ show $ pfds

-- given a TP, checks if it's in the future. If not, plays it
-- playAt :: Clock -> Instr -> TimePoint -> IO ()

playOld :: Clock -> Instr ->  [TimePoint] -> IO ()
playOld c i tp = do
  ts <- currentTS c
  cb <- currentBeat c
  let toBePlayed = ((ioi (head tp))/(beatInMsr ts)) + (thisBar cb)
  let toBeEnded = ((end (head tp))/(beatInMsr ts)) + (thisBar cb)
  if (toBePlayed > cb)
     then do toWait <- timeAtBeat c toBePlayed
             putStrLn $ "to soon! it's " ++ (show cb)
             putStrLn $ "pausing until: " ++ (show toBePlayed)
             waitUntil c (toWait+0.002) >> playOld c i tp
     else if (cb > toBeEnded)
        then do let tp' = nextBeat tp
                let tW |(ioi(head tp')) > (ioi (head tp)) =  timeAtBeat c  ((((ioi (head tp'))/(beatInMsr ts)) + (thisBar cb))::Beats)
                       |(ioi(head tp')) <= (ioi (head tp)) = timeAtBeat c ((((ioi (head tp'))/(beatInMsr ts)) + (nextBar cb))::Beats)
                toWait <- tW
                waitUntil c (toWait+0.002) >> playOld c i tp'; return ()
        else if ((toBePlayed <= cb) && (cb < toBeEnded))
           then do print <- playInstr i
                   let tp' = nextBeat tp
                   putStrLn $ "Just played " ++ print
                   let nextB = ((((ioi (head tp'))/(beatInMsr ts)) + (thisBar cb))::Beats)
                   toWait  <- timeAtBeat c toBeEnded
                   putStrLn $ "Waiting for next beat after " ++ (show nextB)
                   waitUntil c (toWait+0.002) >> playOld c i tp'; return ()
           else return ()

playNew :: Environment -> String -> IO ()
--playWhen :: (Beats -> Bool) -> Pattern a -> Pattern a
--playWhen f p = p { query = (filter (f . (st . wholE)) . query p)}

--playDelta :: Beats -> Beats -> Pattern a -> Pattern a
--playDelta s e = playWhen (\t -> and [ t >= s, t< e])

downB = [(TP 1 2),(TP 3 4)]

fourFloor = [(TP 0 1),(TP 1 2),(TP 2 3),(TP 3 4)]

defaultTPMap :: IO (TVar (M.Map [Char] [TimePoint]))
defaultTPMap = do
  tpMap <- newTVarIO $ M.fromList [("fourFloor", fourFloor),("downB", downB)]
  return $ tpMap
