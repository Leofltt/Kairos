module Kairos.Player where

import Kairos.Base
import Kairos.TimePoint
import Kairos.Clock
import Kairos.Instrument
import Kairos.Network
import Kairos.MapUtilities
import Control.Concurrent
import Control.Concurrent.STM
import Data.Maybe
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


playInstr :: Instr -> IO ()    -- ([Char])
playInstr instr = do
  pfields <- readTVarIO $ pf instr
  let pfs = M.elems pfields
  let pfieldList = pfToString pfs
  let pfds = "i" ++ (show (insN instr)) ++ " 0 " ++ pfieldList
  sendNote pfds
--  return $ show $ pfds

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
--             putStrLn $ "to soon! it's " ++ (show cb)
--             putStrLn $ "pausing until: " ++ (show toBePlayed)
             waitUntil c (toWait+0.002) >> playOld c i tp
     else if (cb > toBeEnded)
             then do let tp' = nextBeat tp
                     let tW |(ioi(head tp')) > (ioi (head tp)) =  timeAtBeat c  ((((ioi (head tp'))/(beatInMsr ts)) + (thisBar cb))::Beats)
                            |(ioi(head tp')) <= (ioi (head tp)) = timeAtBeat c ((((ioi (head tp'))/(beatInMsr ts)) + (nextBar cb))::Beats)
                     toWait <- tW
                     waitUntil c (toWait+0.002) >> playOld c i tp'; return ()
             else if ((toBePlayed <= cb) && (cb < toBeEnded))
                     then do --print <- 
                              playInstr i
                              let tp' = nextBeat tp
--                            putStrLn $ "Just played " ++ print
                              let nextB = ((((ioi (head tp'))/(beatInMsr ts)) + (thisBar cb))::Beats)
                              toWait  <- timeAtBeat c toBeEnded
--                            putStrLn $ "Waiting for next beat after " ++ (show nextB)
                              waitUntil c (toWait+0.002) >> playOld c i tp'; return ()
           else return ()

play :: Environment -> String -> IO ()
play e pn = let
  checkStatus p Stopped  = (forkIO $ playLoop e pn $ Stopped)  >> return ()
  checkStatus p Paused   = (forkIO $ playLoop e pn $ Paused)   >> return ()
  checkStatus p Stopping = (forkIO $ playLoop e pn $ Stopping) >> return ()
  checkStatus p Pausing  = (forkIO $ playLoop e pn $ Pausing)  >> return ()
  checkStatus p Playing  = putStrLn $ "the player " ++ pn ++ " is already playing!"
  in do Just p <- lookupMap (orc e) pn
        checkStatus p $ status p

playLoop :: Environment -> String -> Status -> IO ()

playLoop e pn Playing = do
  Just p <- lookupMap (orc e) pn
  cb <- currentBeat (clock e)
  ts <- currentTS (clock e)
  let pb = toPlay p
  if (pb == Nothing)
      then do  changeStatus e pn Stopping
               Just p' <- lookupMap (orc e) pn
               playLoop e pn $ status p'
      else do let tp = fromJust pb
              let toBePlayed = ((ioi (tp))/(beatInMsr ts)) + (thisBar cb)
              let toBeEnded = ((end (tp))/(beatInMsr ts)) + (thisBar cb)
              if (toBePlayed > cb)
                 then do toWait <- timeAtBeat (clock e) toBePlayed
                         putStrLn $ "the beat " ++ (show toBePlayed) ++ " is in the future"
                         putStrLn $ "the current beat is " ++ (show cb)  
                         waitUntil (clock e) toWait
                         Just p' <- lookupMap (orc e) pn
                         playLoop e pn $ status p'
                 else if (toBeEnded > cb)
                      then do --sequence_ [return ()]
                              Just timeString <- lookupMap (timePs e) (timeF p)
                              let nb = nextBeat timeString
                              forkIO $ playInstr p -- >> return  ()
                              updateToPlay e pn (head nb)
                              let nextToPlay = ((ioi (head nb))/(beatInMsr ts)) + (thisBar cb)
                              toWait <- timeAtBeat (clock e) nextToPlay
                              waitUntil (clock e) toWait
                              Just p' <- lookupMap (orc e) pn
                              playLoop e pn $ status p
                      else do Just timeString <- lookupMap (timePs e) (timeF p)
                              let nb = nextBeat timeString
                              updateToPlay e pn (head nb)
                              let tW | (ioi(head nb)) <= (ioi(tp)) = timeAtBeat (clock e) ((((ioi (head nb))/(beatInMsr ts)) + (nextBar cb))::Beats)
                                     | (ioi(head nb)) > (ioi(tp)) = timeAtBeat (clock e) ((((ioi (head nb))/(beatInMsr ts)) + (thisBar cb))::Beats)
                              toWait <- tW
                              waitUntil (clock e) toWait
                              Just p' <- lookupMap (orc e) pn
                              playLoop e pn $ status p'

playLoop e p Stopped = do
  changeStatus e p Playing
  playLoop e p Playing

playLoop e p Paused = do
  changeStatus e p Playing
  playLoop e p Playing

playLoop e p Stopping = do
  changeStatus e p Stopped
  putStrLn $ "instrument " ++ p ++ " has been stopped."
  return ()

playLoop e p Pausing = do
  Just ins <- lookupMap (orc e) p
  let toWait = waitTime ins
  waitT toWait
  changeStatus e p Paused
  Just p' <- lookupMap (orc e) p
  playLoop e p $ status p'

--playWhen :: (Beats -> Bool) -> Pattern a -> Pattern a
--playWhen f p = p { query = (filter (f . (st . wholE)) . query p)}

--playDelta :: Beats -> Beats -> Pattern a -> Pattern a
--playDelta s e = playWhen (\t -> and [ t >= s, t< e])

--- default Patterns

downB = [(TP 1 1.5),(TP 3 3.5)]

fourFloor = [(TP 0 0.5),(TP 1 1.5),(TP 2 2.5),(TP 3 3.5)]


defaultTPMap :: IO (TVar (M.Map [Char] [TimePoint]))
defaultTPMap = do
  tpMap <- newTVarIO $ M.fromList [("fourFloor", fourFloor),("downB", downB)]
  return $ tpMap


updateInstrument :: Environment -> String -> (Instr -> Instr) -> IO ()
updateInstrument e k f = do
  Just p <- lookupMap (orc e) k
  addToMap (orc e) (k, f p)

changeStatus :: Environment -> String -> Status -> IO ()
changeStatus e k newS = updateInstrument e k (\x -> x { status = newS })

updateToPlay :: Environment -> String -> TimePoint -> IO ()
updateToPlay e k newTP = updateInstrument e k (\x -> x { toPlay = Just newTP })

