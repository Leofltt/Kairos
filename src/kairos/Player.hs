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


playInstr :: Instr -> IO ()
playInstr instr = do
  pfields <- readTVarIO $ pf instr
  let pfs = M.elems pfields
  let pfieldList = pfToString pfs
  let pfds = "i" ++ (show (insN instr)) ++ " 0 " ++ pfieldList
  sendNote pfds

-- given a TP, checks if it's in the future. If not, plays it
-- playAt :: Clock -> Instr -> TimePoint -> IO ()


playOne :: Environment -> Instr -> TimePoint -> IO ()
playOne e i tp = do
   --Just p <- lookupMap (orc e) i
   ts <- currentTS (clock e)
   cb <- currentBeat (clock e)
   let toBePlayed = ((ioi (tp))/(beatInMsr ts)) + (thisBar cb)
--   let toBeEnded = ((end (tp))/(beatInMsr ts)) + (thisBar cb)
   if (toBePlayed > cb)
      then do toWait <- timeAtBeat (clock e) toBePlayed
              waitUntil (clock e) (toWait+0.002)
              playOne e i tp
      else do playInstr i
              return ()


play :: Environment -> String -> IO ()
play e pn = let
  checkStatus p Stopped  = ( forkIO $ playLoop e pn $ Stopped)  >> return ()
  checkStatus p Paused   = ( playLoop e pn $ Paused)   >> return ()
  checkStatus p Stopping = ( playLoop e pn $ Stopping) >> return ()
  --checkStatus p Pausing  = ( playLoop e pn $ Pausing)  >> return ()
  checkStatus p Playing  = putStrLn $ "the player " ++ pn ++ " is already playing!"
  in do Just p <- lookupMap (orc e) pn
        checkStatus p $ status p

playLoop :: Environment -> String -> Status -> IO ()

playLoop e pn Playing = do
  Just p <- lookupMap (orc e) pn
  now <- timeD (clock e)
  cb <- currentBeat (clock e)
  ts <- currentTS (clock e)
  let pb = toPlay p
  if (pb == Nothing)
     then do  changeStatus e pn Stopping
              Just p' <- lookupMap (orc e) pn
              playLoop e pn $ status p'
     else do  let tp = fromJust pb
              Just timeString <- lookupMap (timePs e)  (timeF p)
              let nb = nextBeat timeString tp
              let nextToPlay | (ioi (head nb)) > (ioi (tp)) = ((ioi (head nb))/(beatInMsr ts)) + (thisBar cb)
                             | (ioi (head nb)) <= (ioi (tp)) = ((ioi (head nb))/(beatInMsr ts)) + (nextBar cb)
              --                               | (ioi (head nb)) == (ioi (tp)) = ((ioi (head nb))/(beatInMsr ts)) + (nextBa
              let thisToPlay = ((ioi (tp))/(beatInMsr ts)) + (thisBar cb)
              nextTime <- timeAtBeat (clock e) nextToPlay
              forkIO $ playOne e p tp
              updateToPlay e pn (head nb)
              let toWait = nextTime - now
              waitT toWait
              Just p' <- lookupMap (orc e) pn
              playLoop e pn $ status p'

-- playLoop e pn Playing = do
--   Just p <- lookupMap (orc e) pn
--   cb <- currentBeat (clock e)
--   ts <- currentTS (clock e)
--   let pb = toPlay p
--   if (pb == Nothing)
--       then do  changeStatus e pn Stopping
--                Just p' <- lookupMap (orc e) pn
--                playLoop e pn $ status p'
--       else do  let tp = fromJust pb
--                Just timeString <- lookupMap (timePs e)  (timeF p)
--                let nb = nextBeat timeString
--                updateToPlay e pn (head nb)
--                let nextToPlay | (ioi (head nb)) > (ioi (tp)) = ((ioi (head nb))/(beatInMsr ts)) + (thisBar cb)
--                               | (ioi (head nb)) < (ioi (tp)) = ((ioi (head nb))/(beatInMsr ts)) + (nextBar cb)
--                               | (ioi (head nb)) == (ioi (tp)) = ((ioi (head nb))/(beatInMsr ts)) + (nextBar cb)
--                forkIO $ playOne (clock e) p tp
--                toWait <- timeAtBeat (clock e) nextToPlay
--                changeStatus e pn Pausing
--                now <- timeD (clock e)
--                let wt = toWait - now
--                updateWaitTime e pn wt
--                Just p' <- lookupMap (orc e) pn
--                playLoop e pn $ status p'
              -- let toBePlayed = ((ioi (tp))/(beatInMsr ts)) + (thisBar cb)
              -- let toBeEnded = ((end (tp))/(beatInMsr ts)) + (thisBar cb)
              -- if (toBePlayed > cb)
              --    then do toWait <- timeAtBeat (clock e) toBePlayed
              --            putStrLn $ "the beat " ++ (show toBePlayed) ++ " is in the future"
              --            putStrLn $ "the current beat is " ++ (show cb)
              --            changeStatus e pn Pausing
              --            now <- timeD (clock e)
              --            let wt = toWait - now
              --            updateWaitTime e pn wt
              --            Just p' <- lookupMap (orc e) pn
              --            playLoop e pn $ status p'
              --    else do --if (toBeEnded > cb)
              --         --then do --sequence_ [return ()]
              --            Just timeString <- lookupMap (timePs e) (timeF p)
              --            let nb = nextBeat timeString
              --            playInstr p -- >> return  ()
              --            updateToPlay e pn (head nb)
              --            let nextToPlay = ((ioi (head nb))/(beatInMsr ts)) + (thisBar cb)
              --            toWait <- timeAtBeat (clock e) nextToPlay
              --            changeStatus e pn Pausing
              --            now <- timeD (clock e)
              --            let wt = toWait - now
              --            updateWaitTime e pn wt
              --            Just p' <- lookupMap (orc e) pn
              --            playLoop e pn $ status p
              {-        else do Just timeString <- lookupMap (timePs e) (timeF p)
                              let nb = nextBeat timeString
                              updateToPlay e pn (head nb)
                              let tW | (ioi(head nb)) <= (ioi(tp)) = timeAtBeat (clock e) ((((ioi (head nb))/(beatInMsr ts)) + (nextBar cb))::Beats)
                                     | (ioi(head nb)) > (ioi(tp)) = timeAtBeat (clock e) ((((ioi (head nb))/(beatInMsr ts)) + (thisBar cb))::Beats)
                              toWait <- tW
                              waitUntil (clock e) toWait
                              Just p' <- lookupMap (orc e) pn
                              playLoop e pn $ status p'
                              -}

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

-- updateTimeS :: Environment -> String -> [TimePoint] -> IO ()
-- updateTimeS e k newT = updateInstrument e k (\x -> x { timeF =  newT })

updateWaitTime :: Environment -> String -> Time -> IO ()
updateWaitTime e k newWT = updateInstrument e k (\x -> x { waitTime = newWT })
