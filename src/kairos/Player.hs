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
   if (toBePlayed > cb)
      then do toWait <- timeAtBeat (clock e) toBePlayed
              waitUntil (clock e) (toWait+0.002)
              playOne e i tp
      else do playInstr i
              return ()


play :: Environment -> String -> String -> IO ()
play e pn times = let
  checkStatus p Stopped  = ( forkIO $ playLoop e pn times $ Stopped)  >> return ()
  checkStatus p Stopping = ( playLoop e pn times $ Stopping) >> return ()
  checkStatus p Playing  = putStrLn $ "the player " ++ pn ++ " is already playing!"
  in do Just p <- lookupMap (orc e) pn
        checkStatus p $ status p

-- play loop callBack
playLoop :: Environment -> String -> String -> Status -> IO ()

playLoop e pn times Playing = do
  Just p <- lookupMap (orc e) pn
  now <- timeD (clock e)
  cb <- currentBeat (clock e)
  ts <- currentTS (clock e)
  let pb = toPlay p
  if (pb == Nothing)
     then do  changeStatus e pn Stopping
              Just p' <- lookupMap (orc e) pn
              playLoop e pn times $ status p'
     else do  let tp = fromJust pb
              Just timeString <- lookupMap (timePs e)  times
              updateToPlay e pn $ checkSame tp (head timeString)
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
              playLoop e pn times $ status p'

playLoop e p times Stopped = do
  changeStatus e p Playing
  playLoop e p times Playing


playLoop e p times Stopping = do
  changeStatus e p Stopped
  putStrLn $ "instrument " ++ p ++ " has been stopped."
  return ()


--playWhen :: (Beats -> Bool) -> Pattern a -> Pattern a
--playWhen f p = p { query = (filter (f . (st . wholE)) . query p)}

--playDelta :: Beats -> Beats -> Pattern a -> Pattern a
--playDelta s e = playWhen (\t -> and [ t >= s, t< e])

stop :: Environment -> String -> IO ()
stop e p = changeStatus e p Stopping

--- default Patterns ----------------------------------------

downB = [(TP 1 1.5),(TP 3 3.5)]

upFour = [(TP 0.5 1),(TP 1.5 2),(TP 2.5 3),(TP 3.5 4)]

fourFloor = [(TP 0 0.5),(TP 1 1.5),(TP 2 2.5),(TP 3 3.5)]

-------------------------------------------------------------



defaultTPMap :: IO (TVar (M.Map [Char] [TimePoint]))
defaultTPMap = do
  tpMap <- newTVarIO $ M.fromList [("fourFloor", fourFloor),("downB", downB),("upFour", upFour) ]
  return $ tpMap


updateInstrument :: Environment -> String -> (Instr -> Instr) -> IO ()
updateInstrument e k f = do
  Just p <- lookupMap (orc e) k
  addToMap (orc e) (k, f p)

changeStatus :: Environment -> String -> Status -> IO ()
changeStatus e k newS = updateInstrument e k (\x -> x { status = newS })

-- changeTimeF :: Environment -> String -> String -> IO ()
-- changeTimeF e k newF = updateInstrument e k (\x -> x { timeF = newF })


updateToPlay :: Environment -> String -> TimePoint -> IO ()
updateToPlay e k newTP = updateInstrument e k (\x -> x { toPlay = Just newTP })

checkSame :: TimePoint -> TimePoint -> TimePoint
checkSame m o | m  == o = m
              | m  /= o = o
-- updateWaitTime :: Environment -> String -> Time -> IO ()
-- updateWaitTime e k newWT = updateInstrument e k (\x -> x { waitTime = newWT })
