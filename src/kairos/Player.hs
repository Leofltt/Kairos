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
   let toBePlayed = ((start (tp))/(beatInMsr ts)) + (thisBar cb)
   if (toBePlayed > cb)
      then do toWait <- timeAtBeat (clock e) toBePlayed
              waitUntil (clock e) toWait
              playOne e i tp
      else do playInstr i
              return ()


play :: Environment -> String -> IO ()
play e pn = let
  checkStatus p Stopped  = ( forkIO $ playLoop e pn $ Stopped)  >> return ()
  checkStatus p Stopping = ( playLoop e pn $ Stopping) >> return ()
  checkStatus p Playing  = putStrLn $ "the player " ++ pn ++ " is already playing!"
  in do Just p <- lookupMap (orc e) pn
        checkStatus p $ status p

-- play loop callBack
playLoop :: Environment -> String -> Status -> IO ()

playLoop e pn Playing = do
  Just p <- lookupMap (orc e) pn
  now <- timeD (clock e)
  cb <- currentBeat (clock e)
  ts <- currentTS (clock e)
  let pb = toPlay p
  if ((pb == Nothing) || ((timeF p )== ""))
     then do  changeStatus e pn Stopping
              Just p' <- lookupMap (orc e) pn
              playLoop e pn $ status p'
     else do  let tp = fromJust pb
              Just timeString <- lookupMap (timePs e) (timeF p)
              let nb = nextBeat timeString tp
              let nextToPlay | (start (head nb)) > (start (head timeString)) = ((start (head nb))/(beatInMsr ts)) + (thisBar cb)
                             | (start (head nb)) <= (start (head timeString)) = ((start (head nb))/(beatInMsr ts)) + (nextBar cb)
              let thisToPlay = ((start (head timeString))/(beatInMsr ts)) + (thisBar cb)
              nextTime <- timeAtBeat (clock e) nextToPlay
              forkIO $ playOne e p  $ head timeString
              updateToPlay e pn (head nb)
              Just ins <- lookupMap (orc e) pn
              putStrLn $ show $ fromJust $ toPlay ins
              let toWait = nextTime - now
              waitT toWait
              Just p' <- lookupMap (orc e) pn
              playLoop e pn $ status p'

playLoop e p Stopped = do
  changeStatus e p Playing
  playLoop e p Playing


playLoop e p Stopping = do
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

eightN = [(TP 0 0.4),(TP 0.5 0.9),(TP 1 1.4),(TP 1.5 1.9),(TP 2 2.4),(TP 2.5 2.9),(TP 3 3.4),(TP 3.5 4)]

sixteenN = [(TP 0 0.2),(TP 0.25 0.45),(TP 0.5 0.7),(TP 0.75 0.95),(TP 1 1.2),(TP 1.25 1.45),(TP 1.5 1.7),(TP 1.75 1.95),
            (TP 2 2.2),(TP 2.25 2.45),(TP 2.5 2.7),(TP 2.75 2.95),(TP 3 3.2),(TP 3.25 3.45),(TP 3.5 3.7),(TP 3.75 4)]

-------------------------------------------------------------

quarterN :: Environment -> String -> IO (TimePoint)
quarterN e s = do
  Just i <- lookupMap (orc e) s
  let toP = toPlay i
  let newTP =  fromJust toP + 1
  return $ newTP

defaultTPMap :: IO (TVar (M.Map [Char] [TimePoint]))
defaultTPMap = do
  tpMap <- newTVarIO $ M.fromList [("fourFloor", fourFloor),("downB", downB),("upFour", upFour),("eightN",eightN),("sixteenN",sixteenN)]
  return $ tpMap


updateInstrument :: Environment -> String -> (Instr -> Instr) -> IO ()
updateInstrument e k f = do
  Just p <- lookupMap (orc e) k
  addToMap (orc e) (k, f p)

changeStatus :: Environment -> String -> Status -> IO ()
changeStatus e k newS = updateInstrument e k (\x -> x { status = newS })

changeTimeF :: Environment -> String -> String -> IO ()
changeTimeF e k newF = do
  Just pl <- lookupMap (orc e) k
  let Just toP = toPlay pl
  updateToPlay e k (TP {start = 0, end = (end toP)})
  updateInstrument e k (\x -> x { timeF = newF })


updateToPlay :: Environment -> String -> TimePoint -> IO ()
updateToPlay e k newTP = updateInstrument e k (\x -> x { toPlay = Just newTP })

-- updateWaitTime :: Environment -> String -> Time -> IO ()
-- updateWaitTime e k newWT = updateInstrument e k (\x -> x { waitTime = newWT })
