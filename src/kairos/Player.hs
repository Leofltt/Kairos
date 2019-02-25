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

defaultPerformance :: IO Performance
defaultPerformance = do
  o <- defaultOrc
  f <- defaultFx
  c <- defaultClock
  t <- defaultTPMap
  return $ P { orc = o
             , fx = f
             , clock = c
             , timePs = t
             }


playInstr :: Instr -> IO ()
playInstr instr = do
  pfields <- readTVarIO $ pf instr
  let pfs = M.elems pfields
  let pfieldList = pfToString pfs
  let pfds = "i" ++ (show (insN instr)) ++ " 0 " ++ pfieldList
  sendEvent pfds

-- given a TP, checks if it's in the future. If not, plays it
-- playAt :: Clock -> Instr -> TimePoint -> IO ()


playOne :: Performance -> Instr -> TimePoint -> IO ()
playOne e i tp = do
   --Just p <- lookupMap (orc e) i
   ts <- currentTS (clock e)
   cb <- currentBeat (clock e)
   let toBePlayed = ((start (tp))/(beatInMsr ts)) + (thisBar cb)
   if (toBePlayed > cb)
      then do toWait <- timeAtBeat (clock e) toBePlayed
              waitUntil (clock e) (toWait)
              playOne e i tp
      else do playInstr i
              updatePfields i
              return ()

playNow :: Performance -> String -> IO ()
playNow e i = do
  tp <- beatInBar (clock e)
  Just p <- lookupMap (orc e) i
  playOne e p (pure tp)

play :: Performance -> String -> IO ()
play e pn = let
  checkStatus i Stopped  = ( forkIO $ playLoop e pn $ Stopped)  >> return ()
  checkStatus i Stopping = ( playLoop e pn $ Stopping) >> return ()
  checkStatus i Playing  = putStrLn $ "the instrument " ++ pn ++ " is already playing!"
  in do Just i <- lookupMap (orc e) pn
        checkStatus i $ status i

-- play loop callBack
playLoop :: Performance -> String -> Status -> IO ()

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
              let nb = nextBeat tp timeString
              let nextToPlay | (start nb) > (start tp) = ( (start  (wrapBar ts nb))/(beatInMsr ts)) + (thisBar cb) + ((fromIntegral $ floor $ (start  nb)/(beatInMsr ts)) - (fromIntegral $ floor $ (start  tp)/(beatInMsr ts)))
                             | (start nb) <= (start tp) = ((start nb)/(beatInMsr ts)) + (nextBar cb)
              nextTime <- timeAtBeat (clock e) nextToPlay
              forkIO $ playOne e p (wrapBar ts tp)
              updateToPlay e pn (Just nb)
              Just ins <- lookupMap (orc e) pn
              let toWait = nextTime - now
              waitT (toWait)
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

stop :: Performance -> String -> IO ()
stop e p = changeStatus e p Stopping

--- default Patterns ----------------------------------------

downB = [(TP 1),(TP 3)]

upFour = [(TP 0.5),(TP 1.5),(TP 2.5),(TP 3.5)]

fourFloor = [(TP 0),(TP 1),(TP 2),(TP 3)]

eightN = [(TP 0),(TP 0.5),(TP 1),(TP 1.5),(TP 2),(TP 2.5),(TP 3),(TP 3.5)]

sixteenN = [(TP 0),(TP 0.25),(TP 0.5),(TP 0.75),(TP 1),(TP 1.25),(TP 1.5),(TP 1.75),
            (TP 2),(TP 2.25),(TP 2.5),(TP 2.75),(TP 3),(TP 3.25),(TP 3.5),(TP 3.75)]

defaultTPMap :: IO (TVar (M.Map [Char] [TimePoint]))
defaultTPMap = do
  tpMap <- newTVarIO $ M.fromList [("upFour", upFour),("downB", downB),("eightN",eightN),("sixteenN",sixteenN),("fourFloor",fourFloor)]
  return $ tpMap

updateInstrument :: Performance -> String -> (Instr -> Instr) -> IO ()
updateInstrument e k f = do
  Just i <- lookupMap (orc e) k
  addToMap (orc e) (k, f i)
--
updatePfields :: Instr -> IO ()
updatePfields i = do
   pfields <- readTVarIO (pf i)
   pfpats <- readTVarIO (pats i)
   mapM_ (updateonepfield (pf i)) (M.elems pfpats)


updateonepfield :: TVar PfMap -> PfPat -> IO ()
updateonepfield pfmap pats = do
  Just pf <- lookupMap pfmap (pfNum pats)
  newVal <- (updater pats) pats
  addToMap  pfmap ((pfNum pats),newVal)



changeStatus :: Performance -> String -> Status -> IO ()
changeStatus e k newS = updateInstrument e k (\x -> x { status = newS })

changeTimeF :: Performance -> String -> String -> IO ()
changeTimeF e k newF = do
  Just pl <- lookupMap (orc e) k
  Just ts <- lookupMap (timePs e) newF
  updateToPlay e k (Just $ head ts)
  updateInstrument e k (\x -> x { timeF = newF })

updateToPlay :: Performance -> String -> Maybe TimePoint -> IO ()
updateToPlay e k newTP = updateInstrument e k (\x -> x { toPlay = newTP })

nextBeat :: TimePoint -> [TimePoint] -> TimePoint
nextBeat b xs | filter (b <) xs == [] = head xs
              | otherwise = head $ filter (b <) xs

addTPf :: Performance -> String -> [TimePoint] -> IO ()
addTPf e n ts = addToMap (timePs e) (n,ts)

stopAll e = (mapM_ (stop e)) . M.keys =<< readTVarIO (orc e)
