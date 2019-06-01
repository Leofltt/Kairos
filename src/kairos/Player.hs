module Kairos.Player where

import Kairos.Base
import Kairos.TimePoint
import Kairos.Clock
import Kairos.Instrument
import Kairos.Network
import Kairos.Utilities
import Control.Concurrent
import Control.Concurrent.STM
import Data.Maybe
import qualified Data.Map.Strict as M

defaultPerformance :: IO Performance
defaultPerformance = do
  o <- defaultOrc
  c <- defaultClock
  t <- defaultTPMap
  return $ P { orc = o
             , clock = c
             , timePs = t
             }


setChannel :: String -> Double -> IO ()
setChannel chanName val = do
  let m = chanName ++ " " ++ (show val)
  setChan m


playInstr :: Instr -> IO ()
playInstr instr = do
  pfields <- readTVarIO $ pf instr
  let pfs = M.elems pfields
  let pfieldList = pfToString pfs
  let pfds = "i" ++ (show (insN instr)) ++ " 0 " ++ pfieldList
  sendEvent pfds

playOne :: Performance -> Instr -> TimePoint -> IO ()
playOne perf i tp = do
   ts <- currentTS (clock perf)
   cb <- currentBeat (clock perf)
   now <- timeD (clock perf)
   let toBePlayed = ((start (tp))/(beatInMsr ts)) + (thisBar cb)
   if (toBePlayed > cb)
      then do nextT <- (timeAtBeat (clock perf) toBePlayed)
              let toWait = (realToFrac $ floor ((nextT - now) * 10000))/ 10000
              waitT (toWait)
              playOne perf i tp
      else do playInstr i
              updatePfields i
              return ()

playNow :: Performance -> String -> IO ()
playNow perf i = do
  tp <- beatInBar (clock perf)
  Just p <- lookupMap (orc perf) i
  playOne perf p (pure tp)

playEffect = playNow

play :: Performance -> String -> IO ()
play perf pn = let
  checkStatus i Stopped  = ( forkIO $ playLoop perf pn $ Stopped)  >> return ()
  checkStatus i Stopping = ( playLoop perf pn $ Stopping) >> return ()
  checkStatus i Init = ( playLoop perf pn $ Init) >> return ()
  checkStatus i Playing  = putStrLn $ "the instrument " ++ pn ++ " is already playing!"
  in do Just i <- lookupMap (orc perf) pn
        checkStatus i $ status i

-- play loop callBack
playLoop :: Performance -> String -> Status -> IO ()

playLoop perf pn Playing = do
  Just p <- lookupMap (orc perf) pn
  now <- timeD (clock perf)
  cb <- currentBeat (clock perf)
  ts <- currentTS (clock perf)
  let pb = toPlay p
  if ((pb == Nothing) || ((timeF p )== ""))
     then do  changeStatus perf pn Stopping
              Just p' <- lookupMap (orc perf) pn
              playLoop perf pn $ status p'
     else do  let tp = fromJust pb
              Just timeString <- lookupMap (timePs perf) (timeF p)
              let nb = nextBeat tp timeString
              let nextToPlay | (start nb) > (start tp) = ( (start  (wrapBar ts nb))/(beatInMsr ts)) + (thisBar cb) + ((fromIntegral $ floor $ (start  nb)/(beatInMsr ts)) - (fromIntegral $ floor $ (start  tp)/(beatInMsr ts)))
                             | (start nb) <= (start tp) = ((start nb)/(beatInMsr ts)) + (nextBar cb)
              nextTime <- timeAtBeat (clock perf) nextToPlay
              forkIO $ playOne perf p (wrapBar ts tp)
              updateToPlay perf pn (Just nb)
              let toWait = (realToFrac $ floor ((nextTime - now) * 10000))/ 10000
              waitT (toWait)
              Just p' <- lookupMap (orc perf) pn
              playLoop perf pn $ status p'


playLoop perf p Stopped = do
  changeStatus perf p Init
  playLoop perf p Init

playLoop perf i Init = do
  Just p <- lookupMap (orc perf) i
  n <- beatInBar (clock perf)
  let pb = toPlay p
  if ((pb == Nothing) || ((timeF p )== ""))
     then do  changeStatus perf i Stopping
              Just p' <- lookupMap (orc perf) i
              playLoop perf i $ status p'
     else do  let tp = fromJust pb
              Just timeString <- lookupMap (timePs perf) (timeF p)
              let nb = nextBeat (max tp (TP n) ) timeString
              updateToPlay perf i (Just nb)
              changeStatus perf i Playing
              Just p' <- lookupMap (orc perf) i
              playLoop perf i $ status p'

playLoop perf p Stopping = do
  changeStatus perf p Stopped
  putStrLn $ "instrument " ++ p ++ " has been stopped."
  return ()

stop :: Performance -> String -> IO ()
stop perf i = do
  changeStatus perf i Stopping


stopAll perf = (mapM_ (stop perf)) .  notEffect . M.keys =<< readTVarIO (orc perf)

playAll perf = (mapM_ (play perf)) .  notEffect . M.keys =<< readTVarIO (orc perf)

soloIns perf i = (mapM_ (stop perf)) . filter (/=i) . notEffect . M.keys =<< readTVarIO (orc perf)

notEffect = filter (/= "rev") . filter (/= "del")

--- default Patterns ----------------------------------------

downB = [(TP 1),(TP 3)]

dbk1 = toTP $ [0,2.5]

upFour = toTP $ takeWhile (< 4) [0.5,1.5..]

fourFloor = toTP $ takeWhile (< 4) [0,1..]

eightN = toTP $ takeWhile (< 4) [0,0.5..]

sixteenN = toTP $ takeWhile (< 4) [0,0.25..]

dubb = toTP $ [2.25,2.75]

jGhost = toTP [1.75,2.25,5.75]

jGhost1 = toTP [1.75,2.25,5.75,6.25,7.75]

sixBar = toTP $ map (*4) [1/24, 5,24 .. 21/24]

displayTPat :: Performance -> IO String
displayTPat perf = do
   tpats <- readTVarIO (timePs perf)
   return $ unwords $ interleave ( M.keys tpats)  ( map show (map fromTP (M.elems tpats)))



defaultTPMap :: IO (TVar (M.Map [Char] [TimePoint]))
defaultTPMap = do
  tpMap <- newTVarIO $ M.fromList [("upFour", upFour),("downB", downB),("eightN",eightN)
                                  ,("sixteenN",sixteenN),("fourFloor",fourFloor),("dbk1",dbk1)
                                  ,("jGhost1",jGhost1),("jGhost",jGhost),("dubb",dubb), ("sixBar", sixBar)
                                  ]
  return $ tpMap

updateInstrument :: Performance -> String -> (Instr -> Instr) -> IO ()
updateInstrument perf k f = do
  Just i <- lookupMap (orc perf) k
  addToMap (orc perf) (k, f i)

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
  val <- closertoNow e k ts
  updateToPlay e k (Just val)
  updateInstrument e k (\x -> x { timeF = newF })

closertoNow :: Performance -> String -> [TimePoint] -> IO TimePoint
closertoNow e k ts = do
  Just pl <- lookupMap (orc e) k
  if ((toPlay pl) /= Nothing)
    then do let toP = fromJust (toPlay pl)
            let tp | (takeWhile (<= (toP)) ts) /= [] = last $ takeWhile (<= (toP)) ts
                   | otherwise = head ts
            return $ tp
    else do now <- beatInBar (clock e)
            let tp | (takeWhile (<= (TP now)) ts) /= [] = last $ takeWhile (<= (TP now)) ts
                   | otherwise = head ts
            return $ tp

updateToPlay :: Performance -> String -> Maybe TimePoint -> IO ()
updateToPlay e k newTP = updateInstrument e k (\x -> x { toPlay = newTP })


nextBeat :: TimePoint -> [TimePoint] -> TimePoint
nextBeat b xs | filter (b <) xs == [] = head xs
              | otherwise = head $ filter (b <) xs


addTPf :: Performance -> String -> [TimePoint] -> IO ()
addTPf e n ts = addToMap (timePs e) (n,ts)
