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
  if (kind instr == Csound)
    then do
      let pfds = "i" ++ (show (insN instr)) ++ " 0 " ++ pfieldList
      sendEvent pfds
    else do
      let pfds = "/" ++ (show (insN instr)) ++ "/" ++ pfieldList
      sendOther pfds

playOne :: Performance -> Instr -> TimePoint -> IO ()
playOne perf i tp = do
   ts <- currentTS (clock perf)
   cb <- currentBeat (clock perf)
   now <- timeD (clock perf)
   let toBePlayed = ((start (tp))/(beatInMsr ts)) + (thisBar cb)
   if ((toBePlayed ) > cb)
      then do
              nextT <- timeAtBeat (clock perf) (toBePlayed)
              let toWait = (realToFrac $ floor ((nextT - now) * 10000))/ 10000
              waitT (toWait)
              playOne perf i tp
      else if (cb - toBePlayed <= 0.010)
           then do
                   updatePfields i
                   playInstr i
                   return ()
           else do
                   return ()

playNow :: Performance -> String -> IO ()
playNow perf i = do
  tp <- beatInBar (clock perf)
  Just p <- lookupMap (orc perf) i
  playOne perf p (pure tp)

playEffect = playNow

-- inspired by Conductive, R. Bell https://lac.linuxaudio.org/2011/papers/35.pdf
play :: Performance -> String -> IO ()
play perf pn = let
  checkStatus i Inactive  = ( forkIO $ playLoop perf pn $ Inactive)  >> return ()
  checkStatus i Stopping = ( playLoop perf pn $ Stopping) >> return ()
  checkStatus i Init = ( playLoop perf pn $ Init) >> return ()
  checkStatus i Active  = putStrLn $ "the instrument " ++ pn ++ " is already Active!"
  in do Just i <- lookupMap (orc perf) pn
        checkStatus i $ status i

-- play loop callBack
playLoop :: Performance -> String -> Status -> IO ()

playLoop perf pn Active = do
  Just p <- lookupMap (orc perf) pn
  now <- timeD (clock perf)
  cb <- currentBeat (clock perf)
  ts <- currentTS (clock perf)
  let pb = toPlay p
  if (timeF p )== ""
     then do  changeStatus perf pn Stopping
              Just p' <- lookupMap (orc perf) pn
              playLoop perf pn $ status p'
     else do  let tp = fromJust pb
              Just timeString <- lookupMap (timePs perf) (timeF p)
              let nb = nextBeat tp timeString
              let nextToPlay | (start nb) > (start tp) = ((start  (wrapBar ts nb))/(beatInMsr ts)) + (thisBar cb) + ((fromIntegral $ floor $ (start  nb)/(beatInMsr ts)) - (fromIntegral $ floor $ (start  tp)/(beatInMsr ts)))
                             | (start nb) <= (start tp) = ((start nb)/(beatInMsr ts)) + (nextBar cb)
              nextTime <- timeAtBeat (clock perf) (nextToPlay)
              forkIO $ playOne perf p (wrapBar ts tp)
              updateToPlay perf pn (Just nb)
              let toWait = (realToFrac $ floor ((nextTime - now) * 10000))/ 10000
              waitT (toWait)
              Just p' <- lookupMap (orc perf) pn
              playLoop perf pn $ status p'


playLoop perf p Inactive = do
  changeStatus perf p Init
  playLoop perf p Init

playLoop perf i Init = do
  Just p <- lookupMap (orc perf) i
  n <- beatInBar (clock perf)
  let pb = toPlay p
  if ((pb == Nothing) && ((timeF p )== ""))
     then do  changeStatus perf i Stopping
              Just p' <- lookupMap (orc perf) i
              playLoop perf i $ status p'
     else do  let tp = fromJust pb
              Just timeString <- lookupMap (timePs perf) (timeF p)
              let nb = nextBeat (max tp (TP n) ) timeString
              updateToPlay perf i (Just nb)
              changeStatus perf i Active
              Just p' <- lookupMap (orc perf) i
              playLoop perf i $ status p'

playLoop perf p Stopping = do
  changeStatus perf p Inactive
  putStrLn $ "instrument " ++ p ++ " is now Inactive."
  return ()

stop :: Performance -> String -> IO ()
stop perf i = do
  Just p <- lookupMap (orc perf) i
  --if (status p) == Active
  changeStatus perf i Stopping
  return ()


stopAll perf = (mapM_ (stop perf)) .  notEffect . M.keys =<< readTVarIO (orc perf)

playAll perf = (mapM_ (play perf)) .  notEffect . M.keys =<< readTVarIO (orc perf)

soloIns perf i = (mapM_ (stop perf)) . filter (/=i) . notEffect . M.keys =<< readTVarIO (orc perf)

notEffect = filter (/= "rev") . filter (/= "del") . filter (/= "mix") . filter ( /= "chorus")

--- default Patterns ----------------------------------------


-- a few time patterns in 4/4
downB = [(TP 1),(TP 3)]
dbk = toTP [0,2.5]
upFour = toTP $ takeWhile (< 4) [0.5,1.5..]
fourFloor = toTP $ takeWhile (< 4) [0,1..]
eightN = toTP $ takeWhile (< 4) [0,0.5..]
sixteenN = toTP $ takeWhile (< 4) [0,0.25..]
dubb = toTP [2.25,2.75]
jGhost = toTP [1.75,2.25,5.75]
jGhost1 = toTP [1.75,2.25,5.75,6.25,7.75]
sixBar = toTP $ map (*4) [1/24, 5/24 .. 21/24]
jgk = toTP [0, 0.5, 2.5, 4.5,4.75,6.5]
jgs = toTP [1,1.75,2.25,3.5,4.25,5,5.75,6.25,7.5]
ukgrs = toTP [0.25,1.75,3.25,5.75,7.25]
ukgch = toTP [0.5,0.75, 1.5, 2.5,3.5,3.75,4.5,5.5,6.5,7.5]
bouncyk = toTP [0,0.5,2,2.5,4,4.25,6,6.5]
ir1k = toTP [0,0.5,0.75,1.5,2.5,4,4.5,5.75,6.5]
stdbkk = toTP [0,0.5,1.5,2.5]
stdbks = toTP [1,1.75,2.5,3]
irsn = toTP [1,1.75,3,3.75,5,7,7.75]
uno = [TP 0]
kpanb = toTP [0, 0.75,1.5,2.5,3]
kpanc = toTP [0,0.25,1.5,1.75,2,2.25,3.5,3.75,4,4.5,4.75,5.25,5.5,6,7.5]
kpanbox = toTP [0,1.5,2,3.5,4,4.5,4.75,5.25,5.5,6,7.5]
b2 = toTP [0,0.75,1,2,2.5]
bou2 = toTP [0,1.5,2,3.5,4,5.5,6,7.75]
fwk1 = toTP [0,0.75,1.5,2,2.75,3.5]
fwk2 = toTP [0,0.75,1.25,1.75,2,2.75]
adk = toTP [0, 1, 2,2.5,3,4,5,5.5,6,7,8,8.5,9,10,11,11.5]
adb = toTP [0,1,2,3,4.5,5,6,7,8,9.5,10]

displayTPat :: Performance -> IO String
displayTPat perf = do
   tpats <- readTVarIO (timePs perf)
   return $ unwords $ inter' ( M.keys tpats)  ( map show (map fromTP (M.elems tpats)))



defaultTPMap :: IO (TVar (M.Map [Char] [TimePoint]))
defaultTPMap = do
  tpMap <- newTVarIO $ M.fromList [("upFour", upFour),("downB", downB),("eightN",eightN)
                                  ,("sixteenN",sixteenN),("fourFloor",fourFloor),("dbk",dbk)
                                  ,("jGhost1",jGhost1),("jGhost",jGhost),("dubb",dubb)
                                  ,("sixBar", sixBar),("uno", uno),("jgk",jgk),("irsn",irsn)
                                  ,("stdbkk",stdbkk),("stdbks",stdbks),("ir1k",ir1k),("bouncyk",bouncyk)
                                  ,("ukgch",ukgch),("ukgrs",ukgrs),("jgs",jgs),("jgk",jgk)
                                  ,("kpanb",kpanb),("kpanc",kpanc),("kpanbox",kpanbox),("b2",b2)
                                  ,("bou2",bou2),("fwk1",fwk1),("fwk2",fwk2),("adb",adb),("adk",adk)
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
