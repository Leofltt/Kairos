module Kairos.Player where

import Kairos.Performance ( Performance(..) )
import Kairos.Pfield ( PfPat(updater, pfId), pfToString, PfMap, idInt )
import Kairos.TimePoint
import Kairos.Clock
import Kairos.Instrument
import Kairos.Network ( sendEvent, setChan, sendOSC )
import Kairos.Utilities ( addToMap, lookupMap, inter, sameConstructor)
import Control.Concurrent ( forkIO )
import Control.Concurrent.STM ( newTVarIO, readTVarIO, TVar )
import Data.Maybe ( fromJust, isJust, isNothing )
import qualified Data.Map.Strict as M
import Control.Monad (when, void)

defaultPerformance :: IO Performance
defaultPerformance = do
  o <- defaultOrc
  c <- defaultClock
  t <- defaultTPMap
  return $ P { orc = o
             , clock = c
             , timePs = t
             }

setChannel :: String -> String -> Double -> IO ()
setChannel port chanName val = do
  let m = chanName ++ " " ++ show val
  setChan port m

playInstr :: Instr -> IO ()
playInstr instr = do
  pfields <- readTVarIO $ pf instr
  let pfs = M.elems pfields
  if sameConstructor (kind instr) (Csound "")
    then do
      if itype instr == Instrument 
        then do
          let pfieldList = pfToString pfs
          let pfds = "i" ++ show (insN instr) ++ " 0 " ++ pfieldList
          sendEvent (getPort (kind instr)) pfds
        else do
          -- play effect (to change effect parameters) not implemented yet
          return ()
    else do
      sendOSC (getPort (kind instr)) (insN instr) pfs

playOne :: Performance -> Instr -> TimePoint -> IO ()
playOne perf i tp = do
   ts <- currentTS (clock perf)
   cb <- currentBeat (clock perf)
   now <- timeD (clock perf)
   let toBePlayed = (whenTP tp/beatInMsr ts) + thisBar cb
   if toBePlayed > cb
      then do
              nextT <- timeAtBeat (clock perf) toBePlayed
              let toWait = realToFrac (floor ((nextT - now) * 10000))/ 10000
              waitT toWait
              playOne perf i tp
      else if cb - toBePlayed <= 0.010
           then do
                   updatePfields i
                   playInstr i
           else do
                   return ()

-- play an instrument once immediately
playNow :: Performance -> String -> IO ()
playNow perf i = do
  tp <- beatInBar (clock perf)
  Just p <- lookupMap (orc perf) i
  playOne perf p (pure tp)

playEffect :: Performance -> String -> IO ()
playEffect = playNow

-- inspired by Conductive, R. Bell https://lac.linuxaudio.org/2011/papers/35.pdf
play :: Performance -> String -> IO ()
play perf pn = let
  checkStatus i Inactive  = void ( forkIO $ playLoop perf pn Inactive)
  checkStatus i Stopping = void ( playLoop perf pn Stopping)
  checkStatus i Init = void ( playLoop perf pn Init)
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
  if timeF p== ""
     then do  changeStatus perf pn Stopping
              Just p' <- lookupMap (orc perf) pn
              playLoop perf pn $ status p'
     else do  let tp = fromJust pb
              Just timeString <- lookupMap (timePs perf) (timeF p)
              let nb = nextBeat tp timeString
              let nextToPlay | whenTP nb > whenTP tp = (whenTP  (wrapBar ts nb)/beatInMsr ts) + thisBar cb + (fromIntegral . floor $ whenTP  nb/beatInMsr ts - whenTP tp/beatInMsr ts)
                             | whenTP nb <= whenTP tp = (whenTP nb/beatInMsr ts) + nextBar cb
              nextTime <- timeAtBeat (clock perf) nextToPlay
              _ <- forkIO $ playOne perf p (wrapBar ts tp)
              updateToPlay perf pn (Just nb)
              let toWait = realToFrac (floor ((nextTime - now) * 10000))/ 10000
              waitT toWait
              Just p' <- lookupMap (orc perf) pn
              playLoop perf pn $ status p'

playLoop perf p Inactive = do
  changeStatus perf p Init
  playLoop perf p Init

playLoop perf i Init = do
  Just p <- lookupMap (orc perf) i
  n <- beatInBar (clock perf)
  let pb = toPlay p
  if isNothing pb && (timeF p== "")
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
  when (status p == Active) $ do
      changeStatus perf i Stopping
      return ()

-- stop all instruments that are not effects
stopAll :: Performance -> IO ()
stopAll perf = mapM_ (stop perf) .  notEffect . M.keys =<< readTVarIO (orc perf)

-- plays all instruments that are not effects
playAll :: Performance -> IO ()
playAll perf = mapM_ (play perf) .  notEffect . M.keys =<< readTVarIO (orc perf)

-- solo an instrument
soloIns :: Performance -> String -> IO ()
soloIns perf i = mapM_ (stop perf) . filter (/=i) . notEffect . M.keys =<< readTVarIO (orc perf)


-- display all Time Patterns names and their content
displayTPat :: Performance -> IO [String]
displayTPat perf = do
   tpats <- readTVarIO (timePs perf)
   return $ inter ( M.keys tpats)  ( map (show . fromTP) (M.elems tpats))


-- default map of named patterns of timepoints  
defaultTPMap :: IO (TVar (M.Map [Char] [TimePoint]))
defaultTPMap = do
  newTVarIO $ M.fromList [("upFour", upFour),("downB", downB),("eightN",eightN)
                                  ,("sixteenN",sixteenN),("fourFloor",fourFloor),("dbk",dbk)
                                  ,("jGhost1",jGhost1),("jGhost",jGhost),("dubb",dubb)
                                  ,("sixBar", sixBar),("uno", uno),("jgk",jgk),("irsn",irsn)
                                  ,("stdbkk",stdbkk),("stdbks",stdbks),("ir1k",ir1k),("bouncyk",bouncyk)
                                  ,("ukgch",ukgch),("ukgrs",ukgrs),("jgs",jgs),("jgk",jgk)
                                  ,("kpanb",kpanb),("kpanc",kpanc),("kpanbox",kpanbox),("b2",b2)
                                  ,("bou2",bou2),("fwk1",fwk1),("fwk2",fwk2),("adb",adb),("adk",adk)
                                  ,("dbk2",dbk2)
                                  ]

updateInstrument :: Performance -> String -> (Instr -> Instr) -> IO ()
updateInstrument perf k f = do
  Just i <- lookupMap (orc perf) k
  addToMap (orc perf) (k, f i)

updatePfields :: Instr -> IO ()
updatePfields i = do
   pfpats <- readTVarIO (pats i)
   mapM_ (updateonepfield (pf i)) (M.elems pfpats)

updateonepfield :: TVar PfMap -> PfPat -> IO ()
updateonepfield pfmap pats = do
  newVal <- updater pats pats
  addToMap  pfmap (idInt (pfId pats),newVal)

changeStatus :: Performance -> String -> Status -> IO ()
changeStatus e k newS = updateInstrument e k (\x -> x { status = newS })

changeTimeF :: Performance -> String -> String -> IO ()
changeTimeF e k newF = do
  pl <- lookupMap (orc e) k
  if isNothing pl
    then putStrLn "Instrument not found"
    else do
      tp <- lookupMap (timePs e) newF
      if isNothing  tp
        then putStrLn "Time Pattern not found"
        else do
          let Just ts = tp
          val <- closertoNow e k ts
          updateToPlay e k (Just val)
          updateInstrument e k (\x -> x { timeF = newF })

closertoNow :: Performance -> String -> [TimePoint] -> IO TimePoint
closertoNow e k ts = do
  Just pl <- lookupMap (orc e) k
  if isJust (toPlay pl)
    then do let toP = fromJust (toPlay pl)
            let tp | takeWhile (<= toP) ts /= [] = last $ takeWhile (<= toP) ts
                   | otherwise = head ts
            return tp
    else do now <- beatInBar (clock e)
            let tp | takeWhile (<= TP now) ts /= [] = last $ takeWhile (<= TP now) ts
                   | otherwise = head ts
            return tp

updateToPlay :: Performance -> String -> Maybe TimePoint -> IO ()
updateToPlay e k newTP = updateInstrument e k (\x -> x { toPlay = newTP })

nextBeat :: TimePoint -> [TimePoint] -> TimePoint
nextBeat b xs | not (any (b <) xs) = head xs
              | otherwise = head $ filter (b <) xs

