module Kairos.Instrument where

import Kairos.Base
import Kairos.MapUtilities
import Control.Concurrent
import Control.Concurrent.STM
import qualified Data.Map.Strict as M

pfToString :: [Pfield] -> String
pfToString ps = unwords $ map show ps

getPfields :: Instr -> IO (PfMap)
getPfields i = do
  pf <- readTVarIO $ pf i
  return $ pf

toPfD :: [Double] -> [Pfield]
toPfD (x:xs) = (Pd x) : toPfD xs
toPfD []     = []

toPfS :: [String] -> [Pfield]
toPfS (x:xs) = (Ps x) : toPfS xs
toPfS []     = []

-- default instruments

hihat :: Double -> IO Instr
hihat oc = do
  pfields <- newTVarIO $ M.fromList [(3,Pd 1),(4,Pd 1),(5,Pd 0),(6,Pd oc)]
  emptyPat <- newTVarIO M.empty
  return $ I { insN = 5
             , pf     = pfields
             , toPlay = Just (TP 0)
             , status = Stopped
             , timeF = "upFour"
             , pats = emptyPat
             }

reverb :: IO Instr
reverb = do
  pfields <- newTVarIO $ M.fromList [(3,Pd (-1)),(4,Pd 1),(5,Pd 0.7),(6,Pd 15000)] -- duration, volume, feedback, cutoff freq
  emptyPat <- newTVarIO M.empty
  return $ I { insN   = 666 -- the .1 is so that there is only one instance of reverb at any moment
             , pf     = pfields
             , toPlay = Nothing
             , status = Stopped
             , timeF = "empty"
             , pats = emptyPat
             }

kick909 :: IO Instr
kick909 = do
  pfields <- newTVarIO $ M.fromList  [(3,Pd 1),(4,Pd 1),(5,Pd 0),(6,Ps "/Users/leofltt/Desktop/KairosSamples/909/Kick-909.aif"),(7,Pd 1)]
  emptyPat <- newTVarIO M.empty
  return $ I { insN   = 1
             , pf     = pfields
             , toPlay = Just (TP 0)
             , status = Stopped
             , timeF = "fourFloor"
             , pats = emptyPat
             }

sampler :: String -> IO Instr
sampler path = do
  pfields <- newTVarIO $ M.fromList [(3,Pd 1),(4,Pd 1),(5,Pd 0),(6,Ps path),(7,Pd 1)] -- p5 : Sample path
  emptyPat <- newTVarIO M.empty
  return $ I { insN   = 1
             , pf     = pfields
             , toPlay = Nothing
             , status = Stopped
             , timeF = "empty"
             , pats = emptyPat
             }

acidBass :: IO Instr
acidBass = do
  pfields <- newTVarIO $ M.fromList  [(3,Pd 1),(4,Pd 1),(5,Pd 0),(6,Pd 303),(7,Pd 16000)]
  emptyPat <- newTVarIO M.empty
  return $ I { insN   = 3
             , pf     = pfields
             , toPlay = Just (TP 1.75)
             , status = Stopped
             , timeF = "empty"
             , pats = emptyPat
             }
clap909 :: IO Instr
clap909 = do
  pfields <- newTVarIO $ M.fromList  [(3,Pd 1),(4,Pd 1),(5,Pd 0),(6,Ps "/Users/leofltt/Desktop/KairosSamples/909/Clap-909.aif"),(7,Pd 1)]
  emptyPat <- newTVarIO M.empty
  return $ I { insN   = 1
             , pf     = pfields
             , toPlay = Just (TP 0)
             , status = Stopped
             , timeF = "downB"
             , pats = emptyPat
             }

jsn1 :: IO Instr
jsn1 = do
  pfields <- newTVarIO $ M.fromList  [(3,Pd 1),(4,Pd 1),(5,Pd 0),(6,Ps "/Users/leofltt/Desktop/KairosSamples/Snares/Snare4JungleMidHigh.wav"),(7,Pd 1)]
  emptyPat <- newTVarIO M.empty
  return $ I { insN   = 1
             , pf     = pfields
             , toPlay = Just (TP 0)
             , status = Stopped
             , timeF = "downB"
             , pats = emptyPat
             }


-- default Orchestra

defaultOrc :: IO Orchestra
defaultOrc = do
  chh  <- hihat 0.2
  ohh  <- hihat 0.8
  k    <- kick909
  a303 <- acidBass
  jsn1 <- jsn1
  sj2 <- sampler "/Users/leofltt/Desktop/KairosSamples/Kicks/Snare4JungleMidLow.wav"
  cp   <- clap909
  kcJ  <- sampler "/Users/leofltt/Desktop/KairosSamples/Kicks/KickCymbJungle.wav"
  orc  <- atomically $ newTVar $ M.fromList [("K909",k),("OH808",ohh),("CH808",chh),("sj1",jsn1),("CP909",cp),("kcJ",kcJ),("sj2",sj2),("303",a303)]
  return $ orc

-- default Fx Orchestra

defaultFx :: IO Orchestra
defaultFx = do
  rev <- reverb
  orc <-  atomically $ newTVar $ M.fromList [("rev",rev)]
  return $ orc

-- to add instruments
addInstrument :: Performance -> String -> Instr -> IO ()
addInstrument perf name instr = addToMap (orc perf) (name,instr)


-- function to create a PfPat
createPfPat :: Int -> [Pfield] -> (PfPat -> IO Pfield) -> IO PfPat
createPfPat num pfields updtr = do
  ptrn <- newTVarIO pfields
  return $ PfPat { pfNum = num
                 , pat = ptrn
                 , updater = updtr
                 }

-- alias for common pfields
duration = createPfPat 3
volume = createPfPat 4
revSend = createPfPat 5
samplePath = createPfPat 6

-- function to default a pattern to the value of the pfield
defaultPfpat :: Instr -> PfPat -> IO ()
defaultPfpat i pfp = do
  Just pf <- lookupMap (pf i) (pfNum pfp)
  atomically $ writeTVar (pat pfp) [pf]
  return ()

addPfPath :: Instr -> [Char] -> PfPat -> IO ()
addPfPath i name pfPat = addToMap (pats i) (name,pfPat)

addPfPath' :: Performance -> [Char] -> [Char] -> PfPat -> IO ()
addPfPath' e insname name pfPat = do
  Just i <- lookupMap (orc e) insname
  addPfPath i name pfPat

-- updaters

keep ::  PfPat -> IO Pfield
keep n = do
  pats <- readTVarIO (pat n)
  return $ head pats

nextVal :: PfPat -> IO Pfield
nextVal n = do
  patrn <- readTVarIO (pat n)
  let pat' = (tail patrn)++[head patrn]
  atomically $ writeTVar (pat n) pat'
  return $ (head pat')
