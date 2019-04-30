module Kairos.Instrument where

import Kairos.Base
import Kairos.Utilities
import Control.Concurrent
import Control.Concurrent.STM
import qualified Data.Map.Strict as M
--import System.Random (getStdRandom,randomR)

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
  pfields <- newTVarIO $ M.fromList [(3,Pd 1),(4,Pd 1),(5,Pd 0),(6, Pd 0),(7,Pd oc),(8,Pd 1)]
  emptyPat <- newTVarIO M.empty
  return $ I { insN = 5
             , pf     = pfields
             , toPlay = Just (TP 0)
             , status = Stopped
             , timeF = "upFour"
             , pats = emptyPat
             }

kick909 :: IO Instr
kick909 = do
  pfields <- newTVarIO $ M.fromList  [(3,Pd 1),(4,Pd 1),(5,Pd 0),(6, Pd 0),(7,Ps "/Users/leofltt/Desktop/KairosSamples/909/Kick-909.aif"),(8,Pd 1)]
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
  pfields <- newTVarIO $ M.fromList [(3,Pd 1),(4,Pd 1),(5,Pd 0),(6, Pd 0),(7,Ps path),(8,Pd 1)] -- p6 : Sample path, p7 : pitch
  emptyPat <- newTVarIO M.empty
  return $ I { insN   = 1
             , pf     = pfields
             , toPlay = Nothing
             , status = Stopped
             , timeF = ""
             , pats = emptyPat
             }

acidBass :: IO Instr
acidBass = do
  pfields <- newTVarIO $ M.fromList  [(3,Pd 0.7),(4,Pd 0.7),(5,Pd 0),(6, Pd 0),(7,Pd 48),(8,Pd 16000),(9,Pd 10)]
  emptyPat <- newTVarIO M.empty
  return $ I { insN   = 3
             , pf     = pfields
             , toPlay = Just (TP 0)
             , status = Stopped
             , timeF = ""
             , pats = emptyPat
             }

hoover :: IO Instr
hoover = do
  pfields <- newTVarIO $ M.fromList  [(3,Pd 1),(4,Pd 0.7),(5,Pd 0),(6, Pd 0),(7,Pd 48),(8,Pd 888),(9,Pd 5)]
  emptyPat <- newTVarIO M.empty
  return $ I { insN   = 4
             , pf     = pfields
             , toPlay = Just (TP 0)
             , status = Stopped
             , timeF = ""
             , pats = emptyPat
             }


karp :: IO Instr
karp = do
  pfields <- newTVarIO $ M.fromList  [(3,Pd 1),(4,Pd 1),(5,Pd 0),(6, Pd 0),(7,Pd 48),(8,Pd 0.2),(9,Pd 0.2)]
  emptyPat <- newTVarIO M.empty
  return $ I { insN   = 2
             , pf     = pfields
             , toPlay = Just (TP 0)
             , status = Stopped
             , timeF = ""
             , pats = emptyPat
             }


-- default effects

reverb :: IO Instr
reverb = do
  pfields <- newTVarIO $ M.fromList [(3,Pd (-1))]
  emptyPat <- newTVarIO M.empty
  return $ I { insN   = 550
             , pf     = pfields
             , toPlay = Nothing
             , status = Stopped
             , timeF = ""
             , pats = emptyPat
             }

delay :: IO Instr
delay = do
  pfields <- newTVarIO $ M.fromList [(3,Pd (-1))]
  emptyPat <- newTVarIO M.empty
  return $ I { insN   = 551
             , pf     = pfields
             , toPlay = Nothing
             , status = Stopped
             , timeF = ""
             , pats = emptyPat
             }

-- default Orchestra

defaultOrc :: IO Orchestra
defaultOrc = do
  chh  <- hihat 0.2
  ohh  <- hihat 0.8
  k    <- kick909
  a303 <- acidBass
  hov  <- hoover
  cp   <- sampler "/Users/leofltt/Desktop/KairosSamples/909/Clap-909.aif"
  karpS <- karp
  rev  <- reverb
  del  <- delay
  kcj  <- sampler "/Users/leofltt/Desktop/KairosSamples/Kicks/KickCymbJungle.wav"
  orc  <- atomically $ newTVar $ M.fromList [("K909",k),("OH808",ohh),("CH808",chh)
                                            ,("CP909",cp),("kcj",kcj)
                                            ,("303",a303),("hov",hov)
                                            ,("rev",rev),("del",del)
                                            ,("karp",karpS)
                                            ]
  return $ orc


displayInstruments :: Performance -> IO String
displayInstruments perf = do
   ins <- readTVarIO (orc perf)
   return $ unwords $ M.keys ins

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


-- function to default a pattern to the value of the pfield
defaultPfpat :: Instr -> PfPat -> IO ()
defaultPfpat i pfp = do
  Just pf <- lookupMap (pf i) (pfNum pfp)
  atomically $ writeTVar (pat pfp) [pf]
  return ()


addPfPath :: Instr -> Int -> PfPat -> IO ()
addPfPath i num pfPat = addToMap (pats i) (num,pfPat)

addPfPath' :: Performance -> [Char] -> Int -> PfPat -> IO ()
addPfPath' e insname num pfPat = do
  Just i <- lookupMap (orc e) insname
  addPfPath i num pfPat
