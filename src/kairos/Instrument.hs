module Kairos.Instrument where

import Kairos.Base
import Control.Concurrent
import Control.Concurrent.STM
import qualified Data.Map.Strict as M
import Data.List


pfToString :: [(Pfield)] -> String
pfToString ps = unwords $ map (show) ps

getPfields :: Instr -> IO (PfMap)
getPfields i = do
  pf <- atomically $ readTVar $ pf i
  return $ pf

-- default instruments

hihat :: Double -> IO Instr
hihat oc = do
  pfields <- newTVarIO $ M.fromList [(3,Pd 1),(5,Pd oc),(4,Pd 1)] -- p3 : closed/open (0<=0.5<=1)
  return $ I { insN = 5
             , pf     = pfields
             , toPlay = Just (TP 0)
             , status = Stopped
             , timeF = "upFour"
             }

reverb :: IO Instr
reverb = do
  pfields <- newTVarIO $ M.fromList [(3,Pd (-1)),(4,Pd 0.7),(5,Pd 15000)] -- duration, feedback, cutoff freq
  return $ I { insN   = 666 -- the .1 is so that there is only one instance of reverb at any moment
             , pf     = pfields
             , toPlay = Nothing
             , status = Stopped
             , timeF = ""
             }

kick909 :: IO Instr
kick909 = do
  pfields <- newTVarIO $ M.fromList  [(3,Pd 1),(4,Pd 1),(5,Ps "/Users/leofltt/Desktop/KairosSamples/909/Kick-909.aif")]
  return $ I { insN   = 1
             , pf     = pfields
             , toPlay = Just (TP 0)
             , status = Stopped
             , timeF = "fourFloor"
             }

sampler :: String -> IO Instr
sampler path = do
  pfields <- newTVarIO $ M.fromList [(3,Pd 1),(4,Pd 1),(5,Ps path)] -- p5 : Sample path
  return $ I { insN   = 1
             , pf     = pfields
             , toPlay = Nothing
             , status = Stopped
             , timeF = ""
             }

clap909 :: IO Instr
clap909 = do
  pfields <- newTVarIO $ M.fromList  [(3,Pd 1),(4,Pd 1),(5,Ps "/Users/leofltt/Desktop/KairosSamples/909/Clap-909.aif")]
  return $ I { insN   = 1
             , pf     = pfields
             , toPlay = Just (TP 0)
             , status = Stopped
             , timeF = "downB"
             }

jsn1 :: IO Instr
jsn1 = do
  pfields <- newTVarIO $ M.fromList  [(3,Pd 1),(4,Pd 1),(5,Ps "/Users/leofltt/Desktop/KairosSamples/Snares/Snare4Jungle1.wav")]
  return $ I { insN   = 1
             , pf     = pfields
             , toPlay = Just (TP 0)
             , status = Stopped
             , timeF = "downB"
             }


-- default Orchestra

defaultOrc :: IO Orchestra
defaultOrc = do
  chh  <- hihat 0.2
  ohh  <- hihat 0.8
  k    <- kick909
  jsn1 <- jsn1
  cp   <- clap909
  kcJ  <- sampler "/Users/leofltt/Desktop/KairosSamples/Kicks/KickCymbJungle.wav"
  orc  <- atomically $ newTVar $ M.fromList [("K909",k),("OH808",ohh),("CH808",chh),("snJ1",jsn1),("CP909",cp),("kcJ",kcJ)]
  return $ orc

defaultFx :: IO Orchestra
defaultFx = do
  rev <- reverb
  orc <-  atomically $ newTVar $ M.fromList [("rev",rev)]
  return $ orc
