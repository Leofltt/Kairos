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
             , toPlay = Just (TP 1 1.5)
             , status = Stopped
             , timeF  = "downB"
             , waitTime = 0
             }

reverb :: IO Instr
reverb = do
  pfields <- newTVarIO $ M.fromList [(3,Pd (-1)),(4,Pd 0.7),(5,Pd 15000)] -- duration, feedback, cutoff freq
  return $ I { insN   = 666 -- the .1 is so that there is only one instance of reverb at any moment
             , pf     = pfields
             , toPlay = Nothing
             , status = Stopped
             , timeF  = ""
             , waitTime = 0
             }

kick :: IO Instr
kick = do
  pfields <- newTVarIO $ M.fromList  [(3,Pd 1),(4,Pd 1),(5,Ps "/Users/leofltt/Desktop/Kick-909.aif")]
  return $ I { insN   = 1
             , pf     = pfields
             , toPlay = Just (TP 0 0.5)
             , status = Stopped
             , timeF  = "fourFloor"
             , waitTime = 0
             }

sampler :: String -> IO Instr
sampler path = do
  pfields <- newTVarIO $ M.fromList [(3,Pd 1),(4,Pd 1),(5,Ps path)] -- p5 : Sample path
  return $ I { insN   = 1
             , pf     = pfields
             , toPlay = Nothing
             , status = Stopped
             , timeF  = ""
             , waitTime = 0
             }

-- default Orchestra

defaultOrc :: IO Orchestra
defaultOrc = do
  chh <- hihat 0.2
  ohh <- hihat 0.8
  k   <- kick
  r   <- reverb
  orc <-  atomically $ newTVar $ M.fromList [("K808",k),("oh808",ohh),("ch808",chh),("rev",r)]
  return $ orc
