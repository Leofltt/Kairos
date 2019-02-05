module Kairos.Instrument where

import Control.Concurrent
import Control.Concurrent.STM
import qualified Data.Map.Strict as M
import Data.List
import Data.Typeable

type Orchestra = TVar (M.Map [Char] Instr)

data Instr = I { insN :: Double, pf :: TVar (M.Map Int Pfield) } 

data Pfield  = Ps { pString :: String } | Pd { pDouble :: Double } deriving (Eq, Ord, Typeable)

instance Show Pfield where
  show (Ps s) = show s
  show (Pd d) = show d

pfToString :: [(Pfield)] -> String   
pfToString ps = unwords $ map (show) ps

getPfields :: Instr -> IO (M.Map Int Pfield)
getPfields i = do
  pf <- atomically $ readTVar $ pf i
  return $ pf

--updatePfield :: Instr -> Int -> a -> IO (M.Map Int Pfield) 
--updatePfield i key newValue = let f _  = Just $ toPf newValue in do 
--  pf <- getPfields i
--  return $ M.update key pf

-- default instruments 

hihat :: Double -> IO Instr 
hihat oc = do
  pfields <- newTVarIO $ M.fromList [(3,Pd 1),(5,Pd oc),(4,Pd 1)] -- p3 : closed/open (0<=0.5<=1)
  return $ I { insN = 5
             , pf = pfields
             }

reverb :: IO Instr
reverb = do
  pfields <- newTVarIO $ M.fromList [(3,Pd (-1)),(4,Pd 0.7),(5,Pd 15000)] -- duration, feedback, cutoff freq
  return $ I { insN = 666.1 -- the .1 is so that there is only one instance of reverb at any moment
             , pf = pfields
             }

kick :: IO Instr
kick = do
  pfields <- newTVarIO $ M.fromList  [(3,Pd 1),(4,Pd 1),(5,Ps "/Users/leofltt/Desktop/Kick-909.aif")]
  return $ I { insN = 1 
             , pf = pfields
             }

sampler :: String -> IO Instr
sampler path = do
  pfields <- newTVarIO $ M.fromList [(3,Pd 1),(4,Pd 1),(5,Ps path)] -- p5 : Sample path
  return $ I { insN = 1
             , pf = pfields
             }

-- default Orchestra
defaultOrc :: IO Orchestra
defaultOrc = do
  chh <- hihat 0.2
  ohh <- hihat 0.8
  k   <- kick
  r   <- reverb
  orc <-  atomically $ newTVar $ M.fromList [("K808",k),("ohh808",ohh),("chh808",chh),("Reverb",r)] 
  return $ orc
