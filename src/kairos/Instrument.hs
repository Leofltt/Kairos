module Kairos.Instrument where

import Kairos.Base
import Control.Concurrent
import Control.Concurrent.STM
import qualified Data.Map.Strict as M
import Data.List

-- the play function plays an instrument given a clock
-- play :: Clock -> IOI -> Instr  -> IO()

type Orchestra = TVar (M.Map [Char] Instr)

data Instr = I { insN :: Double, pf :: TVar (M.Map Int Pfield) } 

data Pfield = Pd Double | Ps String deriving (Show, Eq)

pfToString :: [Pfield] -> String
pfToString ps = filter (`notElem` "Ps") $ filter (`notElem` "Pd") $ concat $ map (show) ps   
--make functor,applicative,monad instance 

-- default instruments

hihat :: IO Instr 
hihat = do
  pfields <- newTVarIO $ M.singleton 3 (Pd 0.3) -- closed/open (0<=0.5<=1)
  return $ I { insN = 5
             , pf = pfields
             }

reverb :: IO Instr
reverb = do
  pfields <- newTVarIO $ M.fromList [(3, Pd (-1)),(4, Pd 0.7),(5, Pd 15000)] -- duration, feedback, cutoff freq
  return $ I { insN = 666.1 -- the .1 is so that there is only one instance of reverb at any moment
             , pf = pfields
             }

