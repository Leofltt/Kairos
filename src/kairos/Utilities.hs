module Kairos.Utilities where

import Kairos.Base
import Control.Concurrent
import Control.Concurrent.STM
import qualified Data.Map.Strict as M
import System.Random (getStdRandom,randomR)

-- Map Utilities

modifyMap :: TVar a -> (a -> a) -> IO ()
modifyMap theMap f = atomically $ modifyTVar' theMap f

addToMap :: Ord k => TVar (M.Map k a) -> (k,a) -> IO ()
addToMap theMap (k,v) = modifyMap theMap $ M.insert k v

withMap :: TVar a -> (a -> b) -> IO b
withMap theMap f = do
  toMap <- readTVarIO theMap
  return $ f toMap

lookupMap :: Ord k => TVar (M.Map k a) -> k -> IO (Maybe a)
lookupMap theMap k = withMap theMap $ M.lookup k

-- PfPat Updaters

keep ::  PfPat -> IO Pfield
keep n = do
  pats <- readTVarIO (pat n)
  return $ head pats

nextVal :: PfPat -> IO Pfield
nextVal n = do
  patrn <- readTVarIO (pat n)
  let pat' = (tail patrn)++[head patrn]
  atomically $ writeTVar (pat n) pat'
  return $ head pat'

retrograde :: PfPat -> IO Pfield
retrograde n = do
  patrn <- readTVarIO (pat n)
  let pat' = (last patrn):(init patrn)
  atomically $ writeTVar (pat n) pat'
  return $ head pat'

randomize :: PfPat -> IO Pfield
randomize n = do
  p <- readTVarIO (pat n)
  let l = (length p) - 1
  ran <- randI l
  return $ (!!) p ran

randI :: Int -> IO Int
randI i = getStdRandom $ randomR (0, i)
