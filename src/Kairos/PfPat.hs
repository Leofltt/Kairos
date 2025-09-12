module Kairos.PfPat where

import Control.Concurrent.STM (TVar, atomically, readTVarIO, writeTVar)
import Kairos.Pfield (PfId, Pfield)
import Kairos.Utilities (checkPercentNext, randI)

-- | pattern of pfields and related update function
data PfPat = PfPat
  { -- | id of the pfield
    pfId :: PfId,
    -- | the string of possible values (or only value, depends on what the updater needs)
    pat :: TVar [Pfield],
    -- | the function that decides which value to take
    updater :: Updater
  }

-- | PfPat Update runners
type Updater = PfPat -> IO Pfield

-- | keep the current value
keep :: Updater
keep n = do
  pats <- readTVarIO (pat n)
  return $ head pats

-- | next value in the list
nextVal :: Updater
nextVal n = do
  patrn <- readTVarIO (pat n)
  let res = head patrn
  let pat' = tail patrn ++ [head patrn]
  atomically $ writeTVar (pat n) pat'
  return res

-- | read the list in reverse
retrograde :: Updater
retrograde n = do
  patrn <- readTVarIO (pat n)
  let res = head patrn
  let pat' = last patrn : init patrn
  atomically $ writeTVar (pat n) pat'
  return res

-- | pick a random value from the list
randomize :: Updater
randomize n = do
  p <- readTVarIO (pat n)
  let l = length p - 1
  ran <- randI l
  return $ (!!) p ran

-- | go to next value a certain % of times
percentNext :: Int -> Updater
percentNext i n = do
  val <- randI 100
  p <- readTVarIO (pat n)
  let res = head p
  let result = checkPercentNext val i p
  atomically $ writeTVar (pat n) result
  return res

-- | updater aliases for ease of use
nv :: Updater
nv = nextVal

np :: Int -> Updater
np = percentNext

rnd :: Updater
rnd = randomize

retro :: Updater
retro = retrograde

k :: Updater
k = keep

-- | auto mode: if list has one element use keep, otherwise use nextVal
aup :: Updater
aup n = do
  p <- readTVarIO (pat n)
  if length p == 1
    then do
      k n
    else do
      nv n
