module Kairos.PfPat where 

import Kairos.Pfield 
import Data.Either ()
-- import Control.Concurrent.STM ( TVar )
import Kairos.Utilities ( checkPercentNext, randI )
import Control.Concurrent.STM ( atomically, readTVarIO, writeTVar, TVar )

-- | pfield Id containing the pfield number and it's name
data PfId = Either Int String deriving (Eq, Show, Ord) 

idInt :: PfId -> Int 
idInt (Either x _) = x

idString :: PfId -> String 
idString (Either _ y) = y 

-- | pattern of pfields and related update function
data PfPat = PfPat { pfId :: PfId                  -- ^ id of the pfield
                   , pat  :: TVar [Pfield]         -- ^ the string of possible values (or only value, depends on what the updater needs)
                   , updater :: PfPat -> IO Pfield -- ^ the function that decides which value to take
                   }

-- | PfPat Update runners

-- | keep the current value
keep ::  PfPat -> IO Pfield
keep n = do
  pats <- readTVarIO (pat n)
  return $ head pats

-- | next value in the list
nextVal :: PfPat -> IO Pfield
nextVal n = do
  patrn <- readTVarIO (pat n)
  let pat' = tail patrn++[head patrn]
  atomically $ writeTVar (pat n) pat'
  return $ head pat'

-- | read the list in reverse
retrograde :: PfPat -> IO Pfield
retrograde n = do
  patrn <- readTVarIO (pat n)
  let pat' = last patrn:init patrn
  atomically $ writeTVar (pat n) pat'
  return $ head pat'

-- | pick a random value from the list
randomize :: PfPat -> IO Pfield
randomize n = do
  p <- readTVarIO (pat n)
  let l = length p - 1
  ran <- randI l
  return $ (!!) p ran

-- | go to next value a certain % of times
percentNext :: Int -> PfPat -> IO Pfield
percentNext i n = do
  val <- randI 100
  p <- readTVarIO (pat n)
  let result = checkPercentNext val i p
  atomically $ writeTVar (pat n) result
  return $ head result

-- | updater aliases for ease of use

nv :: PfPat -> IO Pfield
nv = nextVal
np :: Int -> PfPat -> IO Pfield
np = percentNext
rnd :: PfPat -> IO Pfield
rnd = randomize
retro :: PfPat -> IO Pfield
retro = retrograde
k :: PfPat -> IO Pfield
k = keep

-- | auto mode: if list has one element use keep, otherwise use netxVal
a :: PfPat -> IO Pfield
a n = do 
    p <- readTVarIO (pat n)
    if length p == 1 
        then do 
            k n
        else do 
            nv n
