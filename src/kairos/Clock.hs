module Kairos.Clock where

import Control.Concurrent
import Control.Concurrent.STM
import Data.Time.Clock.POSIX
import Kairos.Base

getNow :: IO Double
getNow = do x <- getPOSIXTime; return $ realToFrac x

timeElapsed :: Clock -> IO Double
timeElapsed clock = let
  s = at clock     in
  do 
  x <- getNow
  return (x - s)

newClock :: IO Clock
newClock  = do
  s <- getNow
  return $ Clock { at = s }
