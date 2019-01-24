module Kairos.Clock where

import Control.Concurrent
import Control.Concurrent.STM
import Data.Time.Clock.POSIX
import Kairos.Base

getNow :: IO Double
getNow = do x <- getPOSIXTime; return $ realToFrac x

timeElapsed :: Clock -> IO Double
timeElapsed clock = let
  s = startAt clock in
  do 
  x <- getNow
  return (x - s)

defaultClock :: IO Clock
defaultClock  = do
  s <- getNow
  let timesig = TS { bpm = 120
                   ,  beat = 4
                   ,  startBar = 0 
                   }
  return $ Clock { startAt = s
                 , timeSig = [timesig] 
                 }


