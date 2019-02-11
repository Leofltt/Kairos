module Kairos.Transport where

import Kairos.Clock
import Control.Concurrent
import Control.Monad
import Control.Monad.IO.Class
import Data.Word
import Data.Time.Clock.POSIX

-- timeStamps
type NTPTime = Double

subito :: NTPTime
subito = 1 / 2^(32::Int)

epochDiffUTNTP :: Num a => a
epochDiffUTNTP = (70 * 365 + 17) * 24 * 3600

nTPTtoPOSIX :: Num a => a -> a
nTPTtoPOSIX = (+) (negate epochDiffUTNTP)

timeToNTPTime :: Num a => a -> a
timeToNTPTime = (+) epochDiffUTNTP

ntpTimeToPOSIX :: NTPTime -> POSIXTime
ntpTimeToPOSIX = realToFrac . nTPTtoPOSIX

--ntpToTime :: NTP64 -> Time
--ntpToTime = nTPTtoPOSIX . wordToDoubleNTP

posixToNTPTime :: POSIXTime -> NTPTime
posixToNTPTime = timeToNTPTime . realToFrac

time :: MonadIO m => m NTPTime
time = liftIO $ fmap posixToNTPTime getPOSIXTime

-- thread ops

pauseLimit  :: Fractional a => a
pauseLimit = fromIntegral (maxBound :: Int) /1e6

pauseT :: (MonadIO m, RealFrac a) => a -> m ()
pauseT t = when (t > 0) (liftIO (threadDelay (floor (t * 1e6))))

pauseTuntil :: MonadIO m => NTPTime -> m ()
pauseTuntil t = pauseT . (t -) =<< time

sleepT :: (RealFrac a, MonadIO m) => a -> m ()
sleepT t =
  if t >= pauseLimit
  then let t' = pauseLimit - 1
       in pauseT t >> sleepT (t - t')
  else pauseT t

sleepTuntil :: MonadIO m => NTPTime -> m ()
sleepTuntil t = sleepT . (t -) =<< time
