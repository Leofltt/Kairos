module Kairos.Clock where

import Control.Concurrent
import Control.Concurrent.STM
import Data.Time.Clock.POSIX
import Kairos.Base

getCurrTime :: IO Double
getCurrTime = do x <- getPOSIXTime; return $ realToFrac x


