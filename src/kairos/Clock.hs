module Kairos.Clock where

import Data.Time.Clock.POSIX
import Kairos.Base

getCurrTime :: IO Double
getCurrTime = do x <- getPOSIXTime; return $ realToFrac x


