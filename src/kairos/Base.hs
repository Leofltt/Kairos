-- Kairos.Base : the main kairos data types

module Kairos.Base where

-- an event is some data with a time of execution
type E a =  (Time, a)

-- a pattern is a function that uses a clock to map events to action
--type P a = Clock -> [E a]

-- time represents Bars.CurrPhase (Doubles)
type Time = Double

-- list of timestamps (based on where we are in the score)
type IOI = [Double]
