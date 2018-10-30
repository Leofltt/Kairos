-- Kairos.Base : the main kairos data types

module Kairos.Base where


data Clock = Clock { at :: Time, timeSigs :: [TimeSignature], active :: Bool } deriving (Show)

data TimeSignature = TS { beat :: Double, bar :: Int, bpm :: Double} deriving (Show)

-- Instruments have an id and pfields
data Instr = I { id :: Int, pf :: String }

-- an event is an action with a time of execution
type Event a =  (Time, a)

-- a behavior is a function from event to action
type Behavior a = Clock -> Event a

type Time = Double

type IOI = [Time]
