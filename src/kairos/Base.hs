-- Kairos.Base : the main kairos data types

module Kairos.Base where


data Clock = Clock { at :: Time, timeSigs :: [TimeSignature], active :: Bool } deriving (Show)

data TimeSignature = TS { beat :: Double, bar :: Int, bpm :: Double} deriving (Show)

data Instr = I { id :: Int, pf :: String, iOI :: [IOI] }

-- an event is an action with a time of execution
type Event a =  (Time, a)

-- a behavior is a function from event to actionx
type Behavior a = Clock -> Event a

type Time = Double

type IOI = [Time]
