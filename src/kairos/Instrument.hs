{-# LANGUAGE DeriveDataTypeable #-}

module Kairos.Instrument where

import Kairos.TimePoint ( TimePoint )
import Kairos.Pfield ( PfMap, Pfield(Ps, Pd), idInt, PfId, pfIdInt )
import Kairos.Utilities ( lookupMap )
import Kairos.PfPat ( PfPat(pfId, pat) )
import Kairos.Network (UDPPort)
import Control.Concurrent.STM
    ( atomically, newTVarIO, readTVarIO, writeTVar, TVar )
import qualified Data.Map.Strict as M
import Data.Data ( Data, Typeable )
import Data.Either

-- | Orchestra : a map of instruments and their names
type Orchestra = TVar (M.Map [Char] Instr)

-- | an Instr is a "player" in the Orchestra it may be either an instrument or an effect
data Instr = I { insN :: InstrumentID           -- ^ Instrument ID : Int
               , pf :: TVar PfMap               -- ^ Pfields and their id int
               , status :: Status               -- ^ is the instrument playing ?
               , toPlay :: Maybe TimePoint      -- ^ when to play the instrument next
               , pats :: TVar (M.Map Int PfPat) -- ^ Patterns of Parameters and their IDs
               , timeF :: String                -- ^ Name of the time function to refer to
               , kind :: MessageTo UDPPort      -- ^ where to send the message (Csound or OSC)
               , itype :: InstrType             -- ^ Instrument or Effect ?
               }

-- | Instrument Name: an integer number
type InstrumentID = Int

-- | is the instrument Active ?
data Status = Init | Active | Inactive | Stopping deriving (Show, Eq)

-- | where are we sending the data
data MessageTo a = Csound a | OSC a deriving (Show, Eq, Typeable, Data)

getPort :: MessageTo String -> String 
getPort (Csound x) = x 
getPort (OSC x) = x

-- | instrument or effect ?
data InstrType = Instrument | Effect deriving (Show, Eq)

getPfields :: Instr -> IO PfMap
getPfields i = do
  readTVarIO $ pf i

-- default instruments

hihat :: Double -> IO Instr
hihat oc = do
  pfields <- newTVarIO $ pfFromList [(pfIdInt 3,Pd 1),(pfIdInt 4,Pd 1),(pfIdInt 5,Pd 0),(pfIdInt 6, Pd 0),(pfIdInt 7,Pd 0.5),(pfIdInt 8,Pd oc),(pfIdInt 9,Pd oc),(pfIdInt 10,Pd 1)]
  emptyPat <- newTVarIO M.empty
  return $ I { insN   = 5
             , pf     = pfields
             , toPlay = Nothing
             , status = Inactive
             , timeF  = ""
             , pats   = emptyPat
             , kind   = Csound "11000"
             , itype  = Instrument
             }

sampler :: String -> IO Instr
sampler path = do
  pfields <- newTVarIO $ pfFromList [(pfIdInt 3,Pd 1),(pfIdInt 4,Pd 1)
                                    ,(pfIdInt 5,Pd 0),(pfIdInt 6, Pd 0)
                                    ,(pfIdInt 7,Pd 0.5),(pfIdInt 8, Pd 0)
                                    ,(pfIdInt 9,Ps path),(pfIdInt 10,Pd 1),(pfIdInt 11,Pd 0.9),(pfIdInt 12,Pd 2)] -- p9 : Sample path, p10 : pitch
  emptyPat <- newTVarIO M.empty
  return $ I { insN   = 1
             , pf     = pfields
             , toPlay = Nothing
             , status = Inactive
             , timeF  = ""
             , pats   = emptyPat
             , kind   = Csound "11000"
             , itype  = Instrument
             }

acidBass :: IO Instr
acidBass = do
  pfields <- newTVarIO $ pfFromList  [(pfIdInt 3,Pd 0.7),(pfIdInt 4,Pd 0.7),(pfIdInt 5,Pd 0),(pfIdInt 6, Pd 0),(pfIdInt 7,Pd 0.5),(pfIdInt 8, Pd 0),(pfIdInt 9,Pd 48),(pfIdInt 10,Pd 14000),(pfIdInt 11,Pd 9),(pfIdInt 12, Pd 0)]
  emptyPat <- newTVarIO M.empty
  return $ I { insN   = 3
             , pf     = pfields
             , toPlay = Nothing
             , status = Inactive
             , timeF  = ""
             , pats   = emptyPat
             , kind   = Csound "11000"
             , itype  = Instrument
             }

hoover :: IO Instr
hoover = do
  pfields <- newTVarIO $ pfFromList  [(pfIdInt 3,Pd 1),(pfIdInt 4,Pd 0.7),(pfIdInt 5,Pd 0),(pfIdInt 6, Pd 0),(pfIdInt 7,Pd 0.5),(pfIdInt 8, Pd 0),(pfIdInt 9,Pd 48),(pfIdInt 10,Pd 888),(pfIdInt 11,Pd 5),(pfIdInt 12, Pd 0.2)]
  emptyPat <- newTVarIO M.empty
  return $ I { insN   = 4
             , pf     = pfields
             , toPlay = Nothing
             , status = Inactive
             , timeF  = ""
             , pats   = emptyPat
             , kind   = Csound "11000"
             , itype  = Instrument
             }

karp :: IO Instr
karp = do
  pfields <- newTVarIO $ pfFromList [(pfIdInt 3,Pd 1),(pfIdInt 4,Pd 0.5),(pfIdInt 5,Pd 0),(pfIdInt 6, Pd 0),(pfIdInt 7,Pd 0.5),(pfIdInt 8, Pd 0),(pfIdInt 9,Pd 48),(pfIdInt 10,Pd 0.1),(pfIdInt 11,Pd 0.1)]
  emptyPat <- newTVarIO M.empty
  return $ I { insN   = 9
             , pf     = pfields
             , toPlay = Nothing
             , status = Inactive
             , timeF  = ""
             , pats   = emptyPat
             , kind   = Csound "11000"
             , itype  = Instrument
             }

fmSub :: IO Instr
fmSub = do
  pfields <- newTVarIO $ pfFromList [(pfIdInt 3, Pd 1), (pfIdInt 4, Pd 1), (pfIdInt 5, Pd 0), (pfIdInt 6, Pd 0), (pfIdInt 7,Pd 0.5),(pfIdInt 8, Pd 0),(pfIdInt 9, Pd 60),(pfIdInt 10, Pd 20000)
                                    ,(pfIdInt 11, Pd 2), (pfIdInt 12, Pd 0.2), (pfIdInt 13, Pd 1), (pfIdInt 14, Pd 2000), (pfIdInt 15, Pd 2.45) ]
  emptyPat <- newTVarIO M.empty
  return $ I { insN   = 6
             , pf     = pfields
             , toPlay = Nothing
             , status = Inactive
             , timeF  = ""
             , pats   = emptyPat
             , kind   = Csound "11000"
             , itype  = Instrument
             }

superSaw :: IO Instr
superSaw = do
  pfields <- newTVarIO $ pfFromList [(pfIdInt 3, Pd 1), (pfIdInt 4, Pd 0.8), (pfIdInt 5, Pd 0), (pfIdInt 6, Pd 0), (pfIdInt 7,Pd 0.5),(pfIdInt 8, Pd 0),(pfIdInt 9, Pd 60),(pfIdInt 10, Pd 5000)
                                    ,(pfIdInt 11, Pd 2),(pfIdInt 12, Pd 0.2), (pfIdInt 13, Pd 0.3), (pfIdInt 14, Pd 0.5)]
  emptyPat <- newTVarIO M.empty
  return $ I { insN   = 7
             , pf     = pfields
             , toPlay = Nothing
             , status = Inactive
             , timeF  = ""
             , pats   = emptyPat
             , kind   = Csound "11000"
             , itype  = Instrument
             }

stringPad :: IO Instr
stringPad = do
  pfields <- newTVarIO $ pfFromList [(pfIdInt 3, Pd 1), (pfIdInt 4, Pd 1), (pfIdInt 5, Pd 0), (pfIdInt 6, Pd 0), (pfIdInt 7,Pd 0.5),(pfIdInt 8, Pd 0),(pfIdInt 9, Pd 60)]
  emptyPat <- newTVarIO M.empty
  return $ I { insN   = 8
             , pf     = pfields
             , toPlay = Nothing
             , status = Inactive
             , timeF  = ""
             , pats   = emptyPat
             , kind   = Csound "11000"
             , itype  = Instrument
             }

stutter :: String -> IO Instr
stutter path = do
  pfields <- newTVarIO $ pfFromList [(pfIdInt 3,Pd 1),(pfIdInt 4,Pd 1)
                                    ,(pfIdInt 5,Pd 0),(pfIdInt 6, Pd 0)
                                    ,(pfIdInt 7,Pd 0.5),(pfIdInt 8, Pd 0)
                                    ,(pfIdInt 9,Ps path),(pfIdInt 10,Pd 1),(pfIdInt 11,Pd 0.9)
                                    ,(pfIdInt 12,Pd 2),(pfIdInt 13,Pd 8),(pfIdInt 14,Pd 0),(pfIdInt 15,Pd 1)] -- sample path, pitch, ktresh, kratio, divisor, pick, repeat
  emptyPat <- newTVarIO M.empty
  return $ I { insN   = 2
             , pf     = pfields
             , toPlay = Nothing
             , status = Inactive
             , timeF  = ""
             , pats   = emptyPat
             , kind   = Csound "11000"
             , itype  = Instrument
             }

phax :: IO Instr
phax = do
  pfields <- newTVarIO $ pfFromList [(pfIdInt 3,Pd 1),(pfIdInt 4,Pd 1)
                                    ,(pfIdInt 5,Pd 0),(pfIdInt 6,Pd 0)
                                    ,(pfIdInt 7,Pd 0.5),(pfIdInt 8,Pd 0)
                                    ,(pfIdInt 9,Pd 48),(pfIdInt 10,Pd 1100)
                                    ,(pfIdInt 11,Pd 0.8),(pfIdInt 12,Pd 0.33)
                                    ,(pfIdInt 13,Pd 0),(pfIdInt 14,Pd 1)
                                    ,(pfIdInt 15,Pd 3),(pfIdInt 16,Pd 2)
                                    ,(pfIdInt 17,Pd 0.5),(pfIdInt 18, Pd 0.5)
                                    ,(pfIdInt 19, Pd 0.91),(pfIdInt 20, Pd 1)
                                    ,(pfIdInt 21, Pd 0.5),(pfIdInt 22, Pd 0.9)
                                    ]
  emptyPat <- newTVarIO M.empty
  return $ I { insN = 10
             , pf = pfields
             , toPlay = Nothing
             , status = Inactive
             , timeF  = ""
             , pats   = emptyPat
             , kind   = Csound "11000"
             , itype  = Instrument
             }

models :: String -> Double -> IO Instr
models port chan = do
  pfields <- newTVarIO $ pfFromList [(pfIdInt 3,Pd 1),(pfIdInt 4,Pd 90)
                                    ,(pfIdInt 5,Pd 0),(pfIdInt 6,Pd 0)
                                    ,(pfIdInt 7,Pd 64),(pfIdInt 8,Pd chan)
                                    ,(pfIdInt 9,Pd 60),(pfIdInt 10,Pd 115)
                                    ,(pfIdInt 11,Pd 0),(pfIdInt 12,Pd 20)
                                    ,(pfIdInt 13,Pd 50),(pfIdInt 14,Pd 50)
                                    ,(pfIdInt 15,Pd 20),(pfIdInt 16,Pd 20)
                                    ]
  emptyPat <- newTVarIO M.empty
  return $ I { insN = 100
             , pf = pfields
             , toPlay = Nothing
             , status = Inactive
             , timeF  = ""
             , pats   = emptyPat
             , kind   = Csound port
             , itype  = Instrument
             }

modelChord :: String -> Double -> IO Instr
modelChord port chan = do
  pfields <- newTVarIO $ pfFromList [(pfIdInt 3,Pd 1),(pfIdInt 4,Pd 90)
                                    ,(pfIdInt 5,Pd 0),(pfIdInt 6,Pd 0)
                                    ,(pfIdInt 7,Pd 64),(pfIdInt 8,Pd chan)
                                    ,(pfIdInt 9,Pd 60),(pfIdInt 10,Pd 115)
                                    ,(pfIdInt 11,Pd 0),(pfIdInt 12,Pd 20)
                                    ,(pfIdInt 13,Pd 50),(pfIdInt 14,Pd 50)
                                    ,(pfIdInt 15,Pd 20),(pfIdInt 16,Pd 20)
                                    ]
  emptyPat <- newTVarIO M.empty
  return $ I { insN = 101
             , pf = pfields
             , toPlay = Nothing
             , status = Inactive
             , timeF  = ""
             , pats   = emptyPat
             , kind   = Csound port
             , itype  = Instrument
             }

-- default effects

reverb :: IO Instr
reverb = do
  pfields <- newTVarIO $ pfFromList [(pfIdInt 1, Pd 1),(pfIdInt 2, Pd 1000),(pfIdInt 3, Pd 0.6)]
  emptyPat <- newTVarIO M.empty
  return $ I { insN   = 550
             , pf     = pfields
             , toPlay = Nothing
             , status = Inactive
             , timeF  = ""
             , pats   = emptyPat
             , kind   = Csound "11000"
             , itype  = Effect
             }

delay :: IO Instr
delay = do
  pfields <- newTVarIO $ pfFromList [(pfIdInt 1, Pd 1),(pfIdInt 2, Pd 333),(pfIdInt 3, Pd 0.6)]
  emptyPat <- newTVarIO M.empty
  return $ I { insN   = 551
             , pf     = pfields
             , toPlay = Nothing
             , status = Inactive
             , timeF  = ""
             , pats   = emptyPat
             , kind   = Csound "11000"
             , itype  = Effect
             }

chorus :: IO Instr
chorus = do
   pfields <- newTVarIO $ pfFromList [(pfIdInt 1, Pd 1),(pfIdInt 2, Pd 3),(pfIdInt 3, Pd 4)]
   emptyPat <- newTVarIO M.empty
   return $ I { insN   = 552
              , pf     = pfields
              , toPlay = Nothing
              , status = Inactive
              , timeF  = ""
              , pats   = emptyPat
              , kind   = Csound "11000"
              , itype  = Effect
              }

master :: IO Instr
master = do
  pfields <- newTVarIO $ pfFromList [(pfIdInt 1, Pd 0.8),(pfIdInt 2, Pd 0),(pfIdInt 3, Pd 0),(pfIdInt 4, Pd 50)]
  emptyPat <- newTVarIO M.empty
  return $ I { insN   = 999
             , pf     = pfields
             , toPlay = Nothing
             , status = Inactive
             , timeF  = ""
             , pats   = emptyPat
             , kind   = Csound "11000"
             , itype  = Effect
             }

-------------- Create OSC Instrument ---------
oscInstr :: InstrumentID -> String -> [(PfId, Pfield)] -> IO Instr
oscInstr i_n osc_port pfields = do
  pfieldss <- newTVarIO $ pfFromList pfields
  emptyPat <- newTVarIO M.empty
  return $ I { insN   = i_n
             , pf     = pfieldss
             , toPlay = Nothing
             , status = Inactive
             , timeF  = ""
             , pats   = emptyPat
             , kind   = OSC osc_port
             , itype  = Instrument
             }
---------------------------------------------
-- default Orchestra

defaultOrc :: IO Orchestra
defaultOrc = do
  chh  <- hihat 0.2
  ohh  <- hihat 0.8
  a303 <- acidBass
  hov  <- hoover
  karpS <- karp
  rev  <- reverb
  del  <- delay
  lpFM <- fmSub
  sSaw <- superSaw
  strPad <- stringPad
  phaxo <- phax
  cycles1 <- models "11000" 1
  mstab <- modelChord "11000" 2
  chorus <- chorus
  mix <- master
  ot <- oscInstr 666 "11100" [(pfIdInt 3, Pd 0.8),(pfIdInt 2,Ps "Test")]
  newTVarIO (M.fromList [("OH808",ohh),("CH808",chh)
                                            ,("303",a303),("hov",hov)
                                            ,("rev",rev),("del",del)
                                            ,("karp",karpS),("lpFM",lpFM)
                                            ,("sSaw", sSaw),("strPad",strPad)
                                            ,("mix",mix),("chorus",chorus)
                                            ,("phax",phaxo),("test",ot),("mstab",mstab)
                                            ,("mc",cycles1)
                                            ])

-- returns all instruments that are not effects
notEffect :: [String] -> [String]
notEffect = filter (/= "rev") . filter (/= "del") . filter (/= "mix") . filter ( /= "chorus")

-- | function to default a pfield pattern to the value of the pfield
defaultPfpat :: Instr -> PfPat -> IO ()
defaultPfpat i pfp = do
  Just pf <- lookupMap (pf i) (pfId pfp)
  atomically $ writeTVar (pat pfp) [pf]

pfFromList :: [(PfId, Pfield )] -> M.Map PfId  Pfield
pfFromList = M.fromList 
