module Kairos.Instrument where

import Kairos.Base
import Kairos.Clock
import Kairos.Utilities
import Control.Concurrent
import Control.Concurrent.STM
import qualified Data.Map.Strict as M


pfToString :: [Pfield] -> String
pfToString ps = unwords $ map show ps

getPfields :: Instr -> IO (PfMap)
getPfields i = do
  pf <- readTVarIO $ pf i
  return $ pf

toPfD :: [Double] -> [Pfield]
toPfD (x:xs) = (Pd x) : toPfD xs
toPfD []     = []

pfD :: Double -> Pfield
pfD x = Pd x

toPfS :: [String] -> [Pfield]
toPfS (x:xs) = (Ps x) : toPfS xs
toPfS []     = []

pfS :: String -> Pfield
pfS x = Ps x

withTimeSignature :: Performance -> [Pfield] -> IO [Pfield]
withTimeSignature perf l = do
  ts <- currentTS $ clock perf
  let oneSecond = 60/(bpm ts)
  let oneBarSecond = (beatInMsr ts) * oneSecond
  let beatsInSeconds = map (*oneBarSecond) (stringToDouble $ map show l)
  return $ toPfD beatsInSeconds

-- default instruments

hihat :: Double -> IO Instr
hihat oc = do
  pfields <- newTVarIO $ M.fromList [(3,Pd 1),(4,Pd 1),(5,Pd 0),(6, Pd 0),(7,Pd 0.5),(8,Pd oc),(9,Pd oc),(10,Pd 1)]
  emptyPat <- newTVarIO M.empty
  return $ I { insN   = 5
             , pf     = pfields
             , toPlay = Nothing
             , status = Inactive
             , timeF  = ""
             , pats   = emptyPat
             , kind   = Csound
             }

sampler :: String -> IO Instr
sampler path = do
  pfields <- newTVarIO $ M.fromList [(3,Pd 1),(4,Pd 1)
                                    ,(5,Pd 0),(6, Pd 0)
                                    ,(7,Pd 0.5),(8, Pd 0)
                                    ,(9,Ps path),(10,Pd 1),(11,Pd 0.9),(12,Pd 2)] -- p9 : Sample path, p10 : pitch
  emptyPat <- newTVarIO M.empty
  return $ I { insN   = 1
             , pf     = pfields
             , toPlay = Nothing
             , status = Inactive
             , timeF  = ""
             , pats   = emptyPat
             , kind   = Csound
             }

acidBass :: IO Instr
acidBass = do
  pfields <- newTVarIO $ M.fromList  [(3,Pd 0.7),(4,Pd 0.7),(5,Pd 0),(6, Pd 0),(7,Pd 0.5),(8, Pd 0),(9,Pd 48),(10,Pd 14000),(11,Pd 9),(12, Pd 0)]
  emptyPat <- newTVarIO M.empty
  return $ I { insN   = 3
             , pf     = pfields
             , toPlay = Nothing
             , status = Inactive
             , timeF  = ""
             , pats   = emptyPat
             , kind   = Csound
             }

hoover :: IO Instr
hoover = do
  pfields <- newTVarIO $ M.fromList  [(3,Pd 1),(4,Pd 0.7),(5,Pd 0),(6, Pd 0),(7,Pd 0.5),(8, Pd 0),(9,Pd 48),(10,Pd 888),(11,Pd 5),(12, Pd 0.2)]
  emptyPat <- newTVarIO M.empty
  return $ I { insN   = 4
             , pf     = pfields
             , toPlay = Nothing
             , status = Inactive
             , timeF  = ""
             , pats   = emptyPat
             , kind   = Csound
             }

karp :: IO Instr
karp = do
  pfields <- newTVarIO $ M.fromList  [(3,Pd 1),(4,Pd 0.5),(5,Pd 0),(6, Pd 0),(7,Pd 0.5),(8, Pd 0),(9,Pd 48),(10,Pd 0.1),(11,Pd 0.1)]
  emptyPat <- newTVarIO M.empty
  return $ I { insN   = 9
             , pf     = pfields
             , toPlay = Nothing
             , status = Inactive
             , timeF  = ""
             , pats   = emptyPat
             , kind   = Csound
             }

fmSub :: IO Instr
fmSub = do
  pfields <- newTVarIO $ M.fromList [(3, Pd 1), (4, Pd 1), (5, Pd 0), (6, Pd 0), (7,Pd 0.5),(8, Pd 0),(9, Pd 60),(10, Pd 20000)
                                    ,(11, Pd 2), (12, Pd 0.2), (13, Pd 1), (14, Pd 2000), (15, Pd 2.45) ]
  emptyPat <- newTVarIO M.empty
  return $ I { insN   = 6
             , pf     = pfields
             , toPlay = Nothing
             , status = Inactive
             , timeF  = ""
             , pats   = emptyPat
             , kind   = Csound
             }

superSaw :: IO Instr
superSaw = do
  pfields <- newTVarIO $ M.fromList [(3, Pd 1), (4, Pd 1), (5, Pd 0), (6, Pd 0), (7,Pd 0.5),(8, Pd 0),(9, Pd 60),(10, Pd 500)
                                    ,(11, Pd 2),(12, Pd 0.2), (13, Pd 5000)]
  emptyPat <- newTVarIO M.empty
  return $ I { insN   = 7
             , pf     = pfields
             , toPlay = Nothing
             , status = Inactive
             , timeF  = ""
             , pats   = emptyPat
             , kind   = Csound
             }

stringPad :: IO Instr
stringPad = do
  pfields <- newTVarIO $ M.fromList [(3, Pd 1), (4, Pd 1), (5, Pd 0), (6, Pd 0), (7,Pd 0.5),(8, Pd 0),(9, Pd 60)]
  emptyPat <- newTVarIO M.empty
  return $ I { insN   = 8
             , pf     = pfields
             , toPlay = Nothing
             , status = Inactive
             , timeF  = ""
             , pats   = emptyPat
             , kind   = Csound
             }

stutter :: String -> IO Instr
stutter path = do
  pfields <- newTVarIO $ M.fromList [(3,Pd 1),(4,Pd 1)
                                    ,(5,Pd 0),(6, Pd 0)
                                    ,(7,Pd 0.5),(8, Pd 0)
                                    ,(9,Ps path),(10,Pd 1),(11,Pd 0.9)
                                    ,(12,Pd 2),(13,Pd 8),(14,Pd 0),(15,Pd 1)] -- sample path, pitch, ktresh, kratio, divisor, pick, repeat
  emptyPat <- newTVarIO M.empty
  return $ I { insN   = 2
             , pf     = pfields
             , toPlay = Nothing
             , status = Inactive
             , timeF  = ""
             , pats   = emptyPat
             , kind   = Csound
             }

phax :: IO Instr
phax = do
  pfields <- newTVarIO $ M.fromList [(3,Pd 1),(4,Pd 1)
                                    ,(5,Pd 0),(6,Pd 0)
                                    ,(7,Pd 0.5),(8,Pd 0)
                                    ,(9,Pd 48),(10,Pd 1100)
                                    ,(11,Pd 0.8),(12,Pd 0.33)
                                    ,(13,Pd 0),(14,Pd 1)
                                    ,(15,Pd 3),(16,Pd 2)
                                    ,(17,Pd 0.5),(18, Pd 0.5)
                                    ,(19, Pd 0.91),(20, Pd 1)
                                    ,(21, Pd 0.5),(22, Pd 0.9)
                                    ]
  emptyPat <- newTVarIO M.empty
  return $ I { insN = 10
             , pf = pfields
             , toPlay = Nothing
             , status = Inactive
             , timeF  = ""
             , pats   = emptyPat
             , kind   = Csound
             }
             
-- default effects

reverb :: IO Instr
reverb = do
  pfields <- newTVarIO $ M.fromList [(3,Pd (-1))]
  emptyPat <- newTVarIO M.empty
  return $ I { insN   = 550
             , pf     = pfields
             , toPlay = Nothing
             , status = Inactive
             , timeF  = ""
             , pats   = emptyPat
             , kind   = Csound
             }

delay :: IO Instr
delay = do
  pfields <- newTVarIO $ M.fromList [(3,Pd (-1))]
  emptyPat <- newTVarIO M.empty
  return $ I { insN   = 551
             , pf     = pfields
             , toPlay = Nothing
             , status = Inactive
             , timeF  = ""
             , pats   = emptyPat
             , kind   = Csound
             }

chorus :: IO Instr
chorus = do
   pfields <- newTVarIO $ M.fromList [(3, Pd (-1))]
   emptyPat <- newTVarIO M.empty
   return $ I { insN   = 552
              , pf     = pfields
              , toPlay = Nothing
              , status = Inactive
              , timeF  = ""
              , pats   = emptyPat
              , kind   = Csound
              }

master :: IO Instr
master = do
  pfields <- newTVarIO $ M.fromList [(3, Pd (-1))]
  emptyPat <- newTVarIO M.empty
  return $ I { insN   = 999
             , pf     = pfields
             , toPlay = Nothing
             , status = Inactive
             , timeF  = ""
             , pats   = emptyPat
             , kind   = Csound
             }

-------------- NOT WORKING YET ! ---------
other :: Int -> [(Int,Pfield)] -> IO Instr
other i_n pfields = do
  pfieldss <- newTVarIO $ M.fromList pfields
  emptyPat <- newTVarIO M.empty
  return $ I { insN = i_n
             , pf = pfieldss
             , toPlay = Nothing
             , timeF = ""
             , pats = emptyPat
             , kind = Other
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
  chorus <- chorus
  mix <- master
  ot <- other 666 [(1,Pd 0.8),(2,Ps "Test")]
  orc  <- atomically $ newTVar $ M.fromList [("OH808",ohh),("CH808",chh)
                                            ,("303",a303),("hov",hov)
                                            ,("rev",rev),("del",del)
                                            ,("karp",karpS),("lpFM",lpFM)
                                            ,("sSaw", sSaw),("strPad",strPad)
                                            ,("mix",mix),("chorus",chorus)
                                            ,("phax",phaxo),("test",ot)
                                            ]
  return $ orc


displayInstruments :: Performance -> IO String
displayInstruments perf = do
   ins <- readTVarIO (orc perf)
   return $ unwords $ M.keys ins

-- to add instruments
addInstrument :: Performance -> String -> Instr -> IO ()
addInstrument perf name instr = addToMap (orc perf) (name,instr)


-- function to create a PfPat
createPfPat :: Int -> [Pfield] -> (PfPat -> IO Pfield) -> IO PfPat
createPfPat num pfields updtr = do
  ptrn <- newTVarIO pfields
  return $ PfPat { pfNum = num
                 , pat = ptrn
                 , updater = updtr
                 }


-- function to default a pattern to the value of the pfield
defaultPfpat :: Instr -> PfPat -> IO ()
defaultPfpat i pfp = do
  Just pf <- lookupMap (pf i) (pfNum pfp)
  atomically $ writeTVar (pat pfp) [pf]
  return ()


addPfPath :: Instr -> Int -> PfPat -> IO ()
addPfPath i num pfPat = addToMap (pats i) (num,pfPat)

addPfPath' :: Performance -> [Char] -> Int -> PfPat -> IO ()
addPfPath' e insname num pfPat = do
  Just i <- lookupMap (orc e) insname
  addPfPath i num pfPat
