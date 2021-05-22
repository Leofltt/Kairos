module Kairos.Markov where

import Kairos.Base
import Kairos.Instrument
import Kairos.Utilities
import Text.CSV
import Data.Char
import Data.List
import Data.Maybe
import Control.Concurrent.STM


runMarkovSimpleCSV :: String -> [Pfield] ->  IO [Pfield]
runMarkovSimpleCSV cs pat = do
  doc <- parseCSVFromFile cs
  let table = fmap stringToDouble $ fmap tail $ prepareCSV doc
  runMarkovSimple table pat 

runMarkovSimple :: [[Double]] -> [Pfield] ->  IO [Pfield]
runMarkovSimple table pat = do
  let note = length $ filter (< ( head pat)) pat
  prob <- randF
  let list = scanl1 (+) (pickRow note table)
  let newList = listFromIndex pat $ fromJust $ pickIndex prob list
  return $ newList

runMarkovCSV :: String -> PfPat -> IO Pfield
runMarkovCSV file n = do
  patrn <- readTVarIO (pat n)
  pat' <- runMarkovSimpleCSV file patrn
  atomically $ writeTVar (pat n) pat'
  return $ head pat'

runMarkov :: [[Double]] -> PfPat -> IO Pfield
runMarkov table n = do
  patrn <- readTVarIO (pat n)
  pat' <- runMarkovSimple table patrn
  atomically $ writeTVar (pat n) pat'
  return $ head pat'


pickProb4Index :: Double -> [Double] -> Double
pickProb4Index perc (x:xs) | x >= perc = x
                           | (x <= perc) && ((head xs > perc)) =  head xs
                           | (x <= perc) && ((head xs) <= perc) = pickProb4Index perc (xs++[x])
                           

pickIndex val list = elemIndex (pickProb4Index val list) list

listFromIndex :: [Pfield] -> Int -> [Pfield]
listFromIndex list indx = firstnote:(filter (/= firstnote) list) where
  firstnote = (!!) list indx

prepareCSV a = tail $ noEmptyRows a

noEmptyRows = either (const []) (filter (\row -> 2 <= length row))

pickRow :: Int -> [[a]] -> [a]
pickRow indx prepFile = (!!) prepFile indx

removeNewLine list = (init list) ++ [filter removeNL (last list)]

removeNL c = (isNumber c) || (isSymbol c)
