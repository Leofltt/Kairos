module Kairos.Markov where

import Kairos.Base
import Kairos.Instrument
import Kairos.Utilities
import Text.CSV
import Data.Char
import Data.List
import Data.Maybe
import Control.Concurrent.STM


-- runMarkov csv path -> PfPat -> PfPat
runMarkovSimple :: String -> [Pfield]->  IO [Pfield]
runMarkovSimple cs pat = do
  doc <- parseCSVFromFile cs
  let file = prepareCSV doc
  let note = length $ filter (< ( head pat))  pat
  prob <- randF
  let list = scanl1 (+) (stringToDouble $ pickRow note file)
  let newList = listFromIndex (sort pat) $ fromJust $ pickIndex prob  list
  return $ newList



runMarkov :: String -> PfPat -> IO Pfield
runMarkov file n = do
  patrn <- readTVarIO (pat n)
  pat' <- runMarkovSimple file patrn
  atomically $ writeTVar (pat n) pat'
  return $ head pat'


pickProb4Index :: Double -> [Double] -> Double
pickProb4Index perc (x:xs) | x >= perc = x
                           | (x <= perc) && ((head xs) <= perc) = pickProb4Index perc (xs++[x])
                           | (x <= perc) && ((head xs > perc))=  x

pickIndex val list = elemIndex (pickProb4Index val list) list

listFromIndex :: [Pfield] -> Int -> [Pfield]
listFromIndex list indx = firstnote:(filter (/= firstnote) list) where
  firstnote = (!!) list indx



prepareCSV  a = tail $ noEmptyRows a

noEmptyRows = either (const []) (filter (\row -> 2 <= length row))

pickRow indx prepFile = tail $ (!!) prepFile indx

removeNewLine list = (init list) ++ [filter removeNL (last list)]

removeNL c = (isNumber c) || (isSymbol c)
