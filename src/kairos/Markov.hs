module Kairos.Markov where

import Kairos.Pfield ( Pfield )
import Kairos.PfPat
import Kairos.Utilities ( randF, stringToDouble )
import Text.CSV ( parseCSVFromFile )
import Data.Char ( isNumber, isSymbol )
import Data.List ( elemIndex )
import Data.Maybe ( fromJust )
import Control.Concurrent.STM ( atomically, readTVarIO, writeTVar )

runMarkovSimpleCSV :: String -> [Pfield] ->  IO [Pfield]
runMarkovSimpleCSV cs patt = do
  doc <- parseCSVFromFile cs
  let table = fmap (stringToDouble . tail) (prepareCSV doc)
  runMarkovSimple table patt

runMarkovSimple :: [[Double]] -> [Pfield] ->  IO [Pfield]
runMarkovSimple table patt = do
  let note = length $ filter (< head patt) patt
  prob <- randF
  let list = scanl1 (+) (pickRow note table)
  let newList = listFromIndex patt $ fromJust $ pickIndex prob list
  return newList

-- | updater to run Markov on CSV file
runMarkovCSV :: String -> PfPat -> IO Pfield
runMarkovCSV file n = do
  patrn <- readTVarIO (pat n)
  pat' <- runMarkovSimpleCSV file patrn
  atomically $ writeTVar (pat n) pat'
  return $ head pat'

-- | updater to run Markov on hand coded transition table [[Double]]
runMarkov :: [[Double]] -> PfPat -> IO Pfield
runMarkov table n = do
  patrn <- readTVarIO (pat n)
  pat' <- runMarkovSimple table patrn
  atomically $ writeTVar (pat n) pat'
  return $ head pat'

-- | shorthand versions
rMkv :: [[Double]] -> PfPat -> IO Pfield
rMkv = runMarkov
rMkvCSV :: String -> PfPat -> IO Pfield
rMkvCSV = runMarkovCSV

pickProb4Index :: Double -> [Double] -> Double
pickProb4Index perc (x:xs) | x >= perc = x
                           | (x <= perc) && (head xs > perc) =  head xs
                           | (x <= perc) && (head xs <= perc) = pickProb4Index perc (xs++[x])

pickIndex :: Double -> [Double] -> Maybe Int
pickIndex val list = elemIndex (pickProb4Index val list) list

listFromIndex :: [Pfield] -> Int -> [Pfield]
listFromIndex list indx = firstnote:filter (/= firstnote) list where
  firstnote = (!!) list indx

prepareCSV :: Either a [[b]] -> [[b]]
prepareCSV a = tail $ noEmptyRows a

noEmptyRows :: Either a [[b]] -> [[b]]
noEmptyRows = either (const []) (filter (\row -> 2 <= length row))

pickRow :: Int -> [[a]] -> [a]
pickRow indx prepFile = (!!) prepFile indx

removeNewLine :: [[Char]] -> [[Char]]
removeNewLine list = init list ++ [filter removeNL (last list)]

removeNL :: Char -> Bool
removeNL c = isNumber c || isSymbol c
