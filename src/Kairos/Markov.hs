{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Kairos.Markov where

import Control.Concurrent.STM (atomically, readTVarIO, writeTVar)
import Data.Char (isNumber, isSymbol)
import Data.List (elemIndex)
import Kairos.PfPat (PfPat (pat), Updater)
import Kairos.Pfield (Pfield)
import Kairos.Utilities (randF, stringToDouble)
import Text.CSV (parseCSVFromFile)

runMarkovSimpleCSV :: String -> [Pfield] -> IO [Pfield]
runMarkovSimpleCSV cs patt = do
  doc <- parseCSVFromFile cs
  let rows = prepareCSV doc
  if null rows
    then return patt
    else do
      let table = fmap (stringToDouble . safeTail) rows
      runMarkovSimple table patt

safeTail :: [a] -> [a]
safeTail [] = []
safeTail (_ : xs) = xs

runMarkovSimple :: [[Double]] -> [Pfield] -> IO [Pfield]
runMarkovSimple table patt = do
  if null patt || null table
    then return patt
    else do
      let note = length $ filter (< head patt) patt
      prob <- randF
      let row = pickRow note table
      if null row
        then return patt
        else do
          let list = scanl1 (+) row
          case pickIndex prob list of
            Nothing -> return patt
            Just idx -> do
              let newList = listFromIndex patt idx
              return newList

-- | updater to run Markov on CSV file
runMarkovCSV :: String -> Updater
runMarkovCSV file n = do
  patrn <- readTVarIO (pat n)
  pat' <- runMarkovSimpleCSV file patrn
  atomically $ writeTVar (pat n) pat'
  if null pat'
    then error "runMarkovCSV: empty pattern"
    else return $ head pat'

-- | updater to run Markov on hand coded transition table [[Double]]
runMarkov :: [[Double]] -> Updater
runMarkov table n = do
  patrn <- readTVarIO (pat n)
  pat' <- runMarkovSimple table patrn
  atomically $ writeTVar (pat n) pat'
  if null pat'
    then error "runMarkov: empty pattern"
    else return $ head pat'

-- | shorthand versions
rMkv :: [[Double]] -> Updater
rMkv = runMarkov

rMkvCSV :: String -> Updater
rMkvCSV = runMarkovCSV

rMkvS :: [[Double]] -> [Pfield] -> IO [Pfield]
rMkvS = runMarkovSimple

rMkvSCSV :: String -> [Pfield] -> IO [Pfield]
rMkvSCSV = runMarkovSimpleCSV

pickProb4Index :: Double -> [Double] -> Double
pickProb4Index perc list = go perc list (length list)
  where
    go _ [] _ = 0 -- Should not happen with non-empty list
    go p (x : xs) fuel
      | fuel <= 0 = x -- Safety break to avoid infinite loop
      | x >= p = x
      | null xs = x -- Last element
      | (p > x) && (head xs > p) = head xs
      | otherwise = go p (xs ++ [x]) (fuel - 1)

pickIndex :: Double -> [Double] -> Maybe Int
pickIndex val list
  | null list = Nothing
  | otherwise = elemIndex (pickProb4Index val list) list

listFromIndex :: [Pfield] -> Int -> [Pfield]
listFromIndex list indx
  | null list || indx < 0 || indx >= length list = list
  | otherwise = firstnote : filter (/= firstnote) list
  where
    firstnote = (!!) list indx

prepareCSV :: Either a [[b]] -> [[b]]
prepareCSV a = safeTail $ noEmptyRows a

noEmptyRows :: Either a [[b]] -> [[b]]
noEmptyRows = either (const []) (filter (\row -> 2 <= length row))

pickRow :: Int -> [[a]] -> [a]
pickRow indx prepFile
  | null prepFile = []
  | indx < 0 = head prepFile
  | indx >= length prepFile = last prepFile
  | otherwise = (!!) prepFile indx

removeNewLine :: [[Char]] -> [[Char]]
removeNewLine list = init list ++ [filter removeNL (last list)]

removeNL :: Char -> Bool
removeNL c = isNumber c || isSymbol c
