module Kairos.Utilities where

import Kairos.Base
import Control.Concurrent
import Control.Concurrent.STM
import qualified Data.Map.Strict as M
import Data.List (sort, elem)
import System.Random (getStdRandom,randomR, mkStdGen, randomRs)


-- Map Utilities

modifyMap :: TVar a -> (a -> a) -> IO ()
modifyMap theMap f = atomically $ modifyTVar' theMap f

addToMap :: Ord k => TVar (M.Map k a) -> (k,a) -> IO ()
addToMap theMap (k,v) = modifyMap theMap $ M.insert k v

withMap :: TVar a -> (a -> b) -> IO b
withMap theMap f = do
  toMap <- readTVarIO theMap
  return $ f toMap

lookupMap :: Ord k => TVar (M.Map k a) -> k -> IO (Maybe a)
lookupMap theMap k = withMap theMap $ M.lookup k

-- PfPat Updaters

keep ::  PfPat -> IO Pfield
keep n = do
  pats <- readTVarIO (pat n)
  return $ head pats

nextVal :: PfPat -> IO Pfield
nextVal n = do
  patrn <- readTVarIO (pat n)
  let pat' = (tail patrn)++[head patrn]
  atomically $ writeTVar (pat n) pat'
  return $ head pat'

retrograde :: PfPat -> IO Pfield
retrograde n = do
  patrn <- readTVarIO (pat n)
  let pat' = (last patrn):(init patrn)
  atomically $ writeTVar (pat n) pat'
  return $ head pat'

randomize :: PfPat -> IO Pfield
randomize n = do
  p <- readTVarIO (pat n)
  let l = (length p) - 1
  ran <- randI l
  return $ (!!) p ran

percentNext :: Int -> PfPat -> IO Pfield
percentNext i n = do
  val <- randI 100
  p <- readTVarIO (pat n)
  let result = checkPercentNext val i p
  atomically $ writeTVar (pat n) result
  return $ head result

-- Misc Utilities

randI :: Int -> IO Int
randI i = getStdRandom $ randomR (0, i)

randF :: IO Double
randF = do
  x <- randI 100
  return $ (fromIntegral x) / 100

checkPercentNext :: Int -> Int -> [a] -> [a]
checkPercentNext v i p | v <= i = (tail p)++[head p]
                       | otherwise = p

genNRandomValues :: Int -> Int -> [Int]
genNRandomValues n seed = take n $ (randomRs (0, 100) generator) where
    generator = mkStdGen seed

filterEqualsList :: Ord a => [a] -> [a] -> [a]
filterEqualsList (x:xs) ys     | elem x ys == False = filterEqualsList xs ys
                               | otherwise = filterEqualsList xs (filter (/= x) ys)
filterEqualsList (x:xs) (y:[]) | elem x [y] == False = filterEqualsList xs [y]
                               | otherwise = []
filterEqualsList (x:[]) ys     | elem x ys == False = ys
                               | otherwise = (filter (/= x) ys)
filterEqualsList (x:[]) (y:[]) | elem x [y] == False = [y]
                               | otherwise = []
filterEqualsList _ [] = []
filterEqualsList [] y = y


interleave l1 l2 = sort $ inter' l1 $ filterEqualsList l1 l2
interleave' l1 l2 = inter' l1 l2

inter' :: Ord a => [a] -> [a] -> [a]
inter' (x:[])  y     = x:y
inter'  []     y     = y
inter'  x      []    = x
inter' (a:as) (b:bs) = a : b : (interleave as bs)
-- inter' (x:xs) (y:[]) = x:y:xs
-- inter'  _      _     = []

offset :: Num a => a ->  [a] -> [a]
offset i = map (+ i)

--adapted from: https://stackoverflow.com/questions/27095647/convert-a-string-list-to-a-double-list-in-haskell
stringToDouble :: [String] -> [Double]
stringToDouble [x] = [read x :: Double]
stringToDouble (x:xs) = (read x :: Double) : stringToDouble xs
stringToDouble _ = []

binaryDigit :: Integer -> Integer
binaryDigit 0 = 0
binaryDigit x = 10 * binaryDigit (x `div` 2) + x `mod` 2

stringToBinary :: String -> [Integer]
stringToBinary s = (map binaryDigit) $ map toInteger $ map fromEnum s

binaryToBinString :: Show a => [a] -> String
binaryToBinString s = foldl1 (++) $ map show s

binStringToList :: [a] -> [[a]]
binStringToList [] = []
binStringToList (b:bs) = [b] : binStringToList bs

numToBinary :: Double -> [Double]
numToBinary n = stringToDouble $ binStringToList $ binaryToBinString [binaryDigit $ floor n]

textToBinary :: String -> [Double]
textToBinary t = stringToDouble $ binStringToList $ binaryToBinString $ stringToBinary t

intToDouble :: Int -> Double
intToDouble = fromIntegral

binToNormSum :: [Double] -> [Double]
binToNormSum x =  map (/ (intToDouble $ length x)) $ map intToDouble $ binToSum x

binToSum [] = []
binToSum x | (last x) == 0 = 0 : (binToSum $ init x)
           | (last x) == 1 = (length x) : (binToSum $ init x)

numSeqFromText :: String -> [Double]
numSeqFromText t = reverse $ filter (/=0) (binToNormSum $ textToBinary t)

numSeqFromBin :: Double -> [Double]
numSeqFromBin d = reverse $ filter (/=0) (binToNormSum $ numToBinary d)

doubleToFloat :: Double -> Float
doubleToFloat = realToFrac
