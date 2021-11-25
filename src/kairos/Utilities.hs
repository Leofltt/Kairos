module Kairos.Utilities where

import Control.Concurrent.STM
    ( atomically, readTVarIO, writeTVar, modifyTVar', TVar )
import qualified Data.Map.Strict as M
import Data.List (sort)
import System.Random (getStdRandom,randomR, mkStdGen, randomRs, randomRIO)
import System.IO.Unsafe ( unsafePerformIO )
import Control.Monad.IO.Class ( MonadIO )
import Data.Data ( Data(toConstr) )
import Data.Function ( on )


-- | Map Utilities

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

-- Misc Utilities

shuffle :: Control.Monad.IO.Class.MonadIO m => [a] -> m [a]
shuffle x = if length x < 2 then return x else do
  i <- randomRIO (0, length x-1)
  r <- shuffle (Prelude.take i x ++ Prelude.drop (i+1) x)
  return (x!!i : r)

{-# NOINLINE scramble #-}
scramble :: [a] -> [a]
scramble x = unsafePerformIO $ shuffle x

randI :: Int -> IO Int
randI i = getStdRandom $ randomR (0, i)

randF :: IO Double
randF = do
  x <- randI 100
  return $ fromIntegral x / 100

checkPercentNext :: Int -> Int -> [a] -> [a]
checkPercentNext v i p | v <= i = tail p++[head p]
                       | otherwise = p

genNRandomValues :: Int -> Int -> [Int]
genNRandomValues n seed = take n $ randomRs (0, 100) generator where
    generator = mkStdGen seed

filterEqualsList :: Ord a => [a] -> [a] -> [a]
filterEqualsList (x:xs) ys     | x `notElem` ys = filterEqualsList xs ys
                               | otherwise = filterEqualsList xs (filter (/= x) ys)
filterEqualsList _ [] = []
filterEqualsList [] y = y

interleave :: Ord a => [a] -> [a] -> [a]
interleave l1 l2 = sort $ inter l1 $ filterEqualsList l1 l2

inter :: Ord a => [a] -> [a] -> [a]
inter [x]  y     = x:y
inter []   y     = y
inter x    []    = x
inter (a:as) (b:bs) = a : b : inter as bs

offset :: Num a => a ->  [a] -> [a]
offset i = map (+ i)

modifyList :: (t -> a) -> [t] -> [a]
modifyList f l = [f x | x <- l]

--adapted from: https://stackoverflow.com/questions/27095647/convert-a-string-list-to-a-double-list-in-haskell
stringToDouble :: [String] -> [Double]
stringToDouble [x] = [read x :: Double]
stringToDouble (x:xs) = (read x :: Double) : stringToDouble xs
stringToDouble _ = []

binaryDigit :: Integer -> Integer
binaryDigit 0 = 0
binaryDigit x = 10 * binaryDigit (x `div` 2) + x `mod` 2

stringToBinary :: String -> [Integer]
stringToBinary = map ((binaryDigit . toInteger) . fromEnum)

binaryToBinString :: Show a => [a] -> String
binaryToBinString s = foldl1 (++) $ map show s

binStringToList :: [a] -> [[a]]
binStringToList = map (: [])

numToBinary :: Double -> [Double]
numToBinary n = stringToDouble $ binStringToList $ binaryToBinString [binaryDigit $ floor n]

textToBinary :: String -> [Double]
textToBinary t = stringToDouble $ binStringToList $ binaryToBinString $ stringToBinary t

intToDouble :: Int -> Double
intToDouble = fromIntegral

binToNormSum :: [Double] -> [Double]
binToNormSum x =  map ((/ (intToDouble $ length x)) . intToDouble) (binToSum x)

binToSum :: [Double] -> [Int]
binToSum [] = []
binToSum x | last x == 0 = 0 : binToSum (init x)
           | last x == 1 = length x : binToSum (init x)

numSeqFromText :: String -> [Double]
numSeqFromText t = reverse . filter (/=0) $ binToNormSum $ textToBinary t

numSeqFromBin :: Double -> [Double]
numSeqFromBin d = reverse . filter (/=0) $ binToNormSum $ numToBinary d

doubleToFloat :: Double -> Float
doubleToFloat = realToFrac

sameConstructor :: Data a => a -> a -> Bool
sameConstructor = (==) `on` toConstr

