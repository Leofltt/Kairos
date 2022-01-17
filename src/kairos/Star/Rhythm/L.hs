module Kairos.Star.Rhythm.L where

-- | A data type that holds either an element of type a or a list of elements of type a.
data LList a = E a | L [a] deriving (Show, Eq, Ord)

instance Functor LList where
  fmap f (E x) = E (f x)
  fmap f (L xs) = L (map f xs)

instance (Num a) => Num (LList a) where
  (+) (E x) (E y) = E (x + y)
  (+) (L xs) (L ys) = L (zipWith (+) xs ys)
  (+) (E x) (L ys) = L (map (+x) ys)
  (+) (L xs) (E y) = L (map (+y) xs)
  (*) (E x) (E y) = E (x * y)
  (*) (L xs) (L ys) = L (zipWith (*) xs ys)
  (*) (E x) (L ys) = L (map (*x) ys)
  (*) (L xs) (E y) = L (map (*y) xs)
  abs = fmap abs
  signum = fmap signum
  fromInteger x = E (fromInteger x)
  negate = fmap negate

toTimeSeq :: Double -> [LList Double] -> [LList Double]
toTimeSeq n l = map (fmap (*(n/x)) . (+ (-1))) $ filter (/=0) $ zipWith kindaMult (map normSumInner l) llToInf
  where x = fromIntegral $ length l

llToInf :: [LList Double]
llToInf = fmap E [1,2..]

lToInf :: [Double]
lToInf = [1,2..]

normSumInner :: LList Double -> LList Double
normSumInner (E x) = E x
normSumInner (L xs) = L $ map ((* (1/x)) . (+ (-1))) $ filter (/=0) $ zipWith (*) xs lToInf
  where x = fromIntegral $ length xs

kindaMult :: LList Double -> LList Double -> LList Double
kindaMult (E x) (E y) = E (x*y)
kindaMult (L xs) (E y) = L (map (+y) xs)

flatten :: [LList a] -> [a]
flatten = concatMap flatten'
  where flatten' (E x) = [x]
        flatten' (L xs) = xs