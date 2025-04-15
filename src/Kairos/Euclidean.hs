module Kairos.Euclidean where 

-- Taken and adapted from haskell music theory 
-- https://hackage.haskell.org/package/hmt

type STEP a = ((Int,Int),([[a]],[[a]]))

left :: STEP a -> STEP a
left ((i,j),(xs,ys)) =
    let (xs',xs'') = splitAt j xs
    in ((j,i-j),(zipWith (++) xs' ys,xs''))

right :: STEP a -> STEP a
right ((i,j),(xs,ys)) =
    let (ys',ys'') = splitAt i ys
    in ((i,j-i),(zipWith (++) xs ys',ys''))

bjorklund' :: STEP a -> STEP a
bjorklund' (n,x) =
    let (i,j) = n
    in if min i j <= 1
       then (n,x)
       else bjorklund' (if i > j then left (n,x) else right (n,x))

bjorklund :: (Int,Int) -> [Int]
bjorklund (i,j') =
    let j = j' - i
        x = replicate i [1]
        y = replicate j [0]
        (_,(x',y')) = bjorklund' ((i,j),(x,y))
    in concat x' ++ concat y'

euclidean :: (Int, Int) -> Int -> [Int]
euclidean x shift = rotate shift $ bjorklund x  

-- todo : fix this to work in both directions, 
-- right now it shifts left w/ positive values (counterintuitive)
-- and doesn't really behave with negative values
rotate ::  Int -> [a] -> [a]
rotate n list = take (length list) (drop n (cycle list)) 