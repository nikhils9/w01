module W0102 where

-- |  The idea of merge sort is to
--    split a list into two more or less equal parts, sort them recursively,
--    and then merge two sorted lists using a dedicated merge function.
--
-- >>> mergesort [6,5,3,1,8,7,2,4]
-- [1,2,3,4,5,6,7,8]
--
mergesort :: Ord a => [a] -> [a]
mergesort xs
    | length xs <= 1 = xs
    | otherwise =  
        let (a, b) = split xs
        in merge (mergesort a) (mergesort b) 

-- | Merges two (sorted) lists.
--
-- >>> merge "ADEX" "ABF"
-- "AABDEFX"
--
merge :: Ord a => [a] -> [a] -> [a]
merge xs [] = xs
merge [] ys = ys
merge xf@(x:xs) yf@(y:ys) = if x < y then x : merge xs yf else y : merge xf ys

-- | Splits a list into two lists of almost equal length.
--
split :: [a] -> ([a], [a])
split xs 
    | length xs <= 1 = (xs, [])
    | otherwise = (take half xs, drop half xs)
        where half = length xs `div` 2