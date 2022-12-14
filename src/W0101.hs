module W0101 where

-- | Returns the list of all permutations of the argument
--
-- >>> import qualified Data.List as L
-- >>> L.sort (permutations "abc")
-- ["abc","acb","bac","bca","cab","cba"]
--

-- For a given list it returns a list containing the given list cycled left 'n' number of times
cycleList :: Int -> [a] -> [[a]]
cycleList 0 xs = [xs]
cycleList n xs = xs : cycleList (n-1) (tail xs ++ [head xs]) 

-- Algorithm : Using permutations recursively on the tail of every list obtained from 'cycleList' and then appending the head on all the elements of permutations of the tail
permutations :: [a] -> [[a]]
permutations xs 
    | length xs <= 1 = [xs]
    | otherwise = cycleList (length xs - 1) xs >>= \(y:ys) -> map (y:) $ permutations ys 
