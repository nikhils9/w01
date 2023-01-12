module W0104 where

import qualified Data.Map as M
import Prelude hiding (lookup, null)

data Trie a = Fork (Maybe a) (M.Map Char (Trie a)) deriving (Show, Eq)

instance Functor Trie where
    fmap f t@(Fork a st)
        | null t = empty
        | otherwise = Fork (f <$> a) $ (f <$>) <$> st 

instance Foldable Trie where
    foldr f acc t@(Fork a st)
        | null t = acc
        | otherwise = 
            let acc' = 
                    case a of
                        Nothing -> acc
                        Just a' -> f a' acc
            in foldr' f acc' $ M.elems st

    foldMap f t@(Fork a st)
        | null t = mempty
        | otherwise =
            let v = 
                    case a of
                        Nothing -> mempty
                        Just v' -> f v' 
            in v <> (foldMap' f $ M.elems st)

foldMap' :: Monoid m => (a -> m) -> [Trie a] -> m
foldMap' f [] = mempty
foldMap' f (x:xs) = 
    foldMap f x <> foldMap' f xs

foldr' :: (a -> b -> b) -> b -> [Trie a] -> b
foldr' _ acc [] = acc
foldr' f acc (x:xs) = 
    let acc' = foldr f acc x
    in foldr' f acc' xs

-- | Returns empty trie
--
-- >>> empty
-- Fork Nothing (fromList [])
--
empty  :: Trie a -- produces an empty trie
empty = Fork Nothing M.empty

-- >>> null empty
-- True
null   :: Trie a -> Bool -- checks if a trie is empty
null (Fork Nothing subtries) = M.null subtries
null _ = False

valid  :: Trie a -> Bool -- checks if a trie adheres to the invariant
valid (Fork _ subtries)
    | M.null subtries = True
    | otherwise = and $ map (\(a, trie) -> (not $ null trie) && valid trie) $ M.toList subtries

-- A wrapper function for insert to allow trie creation for all Showable types as opposed to just String.
insert' :: Show b => b -> a -> Trie a -> Trie a
insert' b = insert $ show b

insert :: String -> a -> Trie a -> Trie a -- inserts/overwrites a key-value pair
insert [] val (Fork _ subtries) = Fork (Just val) subtries
insert (x:xs) val (Fork v subtries) = 
    let 
        trie = case M.lookup x subtries of
            Nothing -> empty
            Just t -> t
    in Fork v $ M.insert x (insert xs val trie) subtries

lookup :: String -> Trie a -> Maybe a -- looks up the value associated with the key
lookup [] (Fork val _) = val
lookup (x:xs) (Fork _ subtries) = 
    case M.lookup x subtries of
        Nothing -> Nothing
        Just trie -> lookup xs trie 

delete :: String -> Trie a -> Trie a -- deletes the key if it exists
delete [] trie@(Fork Nothing _) = trie
delete [] (Fork val subtries) = Fork Nothing subtries
delete (x:xs) trie@(Fork val subtries) = 
    case M.lookup x subtries of
        Nothing -> trie
        Just st -> Fork val subtries'
            where subtries' = 
                    let nt = delete xs st
                    in if null nt then M.delete x subtries else M.insert x nt subtries
