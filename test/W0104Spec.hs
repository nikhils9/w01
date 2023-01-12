{-# OPTIONS_GHC -Wno-orphans #-}

module W0104Spec
    ( spec
    ) where

import Test.Hspec
import Test.QuickCheck
import qualified Data.Map as M
import Prelude hiding (lookup, null)
import Control.Monad
import W0104

spec :: Spec
spec = do
    describe "empty" $ do
        it "gives empty trie" $ property prop_empty
    describe "null" $ do
        it "checks if null" $ property prop_null
    describe "valid" $ do
        it "checks if valid" $ property prop_valid
    describe "insert & lookup" $ do
        it "inserts, giving a valid trie and looks up" $ property prop_insert
    describe "delete" $ do
        it "deletes key and gives valid trie" $ property prop_delete

prop_empty :: Property
prop_empty = (empty :: Trie Int) === Fork Nothing (M.fromList [])

prop_null :: Trie Int -> Property
prop_null t = null t === (t == Fork Nothing (M.fromList []))

hasEmptySubtrie :: Trie a -> Bool
hasEmptySubtrie (Fork _ st) = or $ map (\(_, t) -> (null t) || hasEmptySubtrie t) $ M.toList st

prop_valid :: Trie Int -> Property
prop_valid t = valid t === (not.hasEmptySubtrie) t

prop_insert :: String -> Int -> Trie Int -> Property
prop_insert s i t = valid t ==> 
    let nt = insert s i t in (Just i == (lookup s nt)) && valid nt

prop_delete :: String -> Int -> Trie Int -> Property
prop_delete s i t = valid t && lookup s t == Nothing ==>
    let nt = delete s $ insert s i t 
    in t == nt && valid nt

instance Arbitrary a => Arbitrary (Trie a) where
    arbitrary = sized trie
        where trie n = liftM2 Fork arbitrary $ resize (n `div` 4) arbitrary

    shrink (Fork x st)
        | M.null st = []
        | otherwise = [empty] ++ M.elems st ++ [Fork x' st' | (x', st') <- shrink(x, st)]

validTrieGen1 :: Arbitrary a => Gen (Trie a)
validTrieGen1 = 
    sized trie
        where trie n = liftM (Fork Nothing) $ resize (n `div` 4) mapGen1

charGen :: Gen Char
charGen = choose ('a','z')

trieGen :: Arbitrary a => Gen (Trie a)
trieGen = sized trieGen'
    where trieGen' n = liftM2 Fork (liftM Just arbitrary) $ resize (n `div` 4) mapGen1

pairGen :: Arbitrary a => Gen (Char, Trie a)
pairGen = liftM2 (,) charGen trieGen

listGen :: Arbitrary a => Gen [(Char, Trie a)]
listGen = sized listGen'
    where listGen' n
            | n == 0 = return [] 
            | n == 1 = liftM (:[]) pairGen
            | otherwise = liftM2 (:) pairGen $ listGen' (n-1)
    
mapGen1 :: Arbitrary a => Gen (M.Map Char (Trie a))
mapGen1 = liftM M.fromList listGen
