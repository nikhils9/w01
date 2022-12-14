{-# LANGUAGE ScopedTypeVariables #-}

module W0103 where

data Tree a = Leaf a | Node (Tree a) (Tree a) deriving (Show, Eq)

splitleft :: Tree a -> (a, Maybe (Tree a))
splitleft (Leaf a  ) = (a, Nothing)
splitleft (Node l r) = case splitleft l of
  (a, Nothing) -> (a, Just r)
  (a, Just l') -> (a, Just (Node l' r))

-- | A tail-recursive version of @'splitleft'@.
-- Code written after checking solution
splitleft' :: Tree a -> (a, Maybe (Tree a))
splitleft' t = splitleftCont t id

splitleftCont :: forall a. Tree a -> (Maybe (Tree a) -> Maybe (Tree a)) -> (a, Maybe (Tree a))
splitleftCont (Leaf a) cont = (a, cont Nothing)
splitleftCont (Node l r) cont =
  let 
    f :: Maybe (Tree a) -> Tree a
    f Nothing = r
    f (Just l') = Node l' r 
  in splitleftCont l (cont . Just . f)
