-- ~\~ language=Haskell filename=src/Problems/W0104.hs
-- ~\~ begin <<docs/README.md|src/Problems/W0104.hs>>[0]
module Problems.W0104 where
data Tree a = Leaf a | Node (Tree a) (Tree a)

splitleft :: Tree a -> (a, Maybe (Tree a))
splitleft (Leaf a  ) = (a, Nothing)
splitleft (Node l r) = case splitleft l of
  (a, Nothing) -> (a, Just r)
  (a, Just l') -> (a, Just (Node l' r))
-- ~\~ end
