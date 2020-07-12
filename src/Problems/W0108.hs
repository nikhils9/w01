-- ~\~ language=Haskell filename=src/Problems/W0108.hs
-- ~\~ begin <<docs/README.md|src/Problems/W0108.hs>>[0]
module Problems.W0108 where

data GP a = End a
          | Get (Int -> GP a)
          | Put Int (GP a)

-- ~\~ end
