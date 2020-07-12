-- ~\~ language=Haskell filename=Problems/W0108.hs
-- ~\~ begin <<docs/README.md|Problems/W0108.hs>>[0]
module Problems.W0108 where

data GP a = End a
          | Get (Int -> GP a)
          | Put Int (GP a)

-- ~\~ end
