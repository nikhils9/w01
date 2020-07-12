-- ~\~ language=Haskell filename=Problems/W0105.hs
-- ~\~ begin <<docs/README.md|Problems/W0105.hs>>[0]
module Problems.W0105 where
-- |  A trie (sometimes called a prefix tree) is an implementation of a finite
--    map for structured key types where common prefixes of the keys are
--    grouped together.

import           Data.Map

data Trie a = Fork (Maybe a) (Map Char (Trie a))
-- ~\~ end
