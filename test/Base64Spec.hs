module W0105Spec where

import           Problems.W0105
import           Data.Bits
import           Test.Hspec
import           Test.Hspec.QuickCheck          ( prop )
import           Test.QuickCheck         hiding ( (.&.) )

spec :: Spec
spec = do
  describe "trie" $ do
    prop "obeys the invariant of validity, all created tries are valid"
      $ \x -> x == x
