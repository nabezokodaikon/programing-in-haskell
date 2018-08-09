module Chapter5.Chapter5Spec (spec) where

import Test.Hspec
import Chapter5.Chapter5

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    describe "" $ do
        it "" $
