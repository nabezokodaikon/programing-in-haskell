module Chapter6.Chapter6Spec (spec) where

import Test.Hspec
import Chapter6.Chapter6

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    describe "" $ do
        it "" $
