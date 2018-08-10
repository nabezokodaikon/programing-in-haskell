module Chapter6.Chapter6Spec (spec) where

import Test.Hspec
import Chapter6.Chapter6

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    describe "6.1 基本概念" $ do
        it "factorial" $
            factorial 3 `shouldBe` 6
        it "multiple" $
            3 `multiple` 5 `shouldBe` 15
        it "multiple 2" $
            3 `multiple` 1 `shouldBe` 3
        it "multiple 3" $
            3 `multiple` 0 `shouldBe` 0
