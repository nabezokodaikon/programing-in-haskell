module Chapter1.Chapter1Spec (spec) where

import Test.Hspec
import Chapter2.Chapter2

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    describe "2.4 Haskellプログラム" $ do
        it "quadruple" $
            quadruple 10 `shouldBe` 40
        it "double" $
            take (double 2) [1, 2, 3, 4, 5, 6] `shouldBe` [1, 2, 3, 4]
        it "factorial" $
            factorial 10 `shouldBe` 3628800
        it "average" $
            average [1, 2, 3, 4, 5] `shouldBe` 3
    describe "2.6 練習問題" $ do
        it "1 - 1" $
            2 ^ 3 * 4 `shouldBe` (2 ^ 3) * 4 
        it "1 - 2" $
            2 * 3 + 4 * 5 `shouldBe` (2 * 3) + (4 * 5)
        it "1 - 3" $
            2 + 3 * 4 ^ 5 `shouldBe` 2 + (3 * (4 ^ 5))
        it "3" $
            n `shouldBe` 2
        it "4 - 1" $
            last1 [1, 2, 3] `shouldBe` 3
        it "4 - 2" $
            last2 [1, 2, 3] `shouldBe` 3
        it "4 - 3" $
            last3 [1, 2, 3] `shouldBe` 3
        it "5 - 1" $
            init1 [1, 2, 3] `shouldBe` [1, 2]
        it "5 - 2" $
            init2 [1, 2, 3] `shouldBe` [1, 2]
        it "5 - 3" $
            init3 [1, 2, 3] `shouldBe` [1, 2]
