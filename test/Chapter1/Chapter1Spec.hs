module Chapter1.Chapter1Spec (spec) where

import Test.Hspec
import Chapter1.Chapter1

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    describe "1.1 関数" $ do
        it "double" $
            double 2 `shouldBe` 4
    describe "1.5 Haskellの妙味" $ do
        it "sum'" $
            sum' [1..10] `shouldBe` 55
        it "qsort" $
            qsort [3, 5, 1, 4, 2] `shouldBe` [1, 2, 3, 4, 5]
    describe "1.7 練習問題" $ do
        it "1" $
            double(double 2) `shouldBe` double 2 + double 2
        it "2" $
            sum [0] `shouldBe` 0
        it "2-2" $
            sum [1] `shouldBe` 1
        it "3" $
            product' [2, 3, 4] `shouldBe` 24
        it "4" $
            qsort' [3, 5, 1, 4, 2] `shouldBe` [5, 4, 3, 2, 1]
        it "5" $
            qsort'' [2, 2, 3, 1, 1] `shouldBe` [1, 2, 3]
