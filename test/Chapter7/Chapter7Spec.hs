module Chapter7.Chapter7Spec (spec) where

import Test.Hspec
import Chapter7.Chapter7

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    describe "7.1 基本概念" $ do
        it "twice" $
            twice (* 2) 3 `shouldBe` 12
        it "twice 2" $
            twice reverse [1, 2, 3] `shouldBe` [1, 2, 3]
    describe "7.2 リスト処理" $ do
        it "sumsqreven" $
            sumsqreven [1, 2, 3, 4] `shouldBe` 20
    describe "7.3 畳込関数foldr" $ do
        it "sum''" $
            sum'' [1, 2, 3] `shouldBe` 6
        it "length''" $
            length'' [1, 2, 3] `shouldBe` 3
        it "reverse'''" $
            reverse''' [1, 2, 3] `shouldBe` [3, 2, 1]
    describe "7.4 畳込関数foldl" $ do
        it "sum2" $
            sum2 [1, 2, 3] `shouldBe` 6
        it "sumViaFoldl" $
            sumViaFoldl [1, 2, 3] `shouldBe` 6
    describe "7.6 文字列の変換器" $ do
        it "bin2int" $
            bin2int [1, 0, 1, 1] `shouldBe` 13
        it "bin2int 2" $
            bin2int [1, 0, 1, 1] `shouldBe` bin2int2 [1, 0, 1, 1]
        it "int2bin" $
            int2bin 13 `shouldBe` [1, 0, 1, 1]
        it "make8" $
            make8 [1, 0, 1, 1] `shouldBe` [1, 0, 1, 1, 0, 0, 0, 0]
        it "transmit" $
            transmit "abc" `shouldBe` "abc"
