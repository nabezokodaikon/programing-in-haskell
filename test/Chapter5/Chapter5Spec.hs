module Chapter5.Chapter5Spec (spec) where

import Test.Hspec
import Chapter5.Chapter5

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    describe "5.5 シーザー暗号" $ do
        it "positions" $
            positions 1 [1, 2, 3, 1, 4] `shouldBe` [0, 3]
        it "lowers" $
            lowers "Ab C" `shouldBe` 1
        it "count" $
            count 'c' "aabbbccc" `shouldBe` 3
        it "let2int" $
            let2int 'a' `shouldBe` 0
        it "int2let" $
            int2let 0 `shouldBe` 'a'
        it "shift" $
            shift 3 'a' `shouldBe` 'd'
        it "shift 2" $
            shift 3 'z' `shouldBe` 'c'
        it "shift 3" $
            shift (-3) 'c' `shouldBe` 'z'
        it "encode" $
            encode 3 "abc" `shouldBe` "def"
        it "encode 2" $
            encode (-3) "def" `shouldBe` "abc"
        it "rotate" $
            rotate 3 [1, 2, 3, 4, 5] `shouldBe` [4, 5, 1, 2, 3]
    describe "5.7 練習問題" $ do
        it "1" $
            exercises1 `shouldBe` 14
        it "2" $
            replicate' 3 True `shouldBe` [True, True, True]
        it "3" $
            pyths 10 `shouldBe` [(3, 4, 5), (4, 3, 5), (6, 8, 10), (8, 6, 10)]
        it "4 factors" $
            factors 12 `shouldBe` [1, 2, 3, 4, 6]
        it "4 perfects" $
            perfects 500 `shouldBe` [6, 28, 496]
        it "5" $
            [(x, y) | x <- [1, 2, 3], y <- [4, 5, 6]] `shouldBe`
            concat [[(x, y) | y <- [4, 5, 6]] | x <- [1, 2, 3]]
        it "6" $
            positions' 1 [1, 2, 3, 1, 4] `shouldBe` [0, 3]
