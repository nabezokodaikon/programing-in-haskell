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
    describe "6.2 リストに対する再帰" $ do
        it "product'" $
            product' [2, 3, 4] `shouldBe` 24
        it "length'" $
            length' [2, 3, 4] `shouldBe` 3
        it "reverse'" $
            reverse' [2, 3, 4] `shouldBe` [4, 3, 2]
        it "concat'" $
            concat' [1, 2, 3] [4, 5] `shouldBe` [1, 2, 3, 4, 5]
        it "insert" $
            insert 3 [1, 2, 4, 5] `shouldBe` [1, 2, 3, 4, 5]
        it "isort" $
            isort [3, 2, 1, 4] `shouldBe` [1, 2, 3, 4]
    describe "6.3 複数の引数" $ do
        it "zip'" $
            zip' ['a', 'b', 'c'] [1, 2, 3, 4] `shouldBe`
            [('a', 1), ('b', 2), ('c', 3)]
        it "drop'" $
            drop' 0 [1, 2, 3] `shouldBe` [1, 2, 3]
        -- it "drop' 2" $
            -- drop' 3 [] `shouldBe` []
        it "drop' 3" $
            drop' 2 [1, 2, 3, 4] `shouldBe` [3, 4]
    describe "6.4 多重再帰" $ do
        it "fibonacci" $
            fibonacci 3 `shouldBe` 2
        it "fibonacci 2" $
            fibonacci 5 `shouldBe` 5
        it "qsort" $
            qsort [2, 5, 3, 1, 4] `shouldBe` [1, 2, 3, 4, 5]
    describe "6.5 相互再帰" $ do
        it "even'" $
            even' 4 `shouldBe` True
        it "even'" $
            even' 5 `shouldBe` False
        it "odd'" $
            odd' 4 `shouldBe` False 
        it "odd'" $
            odd' 5 `shouldBe` True
        it "evens" $ -- 0番目は偶数
            evens [1, 2, 3, 4, 5] `shouldBe` [1, 3, 5]  
        it "odds" $
            odds [1, 2, 3, 4, 5] `shouldBe` [2, 4]
    describe "6.6 再帰の秘訣" $ do
        it "init'" $
            init' [1, 2, 3] `shouldBe` [1, 2]
    describe "6.8 練習問題" $ do
        it "1. power" $
            2 `power` 3 `shouldBe` 8 
        it "3. and" $
            and [True, True, True] `shouldBe` True
        it "3. and 2" $
            and [True, False, True] `shouldBe` False
        it "3. concat''" $
            concat'' [[1, 2], [3, 4], [5, 6]] `shouldBe` [1, 2, 3, 4, 5, 6]
        it "3. replicate'" $
            replicate' 3 1 `shouldBe` [1, 1, 1]
        it "3. get" $
            get [1, 2, 3] 0 `shouldBe` 1
        it "3. get 2" $
            get [1, 2, 3, 4, 5] 3 `shouldBe` 4
        it "3. elem'" $
            elem' 3 [1, 2, 4, 5] `shouldBe` False
        it "3. elem' 2" $
            elem' 3 [1, 2, 3, 4, 5] `shouldBe` True
        it "4. merge" $
            merge [2, 5, 6] [1, 3, 4, 7] `shouldBe` [1, 2, 3, 4, 5, 6, 7]
        it "5. msort" $
            msort [2, 5, 3, 1, 4] `shouldBe` [1, 2, 3, 4, 5]
        it "6. sum'" $
            sum' [1, 2, 3, 4] `shouldBe` 10
        it "6. take'" $
            take' 2 [1, 2, 3, 4] `shouldBe` [1, 2] 
        it "6. last'" $
            last' [1, 2, 3, 4] `shouldBe` 4
