module Chapter4.Chapter4Spec (spec) where

import Test.Hspec
import Chapter4.Chapter4

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    describe "4.8 練習問題" $ do
        it "1" $
            halve [1, 2, 3, 4, 5, 6] `shouldBe` ([1, 2, 3], [4, 5, 6])
        it "2" $
            safetail [1, 2, 3] `shouldBe` [2, 3]
        -- it "2 - 2" $
            -- safetail [] `shouldBe` []
        it "3" $
            True `orElse` True `shouldBe` True
        it "3 - 2" $
            True `orElse2` False `shouldBe` True
        it "3 - 3" $
            False `orElse3` True `shouldBe` True
        it "3 - 4" $
            False `orElse` False `shouldBe` False 
        it "4" $
            True `andAlso` True `shouldBe` True
        it "4 - 2" $
            True `andAlso` False `shouldBe` False
        it "6" $
            mult 2 3 4 `shouldBe` 24
