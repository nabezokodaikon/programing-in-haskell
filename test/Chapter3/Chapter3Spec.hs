module Chapter3.Chapter3Spec (spec) where

import Test.Hspec
import Chapter3.Chapter3

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    describe "3.5 関数型" $ do
        it "add" $
            add (1, 2) `shouldBe` 3
        it "zeroto" $
            zeroto 3 `shouldBe` [0, 1, 2, 3]
    describe "3.11 練習問題" $ do
        it "1 - 1" $
            (read "['a', 'b', 'c']" :: [Char]) `shouldBe` ['a', 'b', 'c']
        it "1 - 2" $
            (read "('a', 'b', 'c')" :: (Char, Char, Char)) `shouldBe` ('a', 'b', 'c')
        it "1 - 3" $
            (read "[(False, '0'), (True, '1')]" :: [(Bool, Char)]) `shouldBe` [(False, '0'), (True, '1')]
        it "1 - 4" $
            (read "([False, True], ['0', '1'])" :: ([Bool], [Char])) `shouldBe` ([False, True], ['0', '1'])
        it "1 - 5" $
            (read "[[2, 3], [1, 2], [3, 2, 1]]" :: [[Int]]) `shouldBe`
            [tail [1, 2, 3], init [1, 2, 3], reverse [1, 2, 3]]
