module Example.ExampleSpec (spec) where

import Test.Hspec
import Example.Example

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Example" $ do
    it "addition" $
      addition 2 1 `shouldBe` 3
