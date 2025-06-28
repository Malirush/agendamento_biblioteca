module Main where

import Test.Hspec

main :: IO ()
main = hspec $ describe "Spec não implementado" $ do
  it "retorna True" $ True `shouldBe` True