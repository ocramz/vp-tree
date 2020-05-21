module Main where

import Test.Hspec
-- import Test.Hspec.QuickCheck

import qualified Data.Vector as V (Vector, fromList)

import Data.VPTree (VPTree, build)

main :: IO ()
main = hspec spec

spec :: Spec
spec =
  describe "Lib" $ do
    it "works" $ do
      True `shouldBe` True
    -- prop "ourAdd is commutative" $ \x y ->
    --   ourAdd x y `shouldBe` ourAdd y x




