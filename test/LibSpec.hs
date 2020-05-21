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



-- test data

data P = P Double Double
instance Show P where
  show (P x y) = show (x,y)
pps :: V.Vector P
pps = V.fromList [P 0 1, P 1 2, P 3 4, P 2 4, P (- 2) 3, P (-10) 2, P (-8) 3, P 4 3, P 6 7,  P 10 10, P 20 2, P 15 5]
distp :: P -> P -> Double
distp (P x1 y1) (P x2 y2) = sqrt $ (x1 - x2)**2 + (y1 - y2)**2
