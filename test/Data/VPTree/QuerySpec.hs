{-# options_ghc -Wno-unused-imports #-}
module Data.VPTree.QuerySpec where

import Data.Foldable (toList)
import Text.Printf (printf)

-- mwc-probability
import qualified System.Random.MWC.Probability as P (Gen, Prob, withSystemRandom, asGenIO, GenIO, create, initialize, sample, samples, normal, bernoulli, uniformR)
-- primitive
import Control.Monad.Primitive (PrimMonad(..), PrimState)
-- vector
import qualified Data.Vector as V (Vector, map, filter, length, toList, replicate, partition, zipWith, head, tail, fromList, thaw, freeze, (!), foldl)
import qualified Data.Vector.Generic as VG (Vector(..))

import Test.Hspec (Spec, describe, it, shouldBe, shouldSatisfy)
-- import Test.Hspec.QuickCheck

import Data.VPTree.Build (build)
import Data.VPTree.Draw (draw)
import Data.VPTree.Internal (VT, VPTree, withST, withST_, withIO)
import Data.VPTree.Query (range, distances)
import Data.VPTree.TestData (buildP, binDiskSamples, P(..))

spec :: Spec
spec = describe "Data.VPTree.Query" $ do
  it "range : range query result set is within the threshold distance" $ do
    let
      thr = 1.0
      query = P 0 1
      n = 30000
      dat = buildP $ binDiskSamples n
      res = range dat thr query
    res `shouldSatisfy` all (\(d, _) -> d < thr)





