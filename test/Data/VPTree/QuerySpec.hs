{-# options_ghc -Wno-unused-imports #-}
module Data.VPTree.QuerySpec where

import Text.Printf (printf)

-- mwc-probability
import qualified System.Random.MWC.Probability as P (Gen, Prob, withSystemRandom, asGenIO, GenIO, create, initialize, sample, samples, normal, bernoulli, uniformR)
-- primitive
import Control.Monad.Primitive (PrimMonad(..), PrimState)
-- vector
import qualified Data.Vector as V (Vector, map, filter, length, toList, replicate, partition, zipWith, head, tail, fromList, thaw, freeze, (!), foldl)
import qualified Data.Vector.Generic as VG (Vector(..))

import Test.Hspec (Spec, describe, it, shouldBe)
-- import Test.Hspec.QuickCheck

import Data.VPTree.Build (build)
import Data.VPTree.Draw (draw)
import Data.VPTree.Internal (VT, VPTree, withST, withST_, withIO)
import Data.VPTree.Query (range)

spec :: Spec
spec = describe "Data.VPTree.Query" $ do
  it "works" $ do
    1 `shouldBe` 1



-- test data

data P = P Double Double deriving (Eq)
instance Show P where
  show (P x y) = printf "(%2.2f, %2.2f)" x y --show (x,y)
instance Num P where
  fromInteger i = P (fromIntegral i) (fromIntegral i)

(.+.) :: P -> P -> P
P x1 y1 .+. P x2 y2 = P (x1 + x2) (y1 + y2)

distp :: P -> P -> Double
distp (P x1 y1) (P x2 y2) = sqrt $ (x1 - x2)**2 + (y1 - y2)**2



t2, t3 :: VPTree Double P
t3 = buildP $ genN3 12
t2 = buildP $ genN2 12

genN1, genN2, genN3 :: Int -> V.Vector P
genN2 n = V.fromList $ withST_ (P.samples n (binMix 0 20 1 1))

genN1 n = V.fromList $ withST_ (P.samples n (isoNormal2d 0 1))

genN3 n = V.fromList $ withST_ $ P.samples n (binDisk 1 1 0 5)

-- | binary mixture of isotropic 2d normal distribs
binMix :: PrimMonad m =>
          Double -> Double -> Double -> Double -> P.Prob m P
binMix mu1 mu2 sig1 sig2 = do
  b <- coin
  if b
    then isoNormal2d mu1 sig1
    else isoNormal2d mu2 sig2

coin :: PrimMonad m => P.Prob m Bool
coin = P.bernoulli 0.5

isoNormal2d :: PrimMonad m => Double -> Double -> P.Prob m P
isoNormal2d mu sig = P <$> P.normal mu sig <*> P.normal mu sig

binDisk :: PrimMonad m => Double -> Double -> P -> P -> P.Prob m P
binDisk r0 r1 p0 p1 = do
  b <- coin
  if b
    then uniformDisk r0 p0
    else uniformDisk r1 p1

-- point in a disk of radius r and centered at P
uniformDisk :: PrimMonad m => Double -> P -> P.Prob m P
uniformDisk rmax p = do
  r <- P.uniformR (0, rmax)
  aa <- P.uniformR (0, 2 * pi)
  let
    x = r * cos aa
    y = r * sin aa
    p0 = P x y
  pure $ p0 .+. p

buildP :: V.Vector P -> VPTree Double P
buildP = build distp (1.0 :: Double)
