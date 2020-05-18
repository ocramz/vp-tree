{-# language BangPatterns #-}
{-# language DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
{-# language LambdaCase #-}
{-# options_ghc -Wno-unused-imports #-}
{- | Vantage point trees

Data structures and algorithms for nearest neighbor search in general metric spaces - P. N. Yianilos

http://web.cs.iastate.edu/~honavar/nndatastructures.pdf
-}
module Data.VPTree (VPTree, build) where

import Data.Foldable (foldlM)
import Control.Monad.ST (ST, runST)
import Data.List (partition)

-- deepseq
import Control.DeepSeq (NFData (rnf))
-- mwc-probability
import System.Random.MWC.Probability (Gen, Prob, withSystemRandom, asGenST)
-- primitive
import Control.Monad.Primitive (PrimMonad(..), PrimState)
-- sampling
import Numeric.Sampling (sample)
-- --transformers
-- import Control.Monad.Trans.State.Lazy (StateT, get, put, execStateT)
-- vector
import Data.Vector (Vector, fromList, thaw, freeze, (!))
import Data.Vector.Generic.Mutable (MVector)
-- vector-algorithms
import qualified Data.Vector.Algorithms.Merge as V (sort, Comparison)


data VPTree d a = Branch {-# UNPACK #-} !d !a !(VPTree d a) !(VPTree d a)
                | Tip
                deriving (Show, Functor, Foldable, Traversable)


nearest distf x = go
  where go = \case
          Branch tau v ll rr ->
            let d = distf x v
            in
              if d < tau
              then go ll
              else go rr


data P = P Double Double deriving (Show)
ps = [P 0 1, P 1 2, P 3 4, P 2 4, P (- 2) 3, P (-10) 2, P 4 3, P 10 10, P 20 2]
distp :: P -> P -> Double
distp (P x1 y1) (P x2 y2) = sqrt $ x1*x1 + y2*y2


build' :: (Floating d, Ord d) =>
          (a -> a -> d) -> Int -> [a] -> IO (VPTree d a)
build' df n xs = withST (build df n xs)

-- | Build a 'VPTree'
build :: (PrimMonad m, Floating d, Ord d) =>
         (a -> a -> d) -- ^ Distance function
      -> Int -- ^ Size of random sample
      -> [a] -- ^ Dataset
      -> Gen (PrimState m)
      -> m (VPTree d a)
build distf n xs gen = do
  vp <- selectVP distf n xs gen
  let
    nxs = length xs
    branch l | length l <= 1 = pure Tip
             | otherwise = build distf n l gen
    (mu, _) = medianDist distf nxs vp xs
    (ll, rr) = partition (\x -> distf x vp < mu) xs
  ltree <- branch ll
  rtree <- branch rr
  pure $ Branch mu vp ltree rtree

-- | Select a vantage point
selectVP :: (PrimMonad m, Ord d, Floating d) =>
            (a -> a -> d) -- ^ distance function
         -> Int -- ^ size of random sample
         -> [a] -- ^ dataset
         -> Gen (PrimState m)
         -> m a
selectVP distf n sset gen = do
  ps <- sample' n sset gen
  let pbest = head ps
  snd <$> foldlM pickMu (0, pbest) ps
  where
    pickMu (best, pcurr) p = do
      ds <- sample' n sset gen
      let (mu, dists) = medianDist distf n p ds
          spread = variance n dists (repeat mu)
      if spread > best
        then pure (spread, p)
        else pickMu (spread, pcurr) p

sample' :: PrimMonad m => Int -> [a] -> Gen (PrimState m) -> m [a]
sample' n xs g = do
  sm <- sample n xs g
  case sm of
    Nothing -> pure xs
    Just s  -> pure s

medianDist :: Ord d => (t -> p -> d) -> Int -> p -> [t] -> (d, [d])
medianDist distf n p ds = (mu, dists)
  where
    mu = median n dists
    dists = map (`distf` p) ds

median :: Ord a => Int -> [a] -> a
median n xs = sort xsl ! floor (fromIntegral n / 2)
  where xsl = fromList xs

variance :: (Floating a) => Int -> [a] -> [a] -> a
variance n xs mu = mean n $ zipWith sqdiff xs mu
  where
    sqdiff x y = (x - y) ** 2

mean :: (Foldable t, Fractional a) => Int -> t a -> a
mean n xs = sum xs / fromIntegral n

sort :: Ord a => Vector a -> Vector a 
sort v = runST $ do
  vm <- thaw v
  V.sort vm
  freeze vm


withST :: (Gen s -> ST s a)
       -> IO a
withST = withSystemRandom . asGenST



{-
-- | A priority search queue with @Int@ keys and priorities of type @p@ and
-- values of type @v@. It is strict in keys, priorities and values.
data IntPSQ p v
    = Bin {-# UNPACK #-} !Key !p !v {-# UNPACK #-} !Mask !(IntPSQ p v) !(IntPSQ p v)
    | Tip {-# UNPACK #-} !Key !p !v
    | Nil
    deriving (Foldable, Functor, Show, Traversable)

instance (NFData p, NFData v) => NFData (IntPSQ p v) where
    rnf (Bin _k p v _m l r) = rnf p `seq` rnf v `seq` rnf l `seq` rnf r
    rnf (Tip _k p v)        = rnf p `seq` rnf v
    rnf Nil                 = ()
-}


