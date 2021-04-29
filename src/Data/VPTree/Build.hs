{-# options_ghc -Wno-unused-imports #-}
{-# options_ghc -Wno-type-defaults #-}
module Data.VPTree.Build (build
                         -- * Internal
                         , buildVT
                         ) where

import Control.Monad.ST (ST, runST)
import qualified Data.Foldable as F (Foldable(..))
import Data.Foldable (foldlM)
import Data.Maybe (fromMaybe)

-- containers
import qualified Data.Set as S (Set, fromList, difference)
-- import qualified Data.Sequence as SQ (Seq)
-- deepseq
-- import Control.DeepSeq (NFData (rnf))
-- mwc-probability
import qualified System.Random.MWC.Probability as P (Gen, Prob, withSystemRandom, asGenIO, GenIO, create, initialize, sample, samples, normal, bernoulli)
-- primitive
import Control.Monad.Primitive (PrimMonad(..), PrimState)
-- sampling
import Numeric.Sampling (sample)
-- vector
import qualified Data.Vector as V (Vector, map, filter, length, toList, replicate, partition, zipWith, head, tail, fromList, thaw, freeze, (!), foldl)
-- import qualified Data.Vector.Generic as VG (Vector(..))
-- import Data.Vector.Generic.Mutable (MVector)
-- vector-algorithms
import qualified Data.Vector.Algorithms.Merge as V (sort, Comparison)

import Data.VPTree.Internal (VT(..), VPTree(..), withST_)

-- * Construction

-- | Build a 'VPTree'
--
-- Implementation detail : construction of a VP-tree requires a randomized algorithm, but we run that in the ST monad so the result is pure
build :: (RealFrac p, Floating d, Ord d, Eq a) =>
         (a -> a -> d) -- ^ distance function
      -> p -- ^ proportion of remaining dataset to sample at each level
      -> V.Vector a -- ^ dataset
      -> VPTree d a
build distf prop xss = withST_ $ \gen -> do
  vt <- buildVT distf prop xss gen
  pure $ VPT vt distf


-- | Build a VP-tree with the given distance function
buildVT :: (PrimMonad m, RealFrac b, Floating d, Eq a, Ord d) =>
           (a -> a -> d) -- ^ distance function
        -> b -- ^ proportion of remaining dataset to sample at each level
        -> V.Vector a -- ^ dataset
        -> P.Gen (PrimState m) -- ^ PRNG
        -> m (VT d a)
buildVT distf prop xss gen = go xss
  where
    branch l | length l == 1 = pure $ Tip (V.head l)
             | null l = pure Nil
             | otherwise = go l
    go xs = do
      vp <- selectVP distf prop xs gen
      let
        xs' = V.filter (/= vp) xs
        mu = median $ V.map (`distf` vp) xs' -- median distance to the vantage point
        (ll, rr) = V.partition (\x -> distf x vp < mu) xs'
        
      ltree <- branch ll
      rtree <- branch rr
      pure $ Bin mu vp ltree rtree


-- | Select a vantage point
selectVP :: (PrimMonad m, RealFrac b, Ord d, Floating d) =>
            (a -> a -> d) -- ^ distance function
         -> b -- ^ proportion of dataset to sample
         -> V.Vector a -- ^ dataset
         -> P.Gen (PrimState m)
         -> m a
selectVP distf prop sset gen = do

  (pstart, pstail, psc) <- vpRandSplitInit n sset gen

  let pscl = V.toList psc

      pickMu (spread_curr, p_curr) p = do
        ds <- sampleId n2 pscl gen -- sample n2 < n points from psc
        let
          dsv = V.fromList ds
          spread = varianceWrt distf p dsv

        if spread > spread_curr
          then pure (spread, p)
          else pure (spread_curr, p_curr)

  snd <$> foldlM pickMu (0, pstart) pstail

  where
    n = floor (prop * fromIntegral ndata)
    n2 = floor (prop * fromIntegral n)
    ndata = length sset -- size of dataset at current level

vpRandSplitInit :: PrimMonad m =>
                   Int
                -> V.Vector a
                -> P.Gen (PrimState m)
                -> m (a, V.Vector a, V.Vector a) -- (head of C, tail of C, complement of C)
vpRandSplitInit n sset gen = do
  (ps, psc) <- randomSplit n sset gen
  (pstartv, pstail) <- randomSplit 1 ps gen -- Pick a random starting point from ps
  let pstart = V.head pstartv
  pure (pstart, pstail, psc)

-- | Sample a random split of the dataset
--
-- Invariant : the concatenation of the two resulting vectors is a permutation of the input vector
--
-- NB : the second vector in the result tuple will be empty if the requested sample size is larger than the input vector
randomSplit :: (PrimMonad f) =>
               Int -- ^ Size of sample
            -> V.Vector a -- ^ dataset
            -> P.Gen (PrimState f) -- ^ PRNG
            -> f (V.Vector a, V.Vector a)
randomSplit n vv gen = split <$> sampleId n ixs gen
  where
    split xs = (vxs, vxsc)
      where
        ixss = S.fromList xs
        ixsc = S.fromList ixs `S.difference` ixss
        vxs  = pickItems ixss
        vxsc = pickItems ixsc
    m = V.length vv
    ixs = [0 .. m - 1]
    pickItems = V.fromList . foldl (\acc i -> vv V.! i : acc) []



-- | Sample _without_ replacement. Returns the input list if the required sample size is too large
sampleId :: (PrimMonad m, Foldable t) =>
            Int -- ^ Size of sample
         -> t a
         -> P.Gen (PrimState m)
         -> m [a]
sampleId n xs g = fromMaybe (F.toList xs) <$> sample n xs g
{-# INLINE sampleId #-}

-- | Variance of the distance btw the dataset and a given query point
--
-- NB input vector must have at least 1 element
varianceWrt :: (Floating a, Ord a) =>
               (t -> p -> a) -- ^ distance function
            -> p -- ^ query point
            -> V.Vector t
            -> a
varianceWrt distf p ds = variance dists (V.replicate n2 mu) where
  dists = V.map (`distf` p) ds
  mu = median dists
  n2 = V.length ds
{-# INLINE varianceWrt #-}

-- | NB input vector must have at least 1 element
median :: Ord a => V.Vector a -> a
median xs
  | null xs = error "median : input array must have at least 1 element"
  | n == 1 = V.head xs
  | otherwise = sortV xs V.! floor (fromIntegral n / 2)
  where n = length xs
{-# INLINE median #-}

variance :: (Floating a) => V.Vector a -> V.Vector a -> a
variance xs mus = mean $ V.zipWith sqdiff xs mus
  where
    sqdiff x y = (x - y) ** 2
{-# INLINE variance #-}

mean :: (Fractional a) => V.Vector a -> a
mean xs = sum xs / fromIntegral (length xs)
{-# INLINE mean #-}

sortV :: Ord a => V.Vector a -> V.Vector a
sortV v = runST $ do
  vm <- V.thaw v
  V.sort vm
  V.freeze vm
{-# INLINE sortV #-}
