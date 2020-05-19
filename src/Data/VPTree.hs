{-# language BangPatterns #-}
{-# language DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
{-# language LambdaCase #-}
{-# options_ghc -Wno-unused-imports -Wno-type-defaults -Wno-name-shadowing #-}
{- | Vantage point trees

Data structures and algorithms for nearest neighbor search in general metric spaces - P. N. Yianilos

http://web.cs.iastate.edu/~honavar/nndatastructures.pdf
-}
module Data.VPTree
  (VPTree, build)
  where

import Data.Foldable (foldlM)
import Control.Monad.ST (ST, runST)
import Data.List (partition)
import Data.Maybe (fromMaybe)

-- deepseq
import Control.DeepSeq (NFData (rnf))
-- mwc-probability
import System.Random.MWC.Probability (Gen, Prob, withSystemRandom, asGenST, asGenIO, GenIO)
-- primitive
import Control.Monad.Primitive (PrimMonad(..), PrimState)
-- sampling
import Numeric.Sampling (sample)
-- --transformers
-- import Control.Monad.Trans.State.Lazy (StateT, get, put, execStateT)
-- vector
import qualified Data.Vector as V (Vector, map, replicate, partition, zipWith, head, fromList, thaw, freeze, (!))
import Data.Vector.Generic.Mutable (MVector)
-- vector-algorithms
import qualified Data.Vector.Algorithms.Merge as V (sort, Comparison)


data VPTree d a = Branch {-# UNPACK #-} !d !a !(VPTree d a) !(VPTree d a)
                | Tip
                deriving (Show, Functor, Foldable, Traversable)


{- VPT construction and querying : 
http://stevehanov.ca/blog/index.php?id=130
-}

-- nearest distf x = go
--   where go = \case
--           Branch tau v ll rr ->
--             let d = distf x v
--             in
--               if d < tau
--               then go ll
--               else go rr


data P = P Double Double deriving (Show)
pps :: V.Vector P
pps = V.fromList [P 0 1, P 1 2, P 3 4, P 2 4, P (- 2) 3, P (-10) 2, P 4 3, P 10 10, P 20 2]
distp :: P -> P -> Double
distp (P x1 y1) (P x2 y2) = sqrt $ x1*x2 + y1*y2


-- build' :: (Floating d, Ord d) =>
--           (a -> a -> d) -> Float -> V.Vector a -> IO (VPTree d a)
-- build' df p xs = withST (build df p xs)

build' :: (RealFrac b, Floating d, Show a, Ord d) =>
          (a -> a -> d) -> b -> V.Vector a -> IO (VPTree d a)
build' df p xs = withIO $ build df p xs


-- | Build a 'VPTree'
build :: (PrimMonad m, RealFrac b, Floating d, Ord d) =>
         (a -> a -> d) -- ^ Distance function
      -> b -- ^ Proportion of dataset to sample
      -> V.Vector a -- ^ Dataset
      -> Gen (PrimState m) -> m (VPTree d a)
build distf prop xs gen = do
  vp <- selectVP distf prop xs gen
  -- putStrLn $ show vp -- debug
  let
    branch l | length l <= 2 = pure Tip
             | otherwise = build distf prop l gen
    (mu, _) = medianDist distf vp xs
    (ll, rr) = V.partition (\x -> distf x vp < mu) xs
  -- putStrLn $ unwords ["LL :", show ll]
  -- putStrLn $ unwords ["RR :", show rr]
  ltree <- branch ll
  rtree <- branch rr
  pure $ Branch mu vp ltree rtree

-- | Select a vantage point
selectVP :: (PrimMonad m, RealFrac b, Foldable f, Ord d, Floating d) =>
            (a -> a -> d) -- ^ distance function
         -> b -- ^ proportion of dataset to sample
         -> f a -- ^ dataset
         -> Gen (PrimState m)
         -> m a
selectVP distf prop sset gen = do
  ps <- sampleV n sset gen
  let pstart = V.head ps
  snd <$> foldlM pickMu (0, pstart) ps
  where
    n = floor (prop * fromIntegral ndata)
    ndata = length sset -- size of dataset at current level
    pickMu (best, pcurr) p = do
      ds <- sampleV n sset gen
      let (mu, dists) = medianDist distf p ds
          spread = variance dists (V.replicate ndata mu)
      if spread > best
        then pure (spread, p)
        else pickMu (spread, pcurr) p

-- | Sample _without_ replacement. Returns empty list if we ask for too many samples
sampleV :: (PrimMonad m, Foldable f) =>
           Int -- ^ Size of sample
        -> f a -> Gen (PrimState m) -> m (V.Vector a)
sampleV n xs g = V.fromList . fromMaybe [] <$> sample n xs g
{-# INLINE sampleV #-}

medianDist :: Ord d => (t -> p -> d) -> p -> V.Vector t -> (d, V.Vector d)
medianDist distf p ds = (mu, dists)
  where
    mu = median dists
    dists = V.map (`distf` p) ds
{-# INLINE medianDist #-}

median :: Ord a => V.Vector a -> a
median xs = sortV xs V.! floor (fromIntegral n / 2)
  where n = length xs
{-# INLINE median #-}

variance :: (Floating a) => V.Vector a -> V.Vector a -> a
variance xs mu = mean $ V.zipWith sqdiff xs mu
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

withIO :: (GenIO -> IO a) -> IO a
withIO = withSystemRandom . asGenIO

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


