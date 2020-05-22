{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# language BangPatterns #-}
{-# language DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
{-# language LambdaCase #-}
{-# options_ghc -Wno-unused-imports -Wno-type-defaults -Wno-name-shadowing #-}
{- | Vantage point trees

Data structures and algorithms for nearest neighbor search in general metric spaces - P. N. Yianilos

http://web.cs.iastate.edu/~honavar/nndatastructures.pdf
-}
module Data.VPTree
  (VPTree
  -- * Construction
  , build
  -- * Nearest-neighbor query
  -- , nearest
  -- * Utilities
  -- ** Rendering trees
  , draw
  -- ** Random number generation
  -- *** IO 
  , withIO
  -- *** ST
  , withST, withST_
  )
  where

import Data.Foldable (foldlM)
import Data.Ord (Down(..))
import Data.Word (Word32)
import Control.Monad.ST (ST, runST)
import Data.List (partition)
import Data.Maybe (fromMaybe)
import Text.Printf (PrintfArg, printf, PrintfType)

-- boxes
import qualified Text.PrettyPrint.Boxes as B (Box, render, emptyBox, vcat, hcat, text, top, bottom, center1)
-- deepseq
import Control.DeepSeq (NFData (rnf))
-- mwc-probability
import qualified System.Random.MWC.Probability as P (Gen, Prob, withSystemRandom, asGenST, asGenIO, GenIO, create, initialize, samples, normal, bernoulli)
-- primitive
import Control.Monad.Primitive (PrimMonad(..), PrimState)
-- psqueues
import qualified Data.IntPSQ as PQ (IntPSQ, empty, size, insert, findMin, deleteMin)
-- sampling
import Numeric.Sampling (sample)
-- --transformers
-- import Control.Monad.Trans.State.Lazy (StateT, get, put, execStateT)
-- vector
import qualified Data.Vector as V (Vector, map, toList, replicate, partition, zipWith, head, tail, fromList, thaw, freeze, (!))
import qualified Data.Vector.Generic as VG (Vector(..))
import Data.Vector.Generic.Mutable (MVector)
-- vector-algorithms
import qualified Data.Vector.Algorithms.Merge as V (sort, Comparison)

import qualified Data.MaxPQ as MQ (MaxPQ, empty, insert, size, findMax)


-- | Vantage point tree
data VPTree d a = Bin  !d !a !(VPTree d a) !(VPTree d a)
                -- | Sing !d !a
                | Tip
                deriving (Show, Functor, Foldable, Traversable)

instance (NFData d, NFData a) => NFData (VPTree d a) where
  rnf (Bin d x tl tr) = rnf d `seq` rnf x `seq` rnf tl `seq` rnf tr
  rnf Tip = ()




{- VPT construction and querying : 
http://stevehanov.ca/blog/index.php?id=130
-}



-- | Query a 'VPTree' for nearest neighbors
--
-- NB : the distance function used here should be the same as the one used to construct the tree in the first place

-- nearest :: (Fractional d, Ord d) =>
--            (a -> a -> d) -- ^ Distance function
--         -> Int -- ^ Number of nearest neighbors to return
--         -> a -- ^ Query point
--         -> VPTree d a
--         -> PQ.IntPSQ d a
-- nearest distf k x = go PQ.empty 0
--   where
--     go acc _ _ | length acc == k = acc
--     go acc _ Tip = acc
--     go acc i (Bin mu v ll rr)
--       | xmu < 0 = go acc i rr -- query point is outside the radius mu
--       | otherwise = let
--           acc' = PQ.insert i xv v acc
--           in go acc' (succ i) ll
--       where
--         xv = distf x v -- x to vantage point
--         xmu = mu - xv  -- x to the outer shell


nearest :: (Fractional d, Ord d) =>
           (a -> a -> d) -- ^ Distance function
        -- -> Int -- ^ Number of nearest neighbors to return
        -> a -- ^ Query point
        -> VPTree d a
        -> PQ.IntPSQ d a
nearest distf x = go PQ.empty 0 (1/0)
  where
    go acc _ _ Tip = acc
    go acc i srad (Bin mu v ll rr)
      | d < srad' = go acc' (succ i) srad' ll
      | xmu < 0   = go acc  i        srad  rr
      | otherwise = go acc  i        srad  ll
      where
        acc' = PQ.insert i d v acc
        d = distf x v -- x to vantage point
        xmu = mu - d -- x to the outer shell
        srad' = min srad (abs xmu) -- new search radius



nearestIO distf k x = go PQ.empty 0
  where
    go acc i (Bin mu v ll rr) =
      let
        xv = distf x v -- x to vantage point
        xmu = mu - xv  -- x to the outer shell
      in
        if xmu < 0
        then
          do
            logVar "i" i
            logVar "mu" mu
            logVar "v" v
            logVar "xv := d(x, v)" xv
            logVar "mu - xv" xmu
            putStrLn "next : R\n"
            
            go acc i rr -- query point is outside the radius mu
        else
          do
            let acc' = PQ.insert i xv v acc
            logVar "i" i
            logVar "mu" mu
            logVar "v" v
            logVar "xv := d(x, v)" xv
            logVar "mu - xv" xmu
            -- logVar "acc'" acc'
            putStrLn "next : L\n" 

            go acc' (i + 1) ll
    go acc _ _ | length acc == k = pure acc
    go acc _ Tip = pure acc


-- nearest distf x = go PQ.empty 0 (1/0)
--   where
--     go acc _ _ Tip = acc
--     go acc i srad (Bin mu v ll rr)
--       | xmu < 0 = go acc i srad rr -- query point is outside the radius mu
      
--       -- | xv < xmu = go acc i srad ll 
--       -- | otherwise = let
--       --     acc' = PQ.insert i xv v acc
--       --     srad' = min mu srad -- new search radius
--       --     in go acc' (i + 1) srad' ll -- FIXME double check this
      
--       where
--         xv = distf x v -- x to vantage point
--         xmu = mu - xv  -- x to the outer shell





-- -- | keep track of the distance function used when constructing the tree
-- data VPT d a = VPT {
--     vpTree :: VPTree d a
--   , vptDistFun :: a -> a -> d
--                    }

-- -- nearestVPT :: (Fractional t, Ord t) => VPT t a -> a -> PQ.IntPSQ t a
-- nearestVPT (VPT t df) x = nearest df x t

-- buildVPT :: (PrimMonad m, RealFrac b, Floating d, Ord d) =>
--             (a -> a -> d)
--          -> b -> V.Vector a -> Gen (PrimState m) -> m (VPT d a)
-- buildVPT df prop xs gen = do
--   t <- build df prop xs gen
--   pure $ VPT t df



-- | Build a 'VPTree'
build :: (PrimMonad m, RealFrac b, Floating d, Ord d) =>
         (a -> a -> d) -- ^ Distance function
      -> b -- ^ Proportion of remaining dataset to sample at each level
      -> V.Vector a -- ^ Dataset
      -> P.Gen (PrimState m)
      -> m (VPTree d a)
build distf prop xs gen = do
  vp <- selectVP distf prop xs gen
  let
    (mu, _) = medianDist distf vp xs
    (ll, rr) = V.partition (\x -> distf x vp < mu) xs
    branch l | length l <= 1 = pure Tip
             | otherwise = build distf prop l gen -- FIXME termination condition
  ltree <- branch ll
  rtree <- branch rr
  pure $ Bin mu vp ltree rtree

-- | Select a vantage point
selectVP :: (PrimMonad m, RealFrac b, Foldable f, Ord d, Floating d) =>
            (a -> a -> d) -- ^ distance function
         -> b -- ^ proportion of dataset to sample
         -> f a -- ^ dataset
         -> P.Gen (PrimState m)
         -> m a
selectVP distf prop sset gen = do
  ps <- sampleV n sset gen
  let pstart = V.head ps
      ptail = V.tail ps
  snd <$> foldlM pickMu (0, pstart) ptail
  where
    n = floor (prop * fromIntegral ndata)
    ndata = length sset -- size of dataset at current level
    pickMu (spread_curr, p_curr) p = do
      ds <- sampleV n sset gen
      let (mu, dists) = medianDist distf p ds
          spread = variance dists (V.replicate n mu)
      if spread > spread_curr
        then pure (spread, p)
        else pure (spread_curr, p_curr)


logVar :: Show a => String -> a -> IO ()
logVar w x = putStrLn $ unwords [w, "=", show x]

-- | Sample _without_ replacement. Returns empty list if we ask for too many samples
sampleV :: (PrimMonad m, Foldable f) =>
           Int -- ^ Size of sample
        -> f a
        -> P.Gen (PrimState m) -> m (V.Vector a)
sampleV n xs g = V.fromList . fromMaybe [] <$> sample n xs g
{-# INLINE sampleV #-}

medianDist :: Ord d => (t -> p -> d) -> p -> V.Vector t -> (d, V.Vector d)
medianDist distf p ds = (mu, dists)
  where
    mu = median dists
    dists = V.map (`distf` p) ds
{-# INLINE medianDist #-}

median :: Ord a => V.Vector a -> a
median xs = sortV xs V.! floor (fromIntegral n / 2) -- FIXME when n is too small ?
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

-- | Runs a PRNG action in IO
--
-- NB : uses 'withSystemRandom' internally
withIO :: (P.GenIO -> IO a) -- ^ Memory bracket for the PRNG
       -> IO a
withIO = P.withSystemRandom . P.asGenIO

-- | Runs a PRNG action in the 'ST' monad, using a fixed seed
--
-- NB : uses 'create' internally
withST_ :: (forall s . P.Gen s -> ST s a) -- ^ Memory bracket for the PRNG
        -> a
withST_ st = runST $ do
  g <- P.create
  st g

-- | Runs a PRNG action in the 'ST' monad, using a given random seed
--
-- NB : uses 'initialize' internally
withST :: (VG.Vector v Word32) =>
          v Word32 -- ^ Random seed
       -> (forall s . P.Gen s -> ST s a) -- ^ Memory bracket for the PRNG
       -> a
withST seed st = runST $ do
  g <- P.initialize seed
  st g



-- | Draw a tree
--
-- NB : prints distance information up to two decimal digits
draw :: (Show a, PrintfArg d) => VPTree d a -> IO ()
draw = putStrLn . B.render . toBox

toBox :: (Show a, PrintfArg d) => VPTree d a -> B.Box
toBox = \case
  (Bin d x tl tr) ->
    nodeBox x d `stack` (toBox tl `byside` toBox tr)
  -- Sing d x -> nodeBox x d
  Tip -> txt "*"
  where nodeBox x d = txt (printf "%s,%5.2f" (show x) d)

txt :: String -> B.Box
txt t = spc `byside` B.text t `byside` spc
  where spc = B.emptyBox 1 1

byside :: B.Box -> B.Box -> B.Box
byside l r = B.hcat B.top [l, r]

stack :: B.Box -> B.Box -> B.Box
stack t b = B.vcat B.center1 [t, b]






-- test data

data P = P Double Double
instance Show P where
  show (P x y) = printf "(%2.2f, %2.2f)" x y --show (x,y)

coin :: PrimMonad m => P.Prob m Bool
coin = P.bernoulli 0.5

binMixture :: PrimMonad m => Double -> Double -> Double -> Double -> P.Prob m Double
binMixture mu1 mu2 sig1 sig2 = do
  b <- coin
  if b
    then
      P.normal mu1 sig1
    else
      P.normal mu2 sig2

genN2 :: Int -> V.Vector P
genN2 n = withST_ $ \g -> do
  xs <- P.samples n (binMixture 0 10 1 1) g
  ys <- P.samples n (binMixture 0 10 1 1) g
  pure $ V.fromList $ zipWith P xs ys

genNormalP :: Double -> Double -> Int -> V.Vector P
genNormalP mu sig n = withST_ $ \g -> do
  xs <- P.samples n (P.normal mu sig) g
  ys <- P.samples n (P.normal mu sig) g
  pure $ V.fromList $ zipWith P xs ys

vptree :: RealFrac p => V.Vector P -> p -> VPTree Double P
vptree ps p = withST_ $ build distp p ps

t1 = vptree pps

t2 = vptree pps2

-- tpps :: RealFrac p => p -> VPTree Double P
-- tpps p = withST_ $ build distp p pps

pps, pps2 :: V.Vector P
pps = genNormalP 0 3 50

pps2 = genN2 50 -- binary mixture of 2d normals

-- pps :: V.Vector P
-- pps = V.fromList [P 0 1, P 1 2, P 3 4, P 2 4, P (- 2) 3, P (-10) 2, P (-8) 3, P 4 3, P 6 7,  P 10 10, P 20 2, P 15 5]
distp :: P -> P -> Double
distp (P x1 y1) (P x2 y2) = sqrt $ (x1 - x2)**2 + (y1 - y2)**2
