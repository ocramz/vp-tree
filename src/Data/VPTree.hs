{-# language BangPatterns #-}
{-# language DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
{-# language LambdaCase #-}
{-# options_ghc -Wno-unused-imports -Wno-type-defaults -Wno-name-shadowing #-}
{- | Vantage point trees

Satisfying general proximity/similarity queries with metric trees - J. K. Uhlmann

http://faculty.missouri.edu/~uhlmannj/MetricTree91.pdf

Data structures and algorithms for nearest neighbor search in general metric spaces - P. N. Yianilos

http://web.cs.iastate.edu/~honavar/nndatastructures.pdf
-}
module Data.VPTree
  (VPTree
  -- * Construction
  , build
  -- * Lookup

  -- * Utilities
  -- ** Rendering trees
  , draw
  -- ** Random number generation
  , withIO, withST
  )
  where

import Data.Foldable (foldlM)
import Data.Ord (Down(..))
import Control.Monad.ST (ST, runST)
import Data.List (partition)
import Data.Maybe (fromMaybe)
import Text.Printf (PrintfArg, printf, PrintfType)

-- boxes
import qualified Text.PrettyPrint.Boxes as B (Box, render, emptyBox, vcat, hcat, text, top, bottom, center1)
-- deepseq
import Control.DeepSeq (NFData (rnf))
-- mwc-probability
import System.Random.MWC.Probability (Gen, Prob, withSystemRandom, asGenST, asGenIO, GenIO)
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
import Data.Vector.Generic.Mutable (MVector)
-- vector-algorithms
import qualified Data.Vector.Algorithms.Merge as V (sort, Comparison)

-- | Vantage point tree
data VPTree d a = Branch {-# UNPACK #-} !d !a !(VPTree d a) !(VPTree d a)
                -- | Sing !d !a
                | Tip -- (V.Vector a)
                deriving (Show, Functor, Foldable, Traversable)

instance (NFData d, NFData a) => NFData (VPTree d a) where
  rnf (Branch d x tl tr) = rnf d `seq` rnf x `seq` rnf tl `seq` rnf tr
  rnf Tip = ()



-- | Draw a tree
draw :: (Show a, PrintfArg d) => VPTree d a -> IO ()
draw = putStrLn . B.render . toBox

toBox :: (Show a, PrintfArg d) => VPTree d a -> B.Box
toBox = \case
  (Branch d x tl tr) ->
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





{- VPT construction and querying : 
http://stevehanov.ca/blog/index.php?id=130
-}

nearest distf x = go []
  where
    go acc Tip = acc
    go acc (Branch mu v ll rr)
      | xmu < 0 = go acc rr -- query point is outside the radius mu
      | xv < xmu = go acc ll 
      | otherwise = go ( (v, mu) : acc) ll -- FIXME double check this
      where
        xv = distf x v -- x to vantage point
        xmu = mu - xv  -- x to the outer shell


nearest' :: (Fractional t, Ord t) =>
            (p -> a -> t) -> p -> VPTree t a -> PQ.IntPSQ t a
nearest' distf x = go PQ.empty 0 (1/0)
  where
    go acc _ _ Tip = acc
    go acc i srad (Branch mu v ll rr)
      | xmu < 0 = go acc i srad rr -- query point is outside the radius mu
      | xv < xmu = go acc i srad ll -- FIXME double check this
      | otherwise = let
          acc' = PQ.insert i xv v acc
          srad' = min mu srad -- new search radius
          in go acc' (i + 1) srad' ll -- FIXME double check this
      where
        xv = distf x v -- x to vantage point
        xmu = mu - xv  -- x to the outer shell




-- newtype MaxPQ p x = MaxPQ (PQ.IntPSQ (Down p) x)

-- insert :: Ord p => Int -> p -> x -> MaxPQ p x -> MaxPQ p x
-- insert ix p x (MaxPQ pq )= MaxPQ $ PQ.insert ix (Down p) x pq







-- | Build a 'VPTree'
build :: (PrimMonad m, RealFrac b, Floating d, Ord d) =>
         (a -> a -> d) -- ^ Distance function
      -> b -- ^ Proportion of remaining dataset to sample at each level
      -> V.Vector a -- ^ Dataset
      -> Gen (PrimState m)
      -> m (VPTree d a)
build distf prop xs gen = do
  vp <- selectVP distf prop xs gen
  let
    branch l | length l <= 1 = pure Tip
             | otherwise = build distf prop l gen -- FIXME termination condition
    (mu, _) = medianDist distf vp xs
    (ll, rr) = V.partition (\x -> distf x vp < mu) xs
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


-- logVar :: Show a => String -> a -> IO ()
-- logVar w x = putStrLn $ unwords [w, "=", show x]

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
withIO :: (GenIO -> IO a) -- ^ Memory bracket for the PRNG
       -> IO a
withIO = withSystemRandom . asGenIO

-- | Runs a PRNG action in the 'ST' monad
--
-- NB : uses 'withSystemRandom' internally
withST :: (Gen s -> ST s a) -- ^ Memory bracket for the PRNG
       -> IO a
withST = withSystemRandom . asGenST


data P = P Double Double
instance Show P where
  show (P x y) = show (x,y)
pps :: V.Vector P
pps = V.fromList [P 0 1, P 1 2, P 3 4, P 2 4, P (- 2) 3, P (-10) 2, P (-8) 3, P 4 3, P 6 7,  P 10 10, P 20 2, P 15 5]
distp :: P -> P -> Double
distp (P x1 y1) (P x2 y2) = sqrt $ (x1 - x2)**2 + (y1 - y2)**2



