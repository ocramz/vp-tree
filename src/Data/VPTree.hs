{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# language BangPatterns #-}
{-# language DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
{-# language LambdaCase #-}
{-# language DeriveDataTypeable #-}

{-# options_ghc -Wno-type-defaults #-}
{-# options_ghc -Wno-unused-top-binds #-}
{-# options_ghc -Wno-unused-imports #-}
{-# options_ghc  -Wno-name-shadowing #-}
{- | Vantage point trees

Data structures and algorithms for nearest neighbor search in general metric spaces - P. N. Yianilos

http://web.cs.iastate.edu/~honavar/nndatastructures.pdf
-}
module Data.VPTree
  (VPTree
  -- * Construction
  -- , build
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

import Control.Monad.IO.Class (MonadIO(..))
import Data.Foldable (foldlM)
import qualified Data.Foldable as F (Foldable(..))
-- import Data.Ord (Down(..))
import Data.Word (Word32)
-- import Control.Exception (Exception(..))
import Control.Monad.ST (ST, runST)
import Data.Maybe (fromMaybe)
import Data.Typeable (Typeable)
import Text.Printf (PrintfArg, printf)

-- boxes
import qualified Text.PrettyPrint.Boxes as B (Box, render, emptyBox, vcat, hcat, text, top, bottom, center1)
-- containers
import qualified Data.Set as S (Set, fromList, difference)
import qualified Data.Sequence as SQ (Seq)
-- deepseq
import Control.DeepSeq (NFData (rnf))
-- depq
import qualified Data.DEPQ as DQ (DEPQ, empty, size, insert, bottomK)
-- exceptions
import Control.Monad.Catch (MonadThrow(..))
-- mwc-probability
import qualified System.Random.MWC.Probability as P (Gen, Prob, withSystemRandom, asGenIO, GenIO, create, initialize, samples, normal, bernoulli)
-- primitive
import Control.Monad.Primitive (PrimMonad(..), PrimState)
-- psqueues
import qualified Data.IntPSQ as PQ (IntPSQ, empty, size, insert, findMin, deleteMin)
-- sampling
import Numeric.Sampling (sample)
-- transformers
import Control.Monad.Trans.Class (MonadTrans(..))
import Control.Monad.Trans.Maybe (MaybeT(..), runMaybeT)
-- import Control.Monad.Trans.State.Lazy (StateT, get, put, execStateT)
-- vector
import qualified Data.Vector as V (Vector, map, filter, length, toList, replicate, partition, zipWith, head, tail, fromList, thaw, freeze, (!))
import qualified Data.Vector.Generic as VG (Vector(..))
import Data.Vector.Generic.Mutable (MVector)
-- vector-algorithms
import qualified Data.Vector.Algorithms.Merge as V (sort, Comparison)

-- import qualified Data.MaxPQ as MQ (MaxPQ, empty, insert, size, findMax, toList)


-- | Vantage point tree
data VPTree d a = VPT {
  vpTree :: VT d a
  , vptDistFun :: a -> a -> d -- ^ Distance function used to construct the tree
                   }
instance (Eq d, Eq a) => Eq (VPTree d a) where
  (VPT t1 _) == (VPT t2 _) = t1 == t2

instance (Show d, Show a) => Show (VPTree d a) where
  show (VPT t _) = show t


-- nearest :: (Num d, Ord d) =>
--            VPTree d a
--         -> Int
--         -> a
--         -> PQ.IntPSQ d a
-- nearest (VPT t df) k x = nearestVT df k t x

-- -- -- build :: (PrimMonad m, RealFrac b, Floating d, Ord d) =>
-- -- --          (a -> a -> d)
-- -- --       -> b
-- -- --       -> V.Vector a
-- -- --       -> P.Gen (PrimState m)
-- -- --       -> m (VPTree d a)
-- build df prop xs gen = do
--   mt <- buildVT df prop xs gen
--   pure $ (`VPT` df) <$> mt

-- | Vantage point tree (internal representation)
data VT d a = Bin  !d !a !(VT d a) !(VT d a)
            | Tip a
            | Nil
            deriving (Eq, Show, Functor, Foldable, Traversable)

instance (NFData d, NFData a) => NFData (VT d a) where
  rnf (Bin d x tl tr) = rnf d `seq` rnf x `seq` rnf tl `seq` rnf tr
  rnf (Tip x) = rnf x
  rnf Nil = ()




{-
variable tau keeps track of closest neighbour yet encounteres

subtrees are then pruned when the metric information stored in the tree suffices to prove that further consideration is futile, i.e. cannot yield a closer neighbor
-}

-- -- nearestVT :: (Ord p1, Fractional p1) =>
-- --              (p2 -> v -> p1) -> Int -> VT p1 v -> p2 -> SQ.Seq (Int, p1, v)
-- nearestVT :: (Ord p1, Fractional p1) =>
--              (p2 -> a -> p1) -> p2 -> VT p1 a -> DQ.DEPQ p1 a
-- nearestVT distf x = z
--   where
--     z = go DQ.empty 0 tau0
--     tau0 = 1/0 -- initial search radius
--     go acc _ _ Tip = acc
--     go acc i tau (Bin mu v ll rr)
--       | xmu < 0 = go acc i tau' rr -- query point is in outer half-population
--       | d < tau = go acc' (succ i) tau' ll
--       | otherwise = go acc i tau' ll
--       where
--         d    = distf x v -- x to vp
--         xmu  = mu - d -- x to outer shell
--         acc' = DQ.insert i d v acc
--         tau' = min tau d -- updated search radius



-- nearest1 :: (Ord d, Fractional d) =>
--             (a -> a -> d) -> a -> VT d a -> Maybe a
-- nearest1 distf x = go 0 tau0
--   where
--     tau0 = 1/0 -- initial search radius
--     go _ _ Tip = Nothing
--     go i tau (Bin mu v ll rr)
--       | xmu < 0 = go i tau' rr -- query point is in outer half-population
--       | d < tau = Just v
--       | otherwise = go i tau' ll
--       where
--         d    = distf x v -- x to vp
--         xmu  = mu - d -- x to outer shell
--         tau' = min tau d -- updated search radius


-- nearestIO1 distf x = go tau0
--   where
--     tau0 = 1/0 -- initial search radius
--     go _ (Tip _) = pure Nothing
--     go tau (Bin mu v ll rr) = do
--       logVar "mu" mu
--       logVar "tau" tau
--       logVar "d" d
--       logVar "xmu" xmu
--       if xmu < 0
--         then do
--           putStrLn "next : R\n"
--           go tau' rr -- query point is in outer half-population
--         else if d < tau
--         then do
--           logVar "v" v
--           pure $ Just v
--         else do
--           putStrLn "next : L\n"
--           go tau' ll
--       where
--         d    = distf x v -- x to vp
--         xmu  = mu - d -- x to outer shell
--         tau' = min tau d -- updated search radius


-- | Query a 'VPTree' for nearest neighbors
--
-- NB : the distance function used here should be the same as the one used to construct the tree in the first place





-- nearest :: (Fractional d, Ord d) =>
--            (a -> a -> d) -- ^ Distance function
--         -- -> Int -- ^ Number of nearest neighbors to return
--         -> a -- ^ Query point
--         -> VPTree d a
--         -> PQ.IntPSQ d a
-- nearest distf x = go PQ.empty 0 (1/0)
--   where
--     go acc _ _ Tip = acc
--     go acc i srad (Bin mu v ll rr)
--       | d < srad' = go acc' (succ i) srad' ll
--       | xmu < 0   = go acc  i        srad  rr
--       | otherwise = go acc  i        srad  ll
--       where
--         acc' = PQ.insert i d v acc
--         d = distf x v -- x to vantage point
--         xmu = mu - d -- x to the outer shell
--         srad' = min srad (abs xmu) -- new search radius

-- nearestVT :: (Num d, Ord d) =>
--              (a -> a -> d)
--           -> Int
--           -> VT d a
--           -> a
--           -> PQ.IntPSQ d a
-- nearestVT distf k tr x = go PQ.empty 0 maxd0 tr
--   where
--     maxd0 = 0 -- initial search radius
--     go acc _ _    Tip              = acc
--     go acc i maxd (Bin mu v ll rr)
--       | xmu < 0 = go acc i maxd' rr -- query point is in outer half-population
--       | otherwise =
--         let
--           q1 = xmu > maxd' -- x is farther from the outer shell than farthest point
--           q2 = PQ.size acc == k
--         in if q1 || q2
--            then acc
--            else go acc' (succ i) maxd' ll
--       where
--         d     = distf x v -- x to vp
--         xmu   = mu - d -- x to outer shell
--         acc'  = PQ.insert i d v acc
--         maxd' = max maxd d -- next search radius

logVar :: (MonadIO io, Show a) => String -> a -> io ()
logVar w x = liftIO $ putStrLn $ unwords [w, "=", show x]

{-
At any given step we are working with a node of the tree that has a

vantage point v
threshold distance mu.

The query point x will be some distance d from v.

If d is less than mu then use the algorithm recursively to search the subtree of the node that contains the points closer to v than mu; otherwise recurse to the subtree of the node that contains the points that are farther than the vantage point than mu.

If the recursive use of the algorithm finds a neighboring point n with distance to x that is less than |mu − d| then it cannot help to search the other subtree of this node; the discovered node n is returned. Otherwise, the other subtree also needs to be searched recursively.
-}

-- nnnn distf k tr x = z
--   where
--     (z, _, _) = go PQ.empty 0 maxd0 tr
--     maxd0 = 0
--     go acc i maxd Tip = (acc, i, maxd)
--     go acc i maxd (Bin mu v ll rr)
--       | q1 || q2 = go acc' (succ i)  maxd' ll -- x closer to v than to shell
--       | d < mu =   -- x inside shell but not closer to v
--         let
--           (accl, il, maxdl) = go acc i maxd' ll
--         in go accl il maxdl rr
--       | otherwise = go acc i maxd' rr -- x outside shell
--       where
--         d = distf x v
--         xmu = mu - d
--         acc' = PQ.insert i d v acc
--         maxd' = max maxd (abs xmu) -- next search radius
--         q1 = d < xmu
--         q2 = PQ.size acc == k









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





-- | Build a 'VPTree'
-- buildVT :: (PrimMonad m, RealFrac b, Floating d, Ord d, Eq a) =>
--            (a -> a -> d) -- ^ Distance function
--         -> b -- ^ Proportion of remaining dataset to sample at each level
--         -> V.Vector a -- ^ Dataset
--         -> P.Gen (PrimState m)
--         -> m (VT d a)
buildVT distf prop xss gen = go xss
  where
    go xs = do
      mm <- selectVP distf prop xs gen
      case mm of
        Nothing -> pure Nil
        Just (mu, vp) -> do
          let
            xs' = V.filter (/= vp) xs
            (ll, rr) = V.partition (\x -> distf x vp < mu) xs'
            branch l | length l == 1 = pure $ Tip (V.head l)
                     | null l = pure Nil
                     | otherwise = go l
          ltree <- branch ll
          rtree <- branch rr
          pure $ Bin mu vp ltree rtree


-- | Select a vantage point
selectVP :: (PrimMonad m, RealFrac b, Ord d, Floating d) =>
            (a -> a -> d) -- ^ distance function
         -> b -- ^ proportion of dataset to sample
         -> V.Vector a -- ^ dataset
         -> P.Gen (PrimState m)
         -> m (Maybe (d, a))
selectVP distf prop sset gen = runMaybeT $ do
  (ps, psc) <- randomSplit n sset gen
  (pstartv, ptail) <- randomSplit 1 ps gen -- Pick a random starting point from ps
  let pstart = V.head pstartv
      pickMu (spread_curr, p_curr) p = do
        ds <- sampleV n2 psc gen -- sample n2 < n points from psc
        (mu, dists) <- liftMaybeT $ medianDist distf p ds
        let spread = variance dists (V.replicate n2 mu)
        if spread > spread_curr
          then pure (spread, p)
          else pure (spread_curr, p_curr)
  foldlM pickMu (0, pstart) ptail
  where
    n = floor (prop * fromIntegral ndata)
    n2 = floor (prop * fromIntegral n)
    ndata = length sset -- size of dataset at current level

selectVP' distf prop sset gen = runMaybeT $ do
  (ps, psc) <- randomSplit n sset gen
  (pstartv, ptail) <- randomSplit 1 ps gen -- Pick a random starting point from ps
  let pstart = V.head pstartv
      pickMu (spread_curr, p_curr) p = do
        logVar "p_curr" p_curr
        ds <- sampleV n2 psc gen -- sample n2 < n points from psc
        (mu, dists) <- liftMaybeT $ medianDist distf p ds
        let spread = variance dists (V.replicate n2 mu)
        if spread > spread_curr
          then pure (spread, p)
          else pure (spread_curr, p_curr)
  foldlM pickMu (0, pstart) ptail
  where
    n = floor (prop * fromIntegral ndata)
    n2 = floor (prop * fromIntegral n)
    ndata = length sset -- size of dataset at current level


liftMaybeT :: Applicative m => Maybe a -> MaybeT m a
liftMaybeT = MaybeT . pure


-- | Sample a random split of the dataset
--
-- Invariant : the concatenation of the two resulting vectors is a permutation of the input vector
--
-- NB : Will return Nothing if the required sample size is too large
randomSplit :: (PrimMonad m) =>
               Int -- ^ Sample size
            -> V.Vector a -- ^ Dataset
            -> P.Gen (PrimState m)
            -> MaybeT m (V.Vector a, V.Vector a) -- ^ (sample, complementary sample)
randomSplit n vv gen = MaybeT $ fmap split <$> sampl
  where
    sampl = sample n ixs gen
    split xs = (vxs, vxsc)
      where
        ixss = S.fromList xs
        ixsc = ixs `S.difference` ixss
        vxs  = pickItems ixss
        vxsc = pickItems ixsc
    m = V.length vv
    ixs = S.fromList [0 .. m - 1]
    pickItems = V.fromList . foldl (\acc i -> vv V.! i : acc) []


-- | Sample _without_ replacement. Returns empty list if required sample size is too large
sampleV :: (PrimMonad m, Foldable f) =>
           Int -- ^ Size of sample
        -> f a
        -> P.Gen (PrimState m) -> MaybeT m (V.Vector a)
sampleV n xs g = MaybeT $ fmap V.fromList <$> sample n xs g
{-# INLINE sampleV #-}

medianDist :: (Ord d) => (t -> p -> d) -> p -> V.Vector t -> Maybe (d, V.Vector d)
medianDist distf p ds =
  case mmu of
    Nothing -> Nothing
    Just mu -> Just (mu, dists)
  where
    mmu = median dists
    dists = V.map (`distf` p) ds
{-# INLINE medianDist #-}

median :: Ord a => V.Vector a -> Maybe a
median xs
  | n <= 1 = Nothing
  | otherwise = Just $sortV xs V.! floor (fromIntegral n / 2)
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
-- NB : uses 'P.create' internally
withST_ :: (forall s . P.Gen s -> ST s a) -- ^ Memory bracket for the PRNG
        -> a
withST_ st = runST $ do
  g <- P.create
  st g

-- | Runs a PRNG action in the 'ST' monad, using a given random seed
--
-- NB : uses 'P.initialize' internally
withST :: (VG.Vector v Word32) =>
          v Word32 -- ^ Random seed
       -> (forall s . P.Gen s -> ST s a) -- ^ Memory bracket for the PRNG
       -> a
withST seed st = runST $ do
  g <- P.initialize seed
  st g



-- | Draw a tree
--
-- NB : prints distance information rounded to two decimal digits
draw :: (Show a, PrintfArg d) => VPTree d a -> IO ()
draw = drawVT . vpTree

drawVT :: (Show a, PrintfArg d) => VT d a -> IO ()
drawVT = putStrLn . B.render . toBox

toBox :: (Show a, PrintfArg d) => VT d a -> B.Box
toBox = \case
  (Bin d x tl tr) ->
    txt (node x d) `stack` (toBox tl `byside` toBox tr)
  Tip x -> txt $ show x
  Nil   -> txt "*"
  where
    node x d = printf "%s,%5.2f" (show x) d
    -- nodeBox x d =
    --   txt (printf "%s,%5.2f" (show x) d)

txt :: String -> B.Box
txt t = spc `byside` B.text t `byside` spc
  where spc = B.emptyBox 1 1

byside :: B.Box -> B.Box -> B.Box
byside l r = B.hcat B.top [l, r]

stack :: B.Box -> B.Box -> B.Box
stack t b = B.vcat B.center1 [t, b]






-- test data

data P = P Double Double deriving (Eq)
instance Show P where
  show (P x y) = printf "(%2.2f, %2.2f)" x y --show (x,y)

distp :: P -> P -> Double
distp (P x1 y1) (P x2 y2) = sqrt $ (x1 - x2)**2 + (y1 - y2)**2



genN2 :: Int -> V.Vector P
genN2 n = V.fromList $ withST_ (P.samples n (binMix 0 20 1 1))

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



-- tt :: RealFrac p => Int -> p -> VT Double P
tt n = vptree (genN2 n)

-- vptree :: RealFrac p => V.Vector P -> p -> VT Double P
vptree ps p = withST_ $ buildVT distp p ps
