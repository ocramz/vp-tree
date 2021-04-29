{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# options_ghc -Wno-unused-imports #-}
module Data.VPTree.Query (
  range
  -- * Utilities
  , distances
  ) where

import Control.Monad.IO.Class (MonadIO(..))
import Data.Foldable (toList, foldrM, foldlM)


-- containers
import Data.Sequence as SQ (Seq)
import Data.Sequence ((|>))
-- mtl
import Control.Monad.State (MonadState(..))
-- psqueues
import qualified Data.IntPSQ as PQ (IntPSQ, insert, size, empty, toList, minView)
-- transformers
import Control.Monad.Trans.State (State, evalState, runState)
-- vector
import qualified Data.Vector as V (Vector)


import Data.VPTree.Internal (VT(..), VPTree(..))

psqList :: (Ord p) =>
           PQ.IntPSQ p b -> [(p, b)]
psqList q = case PQ.minView q of
  Nothing -> mempty
  Just (_, p, v, qrest) -> (p, v) : psqList qrest

-- | All distances to a query point
distances :: VPTree b a
             -> a -- ^ query
             -> [b]
distances (VPT tt distf) x = map (distf x) $ toList tt

-- | Range query : find all points in the tree closer to the query point than a given threshold
range :: (Num p, Ord p) =>
         VPTree p a
      -> p -- ^ proximity threshold
      -> a -- ^ query point
      -> [(p, a)]
range (VPT tt distf) eps x = psqList $ rangeVT eps x distf tt
-- range (VPT tt distf) eps x = rangeVT' eps x distf tt


rangeVT :: (Num b, Ord b) =>
           b -- ^ proximity threshold
        -> a -> (a -> a -> b) -> VT b a -> PQ.IntPSQ b a
rangeVT eps x distf = flip evalState 0 . go PQ.empty
  where
    go acc = \case
      Tip ts ->
        foldlM insf acc ts
        where
          insf acc t
            | d < eps = do
                i <- get
                let acc' = PQ.insert i d t acc
                put (i + 1)
                pure acc'
            | otherwise = pure acc
            where
              d = distf x t

      Bin mu v ll rr
        | d < eps -> do
            i <- get
            let acc' = PQ.insert i d v acc
            put (i + 1)
            go acc' ll
        | d > mu + eps -> go acc rr
        | d <= mu + eps && d > mu - eps -> do
            accl <- go acc ll
            accr <- go acc rr
            union accl accr
        | otherwise -> go acc ll

        -- | otherwise -> do
        --     accl <- go acc ll
        --     accr <- go acc rr
        --     union accl accr
        where
          d = distf x v




-- rekey starting from the current index
union :: (MonadState Int m, Ord b) =>
         PQ.IntPSQ b c -> PQ.IntPSQ b c -> m (PQ.IntPSQ b c)
union q1 q2 = do
  i0 <- get
  pure $ flip evalState i0 $ foldrM f PQ.empty $ l1 <> l2
  where
    f (_, p, v) acc = do
      i <- get
      let acc' = PQ.insert i p v acc
      put $ succ i
      pure acc'
    l1 = PQ.toList q1
    l2 = PQ.toList q2



-- rangeVT' :: (Ord a, Num a) =>
--             a -> p -> (p -> b -> a) -> VT a b -> [(a, b)]
-- rangeVT' eps x distf = go mempty
--   where
--     insert v qry acc = if d < eps
--       then (d, v) : acc
--       else acc
--       where d = distf qry v
--     go acc = \case
--       Nil -> acc
--       Tip t -> insert t x acc
--       Bin mu v ll rr
--         | d < eps -> go ((d, v) : acc) ll
--         | eps < d - mu -> go acc rr
--         | otherwise -> go acc ll <> go acc rr
--         where
--           d = distf x v




-- nearest :: (Num d, Ord d) =>
--            VPTree d a
--         -> Int
--         -> a
--         -> PQ.IntPSQ d a
-- nearest (VPT t df) k x = nearestVT df k t x


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

-- logVar :: (MonadIO io, Show a) => String -> a -> io ()
-- logVar w x = liftIO $ putStrLn $ unwords [w, "=", show x]

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
--     go acc i maxd Nil = (acc, i, maxd)
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
