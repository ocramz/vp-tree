{-|
Max-heap priority queues
-}
module Data.MaxPQ (
  MaxPQ, empty, size, insert, fromList, topK, findMax, deleteMax, toList
  ) where


import Data.Maybe (fromMaybe)
import Data.Ord (Down(..))

-- containers
import qualified Data.Sequence as S (Seq, empty, (|>))
-- psqueues
import qualified Data.IntPSQ as PQ (IntPSQ, empty, size, insert, findMin, deleteMin, toList)

-- | Max-heap priority queue
data MaxPQ p a = MaxPQ {
  ixNext :: {-# UNPACK#-} !Int
  , unMaxPQ :: PQ.IntPSQ (Down p) a
  } deriving (Eq, Show)

-- | The empty queue
empty :: MaxPQ p a
empty = MaxPQ 0 PQ.empty

-- | Number of elements in the queue
size :: MaxPQ p a -> Int
size (MaxPQ _ pq) = PQ.size pq

-- | Insert an element in the queue
insert :: Ord p =>
          p -- ^ Priority
       -> a -- ^ Element
       -> MaxPQ p a
       -> MaxPQ p a
insert p v (MaxPQ ix pq) = MaxPQ ix' (PQ.insert ix' (Down p) v pq)
  where ix' = succ ix

-- | Populate a 'MaxPQ' from a list
fromList :: (Ord p) => [(p , a)] -> MaxPQ p a
fromList = foldl (flip $ uncurry insert) empty

toList :: MaxPQ p a -> [(p, a)]
toList (MaxPQ _ pq) = noIxs `map` PQ.toList pq

-- | Find the highest priority item in the queue, if the queue contains at least one item
findMax :: Ord p => MaxPQ p a -> Maybe (p, a)
findMax (MaxPQ _ pq) = noIxs <$> PQ.findMin pq

-- | Return a queue without its largest item
deleteMax :: Ord p => MaxPQ p a -> MaxPQ p a
deleteMax (MaxPQ ix pq) = MaxPQ ix $ PQ.deleteMin pq


-- | Return K items with the highest priority, in decreasing order
topK :: Ord p =>
        Int -- ^ Size of result set
     -> MaxPQ p a
     -> S.Seq (p, a)
topK = popK popMax

popK :: (q -> Maybe (a, q))
     -> Int
     -> q
     -> S.Seq a
popK pop kk qq = fromMaybe S.empty $ go qq kk S.empty where
  go _ 0 acc = pure acc
  go q k acc = do
    (x, q') <- pop q
    go q' (k - 1) (acc S.|> x)

popMax :: Ord p => MaxPQ p a -> Maybe ((p, a), MaxPQ p a)
popMax q = do
  x <- findMax q
  let q' = deleteMax q
  pure (x, q')


-- internal
noIxs :: (z, Down a, b) -> (a, b)
noIxs (_, Down p, x) = (p, x)
