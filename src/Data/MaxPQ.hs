module Data.MaxPQ (MaxPQ, empty, size, insert, fromList, findMax, deleteMax) where

import Data.Ord (Down(..))

-- psqueues
import qualified Data.IntPSQ as PQ (IntPSQ, empty, size, insert, findMin, deleteMin)


data MaxPQ p a = MaxPQ {
  ixNext :: !Int
  , unMaxPQ :: PQ.IntPSQ (Down p) a
  } deriving (Eq, Show)

empty :: MaxPQ p a
empty = MaxPQ 0 PQ.empty

size :: MaxPQ p a -> Int
size (MaxPQ _ pq) = PQ.size pq

insert :: Ord p => p -> a -> MaxPQ p a -> MaxPQ p a
insert p v (MaxPQ ix pq) = MaxPQ ix' (PQ.insert ix' (Down p) v pq)
  where ix' = succ ix

fromList :: (Ord p) => [(p , a)] -> MaxPQ p a
fromList = foldl (flip $ uncurry insert) empty

findMax :: Ord p => MaxPQ p a -> Maybe (p, a)
findMax (MaxPQ _ pq) = (\(_, Down p, x) -> (p, x)) <$> PQ.findMin pq

deleteMax :: Ord p => MaxPQ p a -> MaxPQ p a
deleteMax (MaxPQ ix pq) = MaxPQ ix $ PQ.deleteMin pq
