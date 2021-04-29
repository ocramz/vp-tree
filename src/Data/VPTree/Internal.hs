{-# language DeriveGeneric #-}
{-# language DeriveFoldable, DeriveTraversable, DeriveFunctor #-}
module Data.VPTree.Internal where
import GHC.Generics (Generic(..))

-- deepseq
import Control.DeepSeq (NFData (rnf))
-- serialise
import Codec.Serialise (Serialise(..))

-- | Vantage point tree
data VPTree d a = VPT {
  vpTree :: VT d a
  , vptDistFun :: a -> a -> d -- ^ Distance function used to construct the tree
                   } deriving (Generic)

instance (Eq d, Eq a) => Eq (VPTree d a) where
  (VPT t1 _) == (VPT t2 _) = t1 == t2
instance (Show d, Show a) => Show (VPTree d a) where
  show (VPT t _) = show t

-- | Vantage point tree (internal representation)
data VT d a = Bin  !d !a !(VT d a) !(VT d a)
            | Tip a
            | Nil
            deriving (Eq, Show, Generic, Functor, Foldable, Traversable)
instance (Serialise d, Serialise a) => Serialise (VT d a)

instance (NFData d, NFData a) => NFData (VT d a) where
  rnf (Bin d x tl tr) = rnf d `seq` rnf x `seq` rnf tl `seq` rnf tr
  rnf (Tip x) = rnf x
  rnf Nil = ()
