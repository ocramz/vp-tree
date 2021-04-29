{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# language DeriveGeneric #-}
{-# language DeriveFoldable, DeriveTraversable, DeriveFunctor #-}
module Data.VPTree.Internal where

import Control.Monad.ST (ST, runST)
import Data.Word (Word32)
import GHC.Generics (Generic(..))

-- deepseq
import Control.DeepSeq (NFData(..))
-- mwc-probability
import qualified System.Random.MWC.Probability as P (Gen, Prob, withSystemRandom, asGenIO, GenIO, create, initialize)
-- serialise
import Codec.Serialise (Serialise(..))
-- vector
import qualified Data.Vector.Generic as VG (Vector(..))

-- | Vantage point tree
data VPTree d a = VPT {
  vpTree :: VT d a
  , vptDistFun :: a -> a -> d -- ^ Distance function used to construct the tree
                   } deriving (Generic)

instance (Eq d, Eq a) => Eq (VPTree d a) where
  (VPT t1 _) == (VPT t2 _) = t1 == t2
instance (Show d, Show a) => Show (VPTree d a) where
  show (VPT t _) = show t
instance (NFData d, NFData a) => NFData (VPTree d a) where

-- | Vantage point tree (internal representation)
data VT d a = Bin  {
  _mu :: !d -- ^ median distance to vantage point
  , _vp :: !a -- ^ vantage point
  , _near :: !(VT d a) -- ^ points at a distance < mu
  , _far :: !(VT d a) -- ^ points farther than mu
  }
            | Tip a
            | Nil
            deriving (Eq, Show, Generic, Functor, Foldable, Traversable)
instance (Serialise d, Serialise a) => Serialise (VT d a)

instance (NFData d, NFData a) => NFData (VT d a) where
  rnf (Bin d x tl tr) = rnf d `seq` rnf x `seq` rnf tl `seq` rnf tr
  rnf (Tip x) = rnf x
  rnf Nil = ()



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
