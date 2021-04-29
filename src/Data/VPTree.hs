{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# language BangPatterns #-}
{-# language DeriveFunctor, DeriveFoldable, DeriveTraversable, GeneralizedNewtypeDeriving #-}
{-# language LambdaCase #-}
{-# language DeriveDataTypeable #-}
{-# language DeriveGeneric #-}

{-# options_ghc -Wno-type-defaults #-}
{-# options_ghc -Wno-unused-top-binds #-}
-- {-# options_ghc -Wno-unused-imports #-}
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

import Control.Applicative (Alternative(..))
import Control.Monad.IO.Class (MonadIO(..))
-- import Data.Ord (Down(..))
import Data.Word (Word32)
-- import Control.Exception (Exception(..))
import Control.Monad.ST (ST, runST)
import Text.Printf (PrintfArg, printf)

-- mtl
import Control.Monad.Writer (MonadWriter(..))
-- mwc-probability
import qualified System.Random.MWC.Probability as P (Gen, Prob, withSystemRandom, asGenIO, GenIO, create, initialize, sample, samples, normal, bernoulli)
-- primitive
import Control.Monad.Primitive (PrimMonad(..), PrimState)
-- transformers
import Control.Monad.Trans.Maybe (MaybeT(..), runMaybeT)
import Control.Monad.Trans.Writer (WriterT(..), runWriterT, execWriterT)
-- vector
import qualified Data.Vector as V (Vector, map, filter, length, toList, replicate, partition, zipWith, head, tail, fromList, thaw, freeze, (!), foldl)
import qualified Data.Vector.Generic as VG (Vector(..))

-- import qualified Data.MaxPQ as MQ (MaxPQ, empty, insert, size, findMax, toList)

import Data.VPTree.Internal (VT, VPTree)
import Data.VPTree.Build (buildVT)
import Data.VPTree.Query ()
import Data.VPTree.Draw (draw)



newtype App w m a = App {
  unApp :: MaybeT (WriterT w m) a
                        } deriving (Functor, Applicative, Monad, Alternative, MonadIO, MonadWriter w)

runApp :: App w m a -> m (Maybe a, w)
runApp a = runWriterT $ runMaybeT (unApp a)

runAppST :: (forall s . P.Gen s -> App w (ST s) a) -> (Maybe a, w)
runAppST a = withST_ (runApp . a)

-- -- testApp :: PrimMonad m => P.Gen (PrimState m) -> App m [Double] ()
-- testApp g = App $ do
--   z <- P.samples 5 (P.normal 0 1) g
--   tell z
--   pure z

-- sampleApp :: (Foldable f, PrimMonad m) =>
--              Int -> f a -> P.Gen (PrimState m) -> App m [String] [a]
-- sampleApp n ixs g = App $ do
--   zm <- sample n ixs g
--   case zm of
--     Nothing -> do
--       tell ["derp"]
--       empty
--     Just xs -> pure xs


-- runAppST :: (forall s . P.Gen s -> WriterT w (ST s) a) -> (a, w)
-- runAppST a = withST_ (runWriterT . a)




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










-- test data

data P = P Double Double deriving (Eq)
instance Show P where
  show (P x y) = printf "(%2.2f, %2.2f)" x y --show (x,y)

distp :: P -> P -> Double
distp (P x1 y1) (P x2 y2) = sqrt $ (x1 - x2)**2 + (y1 - y2)**2



genN1, genN2 :: Int -> V.Vector P
genN2 n = V.fromList $ withST_ (P.samples n (binMix 0 20 1 1))

genN1 n = V.fromList $ withST_ (P.samples n (isoNormal2d 0 1))

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



-- -- tt :: RealFrac p => Int -> p -> VT Double P
tt n = vptree (genN2 n)

-- -- vptree :: RealFrac p => V.Vector P -> p -> VT Double P
vptree ps p = withST_ $ buildVT distp p ps
