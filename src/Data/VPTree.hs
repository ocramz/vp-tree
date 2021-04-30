{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# language BangPatterns #-}
{-# language DeriveFunctor, DeriveFoldable, DeriveTraversable, GeneralizedNewtypeDeriving #-}
{-# language LambdaCase #-}
{-# language DeriveDataTypeable #-}
{-# language DeriveGeneric #-}

{-# options_ghc -Wno-type-defaults #-}
{-# options_ghc -Wno-unused-top-binds #-}
{-# options_ghc -Wno-unused-imports #-}
{-# options_ghc  -Wno-name-shadowing #-}
{- |

This library provides an implementation of Vantage Point Trees [1], a data structure useful for indexing data points that exist in some metric space.

= Usage

* 'range' : construct an index from a dataset and a distance function

* 'range' : find points in the index that lie within a given distance from the query


= References

1) P. N. Yianilos - Data structures and algorithms for nearest neighbor search in general metric spaces - http://web.cs.iastate.edu/~honavar/nndatastructures.pdf
-}
module Data.VPTree
  (VPTree
  -- * Construction
  , build
  -- * Query
  , range
  -- , nearest
  -- * Utilities
  -- ** Rendering trees
  , draw
  )
  where

import Control.Applicative (Alternative(..))
import Control.Monad.IO.Class (MonadIO(..))
-- import Data.Ord (Down(..))
import Data.Word (Word32)
-- import Control.Exception (Exception(..))
import Control.Monad.ST (ST, runST)
import Text.Printf (PrintfArg, printf)

-- -- deepseq
-- import Control.DeepSeq (NFData(..))
-- mtl
import Control.Monad.Writer (MonadWriter(..))
-- mwc-probability
import qualified System.Random.MWC.Probability as P (Gen, Prob, withSystemRandom, asGenIO, GenIO, create, initialize, sample, samples, normal, bernoulli, uniformR)
-- primitive
import Control.Monad.Primitive (PrimMonad(..), PrimState)
-- transformers
import Control.Monad.Trans.Maybe (MaybeT(..), runMaybeT)
import Control.Monad.Trans.Writer (WriterT(..), runWriterT, execWriterT)
-- vector
import qualified Data.Vector as V (Vector, map, filter, length, toList, replicate, partition, zipWith, head, tail, fromList, thaw, freeze, (!), foldl)
import qualified Data.Vector.Generic as VG (Vector(..))

-- import qualified Data.MaxPQ as MQ (MaxPQ, empty, insert, size, findMax, toList)

import Data.VPTree.Internal (VT, VPTree, withST, withST_, withIO)
import Data.VPTree.Build (build, buildVT)
import Data.VPTree.Query (range)
import Data.VPTree.Draw (draw)

