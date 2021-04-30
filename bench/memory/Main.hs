{-# options_ghc -Wno-unused-imports #-}
module Main where

-- vector
import qualified Data.Vector as V (Vector, map)
-- weigh
import Weigh (mainWith, wgroup, func)

import Data.VPTree.Build (build)
import Data.VPTree.Draw (draw)
import Data.VPTree.Internal (VT, VPTree, withST, withST_, withIO)
import Data.VPTree.Query (range, distances)
import Data.VPTree.TestData (buildP, binDiskSamples, gaussMixSamples, P(..))


main :: IO ()
main = mainWith $ do
  wgroup "Data.VPTree.Build" $ do
    let
      go n = buildP (binDiskSamples n)
    func "build 10" go 10
    func "build 100" go 100
    func "build 1000" go 1000
    func "build 10.000" go 10000
    -- func "build 100.000" go 100000
  wgroup "Data.VPTree.Query : index size 100" $ do
    let
      index = buildIndex binDiskSamples 100
      go n = queryIndex index binDiskSamples 1.0 n
    func "range 10" go 10
    func "range 100" go 100
    func "range 1000" go 1000
    func "range 10.000" go 10000
  wgroup "Data.VPTree.Query : index size 1000" $ do
    let
      index = buildIndex binDiskSamples 1000
      go n = queryIndex index binDiskSamples 1.0 n
    func "range 10" go 10
    func "range 100" go 100
    func "range 1000" go 1000
    func "range 10.000" go 10000
  wgroup "Data.VPTree.Query : index size 10.000" $ do
    let
      index = buildIndex binDiskSamples 10000
      go n = queryIndex index binDiskSamples 1.0 n
    func "range 10" go 10
    func "range 100" go 100
    func "range 1000" go 1000
    func "range 10.000" go 10000


buildIndex :: (t -> V.Vector P) -> t -> VPTree Double P
buildIndex genTree m = buildP (genTree m)

queryIndex :: (Num p1, Ord p1) =>
              VPTree p1 a -> (p2 -> V.Vector a) -> p1 -> p2 -> V.Vector [(p1, a)]
queryIndex index genData thr n = V.map (range index thr) qrys
  where
    qrys = genData n

-- rangeWith :: (p1 -> V.Vector P)
--           -> (p2 -> V.Vector P)
--           -> Double
--           -> p1
--           -> p2
--           -> V.Vector [(Double, P)]
-- rangeWith genTree genData thr m n = V.map (range tree thr) qrys
--   where
--     tree = buildP (genTree m)
--     qrys = genData n



-- main =
--   mainWith (do func "integers count 0" count 0
--                func "integers count 1" count 1
--                func "integers count 2" count 2
--                func "integers count 3" count 3
--                func "integers count 10" count 10
--                func "integers count 100" count 100)
--   where count :: Integer -> ()
--         count 0 = ()
--         count a = count (a - 1)
