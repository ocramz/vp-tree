{-# LANGUAGE LambdaCase #-}
{-# options_ghc -Wno-unused-imports #-}
module Data.VPTree.Draw (
  draw, drawVT
  -- * helpers
  , toStringVT
  ) where

import Text.Printf (PrintfArg, printf)
import Data.VPTree.Internal (VPTree(..), VT(..))

-- boxes
import qualified Text.PrettyPrint.Boxes as B (Box, render, emptyBox, vcat, hcat, text, top, bottom, center1)

-- | Draw a tree
--
-- NB : prints distance information rounded to two decimal digits
draw :: (Show a, PrintfArg d) => VPTree d a -> IO ()
draw = drawVT . vpTree

drawVT :: (Show a, PrintfArg d) => VT d a -> IO ()
drawVT = putStrLn . toStringVT

toStringVT :: (Show a, PrintfArg d) => VT d a -> String
toStringVT = B.render . toBox

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
