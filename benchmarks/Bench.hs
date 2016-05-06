{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Criterion
import           Criterion.Main          (defaultMain)

import qualified Data.ByteString.Char8   as BS
import qualified Data.ByteString.FlatSet as FlatSet

import           System.Mem

evenOdd (l,r) [] = (l, r)
evenOdd (l,r) [x] = (x:l, r)
evenOdd (l,r) (x:y:rs) = evenOdd (x:l, y:r) rs

setupEnv = do
    bs <- BS.words <$> BS.readFile "/usr/share/dict/words"
    let
      (ls, rs) = evenOdd ([],[]) bs
      !left = FlatSet.fromList ls
      !right = FlatSet.fromList rs
    pure (left,right)

main = do
    lr@(!l,!r) <- setupEnv
    performGC
    defaultMain
       [  bgroup "union"
          [ bench "union vectors" $ whnf (uncurry FlatSet.union) lr
          , bench "union MVector" $ whnf (uncurry FlatSet.unionMut) lr
          , bench "union With"    $ whnf (uncurry FlatSet.unionW) lr
          ]
       ]
