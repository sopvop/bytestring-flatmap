{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Criterion
import           Criterion.Main          (defaultMain)

import qualified Data.ByteString.Char8   as BS
import qualified Data.ByteString.FlatSet as FlatSet
import qualified Data.Trie               as Trie

import           System.Mem

evenOdd (l,r) [] = (l, r)
evenOdd (l,r) [x] = (x:l, r)
evenOdd (l,r) (x:y:rs) = evenOdd (x:l, y:r) rs

setupEnv = do
    bs <- BS.words <$> BS.readFile "/usr/share/dict/cracklib-words"
    let
      (ls, rs) = evenOdd ([],[]) bs
      !left = FlatSet.fromList ls
      !right = FlatSet.fromList rs
      !trie = Trie.fromList $ zip ls (repeat ())
    pure (left,right,trie)

main = do
    lr@(!l,!r,!trie) <- setupEnv
    performGC
    defaultMain
       [  {-bgroup "union"
          [ bench "union vectors" $ whnf (uncurry FlatSet.union) lr
          , bench "union MVector" $ whnf (uncurry FlatSet.unionMut) lr
          , bench "union With"    $ whnf (uncurry FlatSet.unionW) lr
          ]
       ,-} bgroup "lookup"
          [ bench "Trie"    $ nf (map (\x -> Trie.member x trie)) (FlatSet.toList l) -- ++ FlatSet.toList r)
          , bench "FlatSet" $ nf (map (\x -> FlatSet.member x l)) (FlatSet.toList l) -- ++ FlatSet.toList r)
          ]
       ]
