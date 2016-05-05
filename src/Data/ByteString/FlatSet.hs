{-# LANGUAGE BangPatterns #-}
module Data.ByteString.FlatSet
  ( FlatSet
  , fromList
  , member
  ) where

import           Prelude                      hiding (length)

import qualified Data.List                    as List

import           Data.ByteString              (ByteString)
import qualified Data.ByteString              as B
import qualified Data.ByteString.Lazy         as BL
import qualified Data.ByteString.Lazy.Builder as BL
import           Data.Vector.Unboxed          (Vector)
import qualified Data.Vector.Unboxed          as V


data FlatSet = FlatSet
  { fsIndices :: !(Vector (Int,Int))
  , fsData    :: !ByteString
  } deriving (Show)


sortNub :: Ord t => [t] -> [t]
sortNub = go . List.sort
  where
    go [] = []
    go (b:bs) = b:go (List.dropWhile (==b) bs)

fromList :: [ByteString] -> FlatSet
fromList bssUnord =
  let
    bss = sortNub bssUnord
    len = List.length bss
    go ([],_) = Nothing
    go (b:bs, idx) =
      let l = B.length b
      in Just ((idx, l), (bs, idx + l))
    indices = V.unfoldrN len go (bss,0)
  in FlatSet indices (B.concat bss)

length :: FlatSet -> Int
length (FlatSet indices _) = V.length indices

unsafeIndex :: Int -> FlatSet -> ByteString
unsafeIndex idx (FlatSet indices bss) =
  uncurry slice (V.unsafeIndex indices idx) bss

slice offset len = B.take len . B.drop offset

sliceP = uncurry slice

compareAt :: FlatSet -> Int -> ByteString -> Ordering
compareAt fs idx bs = compare bs (unsafeIndex idx fs)

member bs fs@(FlatSet indices bss) =
  case V.length indices of
    0 -> False
    1 -> compareAt fs 0 bs == EQ
    2 -> compareAt fs 0 bs == EQ || compareAt fs 1 bs == EQ
    _ -> go 0 (V.length indices)
  where
    go !low !high
      | low > high = False
      | otherwise = case compareAt fs mid bs of
                      LT -> go low (mid-1)
                      EQ -> True
                      GT -> go (mid+1) high

      where
        mid = low + ((high - low) `div` 2)

