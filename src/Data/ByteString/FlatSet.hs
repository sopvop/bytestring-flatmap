{-# LANGUAGE BangPatterns #-}
module Data.ByteString.FlatSet
  ( FlatSet
  -- * Query
  , member
  , notMember
  , null
  , size
  , lookupGE
  , lookupLE
  , lookupGT
  , lookupLT
  -- * Construction
  , fromList
  -- * Deconstruction
  , toList
  , toVector
  ) where

import           Prelude             hiding (length, null)

import qualified Data.List           as List

import           Data.ByteString     (ByteString)
import qualified Data.ByteString     as B
import qualified Data.Vector         as V
import qualified Data.Vector.Unboxed as UV

data FlatSet = FlatSet
  { fsIndices :: !(UV.Vector (Int,Int))
  , fsData    :: !ByteString
  } deriving (Show, Eq)


sortNub :: Ord t => [t] -> [t]
sortNub = go . List.sort
  where
    go [] = []
    go (b:bs) = b:go (List.dropWhile (==b) bs)


-- | /O(n*log n)/ Build 'FlatSet' from the list of 'ByteString's.
fromList :: [ByteString] -> FlatSet
fromList bssUnord =
  let
    bss = sortNub bssUnord
    len = List.length bss
    go ([],_) = Nothing
    go (b:bs, idx) =
      let l = B.length b
      in Just ((idx, l), (bs, idx + l))
    indices = UV.unfoldrN len go (bss,0)
  in FlatSet indices (B.concat bss)

-- | /O(n)/. Convert set to the vector of 'ByteString's.
-- 'ByteString's in vector will be sorted.
--
-- Those bytestrings reuses 'FlatSet' storage, so bytestring
-- will not be removed unless all the ByteStrings in vector
-- can't be freed. If you need to detach 'ByteString' from
-- the set storage you need to use 'Data.ByteString.copy'.
toVector :: FlatSet -> V.Vector ByteString
toVector set = V.map go $ V.convert (fsIndices set) where
  go (off,len) = slice off len (fsData set)

-- | /O(n)/. Create a list of the set contents. All comments
-- in 'toVector' applies to this method.
toList :: FlatSet -> [ByteString]
toList = V.toList . toVector

-- | /O(1)/ Calculate the size of the set.
size :: FlatSet -> Int
size (FlatSet indices _) = UV.length indices

-- | /O(1)/ Check whether set is empty.
null :: FlatSet -> Bool
null = (==0) . size




unsafeIndex :: Int -> FlatSet -> ByteString
unsafeIndex idx (FlatSet indices bss) =
  uncurry slice (UV.unsafeIndex indices idx) bss

slice :: Int -> Int -> ByteString -> ByteString
slice offset len = B.take len . B.drop offset

-- | /O(1)/. Find value at a given index.
valueAt :: Int -> FlatSet -> Maybe ByteString
valueAt idx (FlatSet indices bss) =
  flip (uncurry slice) bss <$> indices UV.!? idx

compareAt :: FlatSet -> Int -> ByteString -> Ordering
compareAt fs idx bs = compare bs (unsafeIndex idx fs)

-- | /O(log n)/ Check if 'ByteString' is member of the set.
member :: ByteString -> FlatSet -> Bool
member bs fs = case lookupGeneric bs fs of
  Left{} -> False
  Right{} -> True

-- | /O(log n)/ Find first smallest element larger than this one.
lookupLT :: ByteString -> FlatSet -> Maybe ByteString
lookupLT bs fs = valueAt (either fst pred (lookupGeneric bs fs)) fs

-- | /O(log n)/ Find first largest element smaller than this one.
lookupGT :: ByteString -> FlatSet -> Maybe ByteString
lookupGT bs fs = valueAt (either snd succ (lookupGeneric bs fs)) fs

-- | /O(log n)/ Find first smallest element larger or equal than this one.
lookupLE :: ByteString -> FlatSet -> Maybe ByteString
lookupLE bs fs = valueAt (either fst id (lookupGeneric bs fs)) fs

-- | /O(log n)/ Find first largest element smaller or equal than this one.
lookupGE :: ByteString -> FlatSet -> Maybe ByteString
lookupGE bs fs = valueAt (either snd id (lookupGeneric bs fs)) fs

-- | /O(log n)/ Try to find index of the element in the 'FlatSet'.
-- it if element is not found, return indices of the larger and
-- smaller elements.
-- Returned indices may not be presented in set if element is on
-- the border.
lookupGeneric :: ByteString -> FlatSet -> Either (Int, Int) Int
lookupGeneric bs fs =
  case UV.length (fsIndices fs) of
    0 -> Left (-1,1) -- XXX: Looks terribly insane
    1 -> if compareAt fs 0 bs == EQ
         then Right 0
         else Left (-1,1)
    n -> go 0 n
  where
    go !low !high
      | low > high = Left (high, low)
      | otherwise  = case compareAt fs mid bs of
                       LT -> go low (mid-1)
                       EQ -> Right mid
                       GT -> go (mid+1) high
      where
        mid = low + ((high - low) `div` 2)

notMember :: ByteString -> FlatSet -> Bool
notMember = (not .). member
