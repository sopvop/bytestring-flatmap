{-# LANGUAGE BangPatterns #-}
module Data.ByteString.FlatSet
  ( FlatSet
  -- * Query
  , member
  , notMember
  , null
  , size
  , index
  , lookupGE
  , lookupLE
  , lookupGT
  , lookupLT
  , intersection
  , union
  , unionW
  , unionMut
  -- * Construction
  , fromList
  , unsafeFromAscList
  -- * Deconstruction
  , toList
  , toVector
  ) where

import           Prelude                  hiding (length, null)

import           Control.Monad            (void)
import           Control.Monad.ST         (ST)

import           Foreign.ForeignPtr       (withForeignPtr)
import           Foreign.Ptr              (plusPtr)

import qualified Data.List                as List

import           Data.ByteString          (ByteString)
import qualified Data.ByteString          as B
import qualified Data.ByteString.Internal as BI
import qualified Data.Vector              as V
import qualified Data.Vector.Mutable      as MV
import qualified Data.Vector.Unboxed      as UV

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
fromList = unsafeFromAscList . sortNub

-- | /O(n)/ Build 'FlatSet' from sorted list of 'ByteString's
unsafeFromAscList :: [ByteString] -> FlatSet
unsafeFromAscList bss = FlatSet indices (B.concat bss)
  where
    len = List.length bss
    go ([],_) = Nothing
    go (b:bs, idx) =
      let l = B.length b
      in Just ((idx, l), (bs, idx + l))
    indices = UV.unfoldrN len go (bss,0)

-- | /O(n)/ Build 'FlatSet' from sorted vector of 'ByteString's
unsafeFromAscVector :: V.Vector ByteString -> FlatSet
unsafeFromAscVector bss = FlatSet indices datas
  where
    !indices = UV.convert $ V.postscanl' goIdx (0,0) $ bss
    goIdx (!i,!l) bs = (i+l, B.length bs)
    !datas = concatByteStringV bss

concatByteStringV :: V.Vector ByteString -> ByteString
concatByteStringV bss
  | V.null bss = mempty
  | V.length bss == 1 = V.head bss
  | otherwise  = BI.unsafeCreate totalLen $ \ptr -> void (V.foldM' go ptr bss)
  where
    totalLen = V.foldr ( (+) . B.length) 0  bss
    go !ptr (BI.PS fp off len) = do
       withForeignPtr fp $ \p -> BI.memcpy ptr (p `plusPtr` off) len
       pure (ptr `plusPtr` len)

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
{-# INLINE unsafeIndex #-}


slice :: Int -> Int -> ByteString -> ByteString
slice offset len = B.take len . B.drop offset
{-# INLINE slice #-}

-- | /O(1)/. Find value at a given index.
valueAt :: Int -> FlatSet -> Maybe ByteString
valueAt idx (FlatSet indices bss) =
  flip (uncurry slice) bss <$> indices UV.!? idx

-- | /O(log n)/ Check if 'ByteString' is member of the set.
member :: ByteString -> FlatSet -> Bool
member bs fs = case lookupGeneric bs fs of
  Left{} -> False
  Right{} -> True

-- | /O(log n)/ Find index of element in set.
index :: ByteString -> FlatSet -> Maybe Int
index bs fs = either (const Nothing) Just (lookupGeneric bs fs)

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


compareAt fs idx (BI.PS fp1 off1 len1) =
    BI.accursedUnutterablePerformIO $
      withForeignPtr fp1 $ \p1 ->
      withForeignPtr fp2 $ \p2 -> do
        i <- BI.memcmp (p1 `plusPtr` off1) (p2 `plusPtr` (off2 + off)) (min len1 len2)
        return $! case i `compare` 0 of
                    EQ  -> len1 `compare` len2
                    x   -> x
  where
    (!off, !len2) = UV.unsafeIndex (fsIndices fs) idx
    (BI.PS fp2 off2 _) = fsData fs
{-# INLINE compareAt #-}

-- | /O(log n)/ Try to find index of the element in the 'FlatSet'.
-- it if element is not found, return indices of the larger and
-- smaller elements.
-- Returned indices may not be presented in set if element is on
-- the border.
lookupGeneric :: ByteString -> FlatSet -> Either (Int, Int) Int
{-# INLINE lookupGeneric #-}
lookupGeneric bs fs =
  case UV.length (fsIndices fs) of
    0 -> Left (-1,1) -- XXX: Looks terribly insane
    1 -> if compareAt fs 0 bs == EQ
         then Right 0
         else Left (-1,1)
    n -> lookupInRange bs fs 0 (n-1)

lookupInRange :: ByteString -> FlatSet -> Int -> Int -> Either (Int, Int) Int
lookupInRange bs fs from to = go from to
  where
    go !low !high
       | low > high = Left (high, low)
       | otherwise  = case compareAt fs mid bs of
                        LT -> go low (mid-1)
                        EQ -> Right mid
                        GT -> go (mid+1) high
       where
         mid = low + ((high - low) `div` 2)
{-# INLINE lookupInRange #-}

notMember :: ByteString -> FlatSet -> Bool
notMember = (not .). member

maxMinBySize :: FlatSet -> FlatSet -> (FlatSet, FlatSet)
maxMinBySize l r = if size l > size r
                   then (l,r)
                   else (r,l)

-- | / O(n + m)/ The intersection of two sets.
intersection :: FlatSet -> FlatSet -> FlatSet
intersection iLeft iRight = unsafeFromAscVector $ V.unfoldrN sz go (0,0)
  where
    -- Swap so we do less lookups in bigger set
    (left, right) = maxMinBySize iLeft iRight
    !sz = size right -- intersection can't be bigger than smallest set
    !leftSize = size left
    go (base, idx)
      | idx >= sz = Nothing
      | otherwise = let !v = unsafeIndex idx right
                    in case lookupInRange v left base (leftSize-1) of
                         Left (l, _) -> go (l, succ idx)
                         Right l -> Just (v, (l, succ idx))

-- | / O smthing / The union of two sets.
union :: FlatSet -> FlatSet -> FlatSet
union left right
  | null left = right
  | null right = left
  | otherwise = unsafeFromAscVector mergeVec
  where
    lvec = toVector left
    rvec = toVector right
    sz = V.length lvec + V.length rvec
    mergeVec = V.unfoldrN sz go (Left (lvec, rvec))
    go (Left (l, r))
      | V.null l = go (Right r)
      | V.null r = go (Right l)
      | otherwise =
        let
          lv = V.head l
          rv = V.head r
        in case compare lv rv of
             LT -> Just (lv, Left (V.tail l, r))
             EQ -> Just (lv, Left (V.tail l, V.tail r))
             GT -> Just (rv, Left (l, V.tail r))
    go (Right v) = if V.null v
                   then Nothing
                   else Just (V.head v, Right (V.tail v))

-- XXX Decide what to do with this. Needs better benches

data Which = L !Int
           | R !Int
           | LR !Int !Int

unionW :: FlatSet -> FlatSet -> FlatSet
unionW left right
  | null left = right
  | null right = left
  | otherwise = unsafeFromAscVector $ V.map toBS mergeVec
  where
    lsize = size left
    rsize = size right
    sz = size left + size right
    mergeVec = V.unfoldrN sz go (LR 0 0)
    toBS (Left s) = uncurry slice s $ fsData left
    toBS (Right s) = uncurry slice s $ fsData right
    go (L idx) = if idx >= lsize
                 then Nothing
                 else Just (Left (fsIndices left UV.! idx), L (succ idx))
    go (R idx) = if idx >= lsize
                 then Nothing
                 else Just (Right (fsIndices right UV.! idx), R (succ idx))
    go (LR l r)
      | l >= lsize = go (R r)
      | r >= rsize = go (L l)
      | otherwise =
        let
          ls = fsIndices left  UV.! l
          rs = fsIndices right UV.! r
          lv = uncurry slice ls $ fsData left
          rv = uncurry slice rs $ fsData right
        in case compare lv rv of
             LT -> Just (Left ls, LR (succ l) r)
             EQ -> Just (Left ls, LR (succ l) (succ r))
             GT -> Just (Right rs, LR l (succ r))


-- for benchmarks
unionMut :: FlatSet -> FlatSet -> FlatSet
unionMut iLeft iRight
  | null iLeft = iRight
  | null iRight = iLeft
  | otherwise = unsafeFromAscVector $ V.create doUnion
  where
    left = toVector iLeft
    right = toVector iRight
    sz = V.length left + V.length right
    doUnion :: ST s (MV.MVector s ByteString)
    doUnion = do
      res <- MV.new sz
      let
        writeAll v idx
          | V.null v = pure idx
          | otherwise = MV.write res idx (V.head v)
                        *> writeAll (V.tail v) (succ idx)
        loop l r idx
          | V.null l = writeAll r idx
          | V.null r = writeAll l idx
          | otherwise =
            let
              hleft = V.head l
              hright = V.head r
            in case compare hleft hright of
                 LT -> MV.write res idx hleft *> loop (V.tail l) r (succ idx)
                 EQ -> MV.write res idx hleft *> loop (V.tail l) (V.tail r) (succ idx)
                 GT -> MV.write res idx hright *> loop l (V.tail r) (succ idx)
      finalSize <- loop left right 0
      pure $ MV.take finalSize res
