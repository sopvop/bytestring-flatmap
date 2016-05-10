{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Data.ByteString.FlatMap.Internal
  where

import           Control.Applicative

import           Data.ByteString         (ByteString)
import qualified Data.List               as List
import           Data.Ord                (comparing)

import qualified Data.Vector             as V
import           Data.Vector.Generic     (Vector)
import qualified Data.Vector.Generic     as VG

import           Data.ByteString.FlatSet (FlatSet)
import qualified Data.ByteString.FlatSet as FlatSet

import           Prelude                 hiding (map, mapM, mapM_)

data FlatMap v = FlatMap
  { fmValues :: !v
  , fmKeys   :: !FlatSet
  }

-- | /O(1)/ number of elements in map
size :: FlatMap v -> Int
size (FlatMap _ k) = FlatSet.size k

-- | /O(log n)/ Lookup value in map
lookup :: Vector v a
       => ByteString
       -> FlatMap (v a)
       -> Maybe a
lookup k m = case FlatSet.index k (fmKeys m) of
               Just idx -> Just $! VG.unsafeIndex (fmValues m) idx
               Nothing -> Nothing

-- | /O(log n)/ map value at key, if present
adjust :: Vector v a
       => (a -> a)
       -> ByteString
       -> FlatMap (v a)
       -> FlatMap (v a)
adjust f k m@(FlatMap v keys) =
  case FlatSet.index k keys of
    Nothing -> m
    Just idx -> FlatMap (v VG.// [(idx, f ( v VG.! idx))]) keys -- looks stupid

-- | /O(n)/ map a function over values
map :: (Vector v a, Vector v b)
    => (a -> b)
    -> FlatMap (v a)
    -> FlatMap (v b)
map f m = FlatMap (VG.map f (fmValues m)) (fmKeys m)

-- | /O(n)/ apply function to every element and it's key
imap :: (Vector v a, Vector v b)
     => (ByteString -> a -> b)
     -> FlatMap (v a)
     -> FlatMap (v b)
imap f (FlatMap v keys) =
  FlatMap (VG.imap (\idx a -> f (FlatSet.unsafeValueAt idx keys) a) v) keys

-- | /O(n)/ monadic map over values in map
mapM :: (Monad m, Vector v a, Vector v b)
     => (a -> m b)
     -> FlatMap (v a)
     -> m (FlatMap (v b))
mapM f (FlatMap v keys) =
  flip FlatMap keys <$> VG.mapM f v

-- | /O(n)/ Apply monadic action to every value in map and its key.
imapM :: (Monad m, Vector v a, Vector v b)
      => (ByteString -> a -> m b)
      -> FlatMap (v a)
      -> m (FlatMap (v b))
imapM f (FlatMap v keys) =
  flip FlatMap keys <$> VG.imapM (\idx a -> f (FlatSet.unsafeValueAt idx keys) a) v

-- | /O(n)/ Apply monadic action to every value in map, returning resulting map.
mapM_ :: (Monad m, Vector v a)
      => (a -> m b)
      -> FlatMap (v a)
      -> m ()
mapM_ f (FlatMap v _) = VG.mapM_ f v

-- | /O(n)/ Apply monadic action to every value in map and its key.
imapM_ :: (Monad m, Vector v a)
      => (ByteString -> a -> m b)
      -> FlatMap (v a)
      -> m ()
imapM_ f (FlatMap v keys) = VG.imapM_ (\idx a -> f (FlatSet.unsafeValueAt idx keys) a) v

-- | /O(n)/ Left fold with strict accumulator
foldl' :: Vector v a => (b -> a -> b) -> b -> FlatMap (v a) -> b
foldl' f acc0 (FlatMap v _) = VG.foldl' f acc0 v

-- | /O(n)/ Left fold on non-empty map with strict accumulator
foldl1' :: Vector v a => (a -> a -> a) -> FlatMap (v a) -> a
foldl1' f (FlatMap v _) = VG.foldl1' f v

-- | /O(n)/ Right fold
foldr :: Vector v a => (a -> b -> b) -> b -> FlatMap (v a) -> b
foldr f acc0 (FlatMap v _) = VG.foldr f acc0 v

-- | /O(n)/ Right fold on non-empty map
foldr1 :: Vector v a => (a -> a -> a) -> FlatMap (v a) -> a
foldr1 f (FlatMap v _) = VG.foldr1 f v

-- | /O(n)/ Left fold over key/value pairs with strict accumulator
ifoldl' :: Vector v a
        => (b -> ByteString -> a -> b)
        -> b
        -> FlatMap (v a)
        -> b
ifoldl' f acc0 (FlatMap v keys) = VG.ifoldl' go acc0 v
  where
    go !acc idx a = f acc (FlatSet.unsafeValueAt idx keys) a

-- | /O(n)/ Right fold over key/value pairs with strict accumulator
ifoldr' :: Vector v a
        => (ByteString -> a -> b -> b)
        -> b
        -> FlatMap (v a)
        -> b
ifoldr' f acc0 (FlatMap v keys) = VG.ifoldr' go acc0 v
  where
    go idx a acc = f (FlatSet.unsafeValueAt idx keys) a acc

-- | /O(n)/ convert value vector backing the map
convert :: (Vector v a, Vector w a)
        => FlatMap (v a)
        -> FlatMap (w a)
convert (FlatMap v keys) = FlatMap (VG.convert v) keys


sortNubKeys :: [(ByteString, a)] -> [(ByteString, a)]
sortNubKeys = go . List.sortBy (comparing fst)
  where
    go [] = []
    go (b:bs) = b:go (List.dropWhile ((==) (fst b) . fst) bs)
{-# INLINE sortNubKeys #-}

-- | /O(n*log n)/ Create 'FlatMap' from list of keys and values.
-- If keys are specified several times, later values are ignored
fromList :: Vector w a => [(ByteString, a)] -> FlatMap (w a)
fromList vl = FlatMap (V.convert vals) (FlatSet.unsafeFromAscVector keys)
  where
    (keys, vals) = V.unzip $ V.fromListN len vls
    vls = sortNubKeys vl
    len = List.length vls

-- | /O(n)/ Convert to vector of key-value pairs, ascending by key
-- Note that keys reuse storage in 'FlatMap'. Use ByteString.copy if you
-- Need to store keys longer than map, otherwise storage will not be freed
toVector :: Vector v b => FlatMap (v b) -> V.Vector (ByteString, b)
toVector (FlatMap vals keys) = V.zip (FlatSet.toVector keys) (V.convert vals)

-- | /O(n)/ Convert to list of key-value pairs, ascending by key
toList :: Vector v b => FlatMap (v b) -> [(ByteString, b)]
toList v = V.toList (toVector v)


toValuesVector :: FlatMap v -> v
toValuesVector (FlatMap v _) = v

toValues :: Vector v a => FlatMap (v a) -> [a]
toValues = VG.toList . toValuesVector

toKeysVector :: FlatMap v -> V.Vector ByteString
toKeysVector (FlatMap _ keys) = FlatSet.toVector keys

toKeys :: FlatMap v -> [ByteString]
toKeys (FlatMap _ keys) = FlatSet.toList keys
