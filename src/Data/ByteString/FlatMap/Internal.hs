{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Data.ByteString.FlatMap.Internal
  where

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
  { fmValues :: v
  , fmKeys   :: FlatSet
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
               Just idx -> Just $! fmValues m VG.! idx
               Nothing -> Nothing

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
mapM f (FlatMap v keys) = do
  v' <- VG.mapM f v
  pure $! FlatMap v' keys

-- | /O(n)/ Apply monadic action to every value in map and its key.
imapM :: (Monad m, Vector v a, Vector v b)
      => (ByteString -> a -> m b)
      -> FlatMap (v a)
      -> m (FlatMap (v b))
imapM f (FlatMap v keys) = do
  v' <- VG.imapM (\idx a -> f (FlatSet.unsafeValueAt idx keys) a) v
  pure $! FlatMap v' keys

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