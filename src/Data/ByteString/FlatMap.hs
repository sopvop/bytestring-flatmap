{-# LANGUAGE MultiParamTypeClasses #-}
module Data.ByteString.FlatMap
  ( FlatMap
  -- * Constructing
  , fromList
  -- * Querying
  , size
  , lookup
  , adjust
  -- * Mapping
  , map
  , imap
  -- * Monadic mapping
  , mapM
  , mapM_
  , imapM
  , imapM_
  -- * Converting
  , convert
  -- * Deconstructing
  , toVector
  , toList
  , toValues
  , toKeys
  , toValuesVector
  , toKeysVector
  ) where


import           Data.ByteString                  (ByteString)
import qualified Data.ByteString.FlatMap.Internal as G

import           Data.Vector                      (Vector)
import qualified Data.Vector                      as V
import qualified Data.Vector.Generic              as VG

import           Prelude                          hiding
    (lookup, map, mapM, mapM_)

type FlatMap a = G.FlatMap (Vector a)


-- | /O(n*log n)/ Create 'FlatMap' from list of keys and values.
-- If keys are specified several times, later values are ignored
fromList :: [(ByteString, a)] -> FlatMap a
fromList = G.fromList
{-# INLINE fromList #-}

-- | /O(1)/ number of elements in map
size :: FlatMap a -> Int
size = G.size
{-# INLINE size #-}

-- | /O(log n)/ Lookup value in map
lookup :: ByteString
       -> FlatMap a
       -> Maybe a
lookup = G.lookup
{-# INLINE lookup #-}

adjust :: (a -> a)
       -> ByteString
       -> FlatMap a
       -> FlatMap a
adjust = G.adjust
{-# INLINE adjust #-}
-- | /O(n)/ map a function over values
map :: (a -> b)
    -> FlatMap a
    -> FlatMap b
map = G.map
{-# INLINE map #-}


-- | /O(n)/ apply function to every element and it's key
imap :: (ByteString -> a -> b)
     -> FlatMap a
     -> FlatMap b
imap = G.imap
{-# INLINE imap #-}

-- | /O(n)/ monadic map over values in map
mapM :: Monad m
     => (a -> m b)
     -> FlatMap a
     -> m (FlatMap b)
mapM = G.mapM
{-# INLINE mapM #-}

-- | /O(n)/ Apply monadic action to every value in map and its key.
imapM :: Monad m
      => (ByteString -> a -> m b)
      -> FlatMap a
      -> m (FlatMap b)
imapM = G.imapM
{-# INLINE imapM #-}


-- | /O(n)/ Apply monadic action to every value in map, returning resulting map.
mapM_ :: Monad m
      => (a -> m b)
      -> FlatMap a
      -> m ()
mapM_ = G.mapM_
{-# INLINE mapM_ #-}

-- | /O(n)/ Apply monadic action to every value in map and its key.
imapM_ :: Monad m
       => (ByteString -> a -> m b)
       -> FlatMap a
       -> m ()
imapM_ = G.imapM_
{-# INLINE imapM_ #-}

convert :: (VG.Vector v a)
        => G.FlatMap (v a)
        -> FlatMap a
convert = G.convert
{-# INLINE convert #-}

toVector :: FlatMap a -> V.Vector (ByteString, a)
toVector = G.toVector
{-# INLINE toVector #-}

toList :: FlatMap a -> [(ByteString, a)]
toList = V.toList . toVector


toValuesVector :: FlatMap a -> Vector a
toValuesVector = G.toValuesVector
{-# INLINE toValuesVector #-}

toValues :: FlatMap a -> [a]
toValues = G.toValues
{-# INLINE toValues #-}

toKeysVector :: FlatMap v -> Vector ByteString
toKeysVector = G.toKeysVector
{-# INLINE toKeysVector #-}

toKeys :: FlatMap v -> [ByteString]
toKeys = G.toKeys
{-# INLINE toKeys #-}
