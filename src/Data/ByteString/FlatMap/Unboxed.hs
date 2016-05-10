{-# LANGUAGE MultiParamTypeClasses #-}
module Data.ByteString.FlatMap.Unboxed
  ( FlatMap
  -- * Constructing
  , fromList
  -- * Querying
  , size
  , lookup
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
  )
  where

import           Data.ByteString                  (ByteString)
import qualified Data.ByteString.FlatMap.Internal as G

import qualified Data.Vector                      as V
import qualified Data.Vector.Generic              as VG
import           Data.Vector.Unboxed              (Unbox, Vector)

import           Prelude                          hiding
    (lookup, map, mapM, mapM_)

type FlatMap a = G.FlatMap (Vector a)

-- | /O(n*log n)/ Create 'FlatMap' from list of keys and values.
-- If keys are specified several times, later values are ignored
fromList :: Unbox a => [(ByteString, a)] -> FlatMap a
fromList = G.fromList
{-# INLINE fromList #-}

-- | /O(1)/ number of elements in map
size :: FlatMap a -> Int
size = G.size
{-# INLINE size #-}

-- | /O(log n)/ Lookup value in map
lookup :: Unbox a
       => ByteString
       -> FlatMap a
       -> Maybe a
lookup = G.lookup
{-# INLINE lookup #-}

-- | /O(n)/ map a function over values
map :: (Unbox a, Unbox b)
    => (a -> b)
    -> FlatMap a
    -> FlatMap b
map = G.map
{-# INLINE map #-}


-- | /O(n)/ apply function to every element and it's key
imap :: (Unbox a, Unbox b)
     => (ByteString -> a -> b)
     -> FlatMap a
     -> FlatMap b
imap = G.imap
{-# INLINE imap #-}

-- | /O(n)/ monadic map over values in map
mapM :: (Monad m, Unbox a, Unbox b)
     => (a -> m b)
     -> FlatMap a
     -> m (FlatMap b)
mapM = G.mapM
{-# INLINE mapM #-}

-- | /O(n)/ Apply monadic action to every value in map and its key.
imapM :: (Monad m, Unbox a, Unbox b)
      => (ByteString -> a -> m b)
      -> FlatMap a
      -> m (FlatMap b)
imapM = G.imapM
{-# INLINE imapM #-}


-- | /O(n)/ Apply monadic action to every value in map, returning resulting map.
mapM_ :: (Monad m, Unbox a)
      => (a -> m b)
      -> FlatMap a
      -> m ()
mapM_ = G.mapM_
{-# INLINE mapM_ #-}

-- | /O(n)/ Apply monadic action to every value in map and its key.
imapM_ :: (Monad m, Unbox a)
       => (ByteString -> a -> m b)
       -> FlatMap a
       -> m ()
imapM_ = G.imapM_
{-# INLINE imapM_ #-}

convert :: (VG.Vector v a, Unbox a)
        => G.FlatMap (v a)
        -> FlatMap a
convert = G.convert
{-# INLINE convert #-}

toVector :: Unbox a => FlatMap a -> V.Vector (ByteString, a)
toVector = G.toVector
{-# INLINE toVector #-}

toList :: Unbox a => FlatMap a -> [(ByteString, a)]
toList = V.toList . toVector
