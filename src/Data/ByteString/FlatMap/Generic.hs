module Data.ByteString.FlatMap.Generic
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
  -- * Folds
  , foldl'
  , foldl1'
  , foldr
  , foldr1
  -- * Folds with key
  , ifoldl'
  , ifoldr'
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

import           Data.ByteString.FlatMap.Internal

import           Prelude                          hiding
    (lookup, map, mapM, mapM_)
