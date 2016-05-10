module Data.ByteString.FlatMap.Generic
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
  ) where

import           Data.ByteString.FlatMap.Internal

import           Prelude                          hiding
    (lookup, map, mapM, mapM_)
