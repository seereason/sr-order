-- | Re-export the conventional (non-contraveriant) symbols from
-- assoc-listlike.  Also adds some operations that assume the key is
-- an Enum.

module Data.AssocList
  ( -- * Constraint
    AssocList

    -- * Exception
  , MissingAssocListKey(MissingAssocListKey)

    -- * Sorting
  , sortKeys

    -- * Lookup
  , lookupFirst
  , lookupAll

    -- * Removal
  , removeFirst
  , removeAll

    -- * Mapping
    -- $mapping
  , mapFirst
  , mapAll

    -- * Alteration
    -- $alteration
  , alterFirst
  , alterAll

    -- * Grouping
  , partition
  , break
  , breakPartition

    -- * Operators
  , (!)
  , (!?)

    -- * Enum operations
  , keys
  , values
  , next
  , appendEnum
  , prependEnum
  , mapKeys
  , insertAt
  , insertPairAt
  ) where

-- import Data.AssocList.ListLike.Comparison
import Data.AssocList.ListLike.Concept
import Data.AssocList.ListLike.Eq
-- import Data.AssocList.ListLike.Equivalence
import Data.AssocList.ListLike.Ord
-- import Data.AssocList.ListLike.Predicate

import Control.Lens (_1, _2, over)
import Data.ListLike as LL hiding (break, partition)

import Prelude hiding (break, splitAt)

keys :: (AssocList o k v) => o -> [k]
-- keys :: (Functor f, ListLike full (Item full), ListLike (f (b1, b2)) (GHC.Exts.Item full)) => full -> f b1
keys = fmap fst . fromListLike
{-# INLINEABLE keys #-}

values :: (AssocList o k v) => o -> [v]
values = fmap snd . fromListLike
{-# INLINEABLE values #-}

next :: (AssocList o k v, Enum k, Ord k) => o -> k
next = succ . foldl' max (toEnum 0) . keys
{-# INLINEABLE next #-}

appendEnum :: (AssocList o k v, Enum k, Ord k) => v -> o -> o
appendEnum v o = o <> singleton (next o, v)
{-# INLINEABLE appendEnum #-}

prependEnum :: (AssocList o k v, Enum k, Ord k) => v -> o -> o
prependEnum v o = singleton (succ (foldl' max (toEnum 0) (keys o)), v) <> o
{-# INLINEABLE prependEnum #-}

mapKeys :: (AssocList o k v, AssocList o' k' v) => (k -> k') -> o -> o'
mapKeys f = fromList . fmap (over _1 f) . fromListLike
{-# INLINEABLE mapKeys #-}

insertAt :: (AssocList o k v, Enum k, Ord k) => Int -> v -> o -> (o, k)
insertAt n v o = let k = next o in (insertPairAt n (k, v) o, k)
{-# INLINEABLE insertAt #-}

-- | Attempting to insert an existing k may give surprising results.
-- Best to delete it first.
insertPairAt :: AssocList o k v => Int -> (k, v) -> o -> o
insertPairAt n (k, v) o = uncurry (<>) $ over _2 (singleton (k, v) <>) $ splitAt n o
{-# INLINEABLE insertPairAt #-}
