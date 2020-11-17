{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS -Wall #-}

-- | A combination of a Map and a Sequence - that is, each element of the
-- Map has a unique sequence number between zero and the number of
-- elements in the map, and this sequence can be modified.  There is a
-- ListLike instance that treats it as a sequence of (k, a) pairs.

module Data.Order
  ( module Data.Order.Order
  , module Data.Order.One
  , module Data.Order.AssocList
  , Order
  , tests
  ) where

import qualified Data.Foldable as Foldable
import Data.Int
import Data.Monoid
import Data.Order.AssocList
import Data.Order.One
import Data.Order.Order
import Data.SafeCopy (extension, SafeCopy(..))
import Extra.QuickCheck
import Data.SafeCopy (Migrate(..), SafeCopy')
import Prelude hiding (break, drop, filter, foldMap, length, lookup, map, take, zip)
import Test.QuickCheck

#if 1
type Order k = MapAndVec k
#else
-- DANGER - if you uncomment this all your Orders will be converted to
-- AssocLists, and you can't reverse this without adding more
-- migrations and safecopy instances.  (Or re-syncing the database.)
--
-- I did try this change and it worked, but it did not seem to help in
-- any way.  It may have been somewhat slower.

type Order k = AssocList k

instance (SafeCopy' k, Ord k, Enum k, SafeCopy' v) => SafeCopy (AssocList k v) where version = 4; kind = extension

instance (Ord k, Enum k, SafeCopy' k, SafeCopy' v) => Migrate (AssocList k v) where
  type MigrateFrom (AssocList k v) = MapAndVec k v
  migrate = AssocList . pairs
#endif

type Key = Integer

data InsertPosition k v = InsertPosition (Order k v) Int deriving Show

data ElementPosition o k v = ElementPosition (o v) (Maybe Int) deriving Show

#if 0
insertPairAtWith :: Enum k => (a -> a -> a) -> Int -> (k, a) -> Order k a -> Order k a
insertPairAtWith f n (k, new) o =
  case EnumMap.lookup k (_map o) of
    Nothing ->
      let (a, b) = Vector.splitAt n (_vec o) in
      Order (EnumMap.insert k new (_map o)) (a <> Vector.singleton k <> b)
    Just _old ->
      Order
        (EnumMap.insertWith f k new (_map o))
        (uncurry (<>) $ over _2 (Vector.cons k) $ Vector.splitAt n (_vec o))

takeWhile :: (Enum k, Ord k) => ((k, a) -> Bool) -> Order k a -> Order k a
takeWhile f o =
  Order mbefore before
  where
    f' k = f (k, _map o ! k)
    before = Vector.takeWhile f' (_vec o)
    beforeSet = foldr Set.insert mempty before
    mbefore = EnumMap.filterWithKey (\k _ -> Set.member k beforeSet) (_map o)

appendPair :: (Enum k, Ord k) => Order k a -> (k, a) -> Order k a
appendPair m (k, a) = m <> one (k, a)

appendElem :: (Enum k, Ord k) => Order k a -> a -> (Order k a, k)
appendElem o a = let k = next o in (appendPair o (k, a), k)

prependPair :: (Enum k, Ord k) => (k, a) -> Order k a -> Order k a
prependPair (k, a) o = one (k, a) <> o

prependElem :: (Enum k, Ord k) => a -> Order k a -> (Order k a, k)
prependElem a o = let k = next o in (prependPair (k, a) o, k)

-- | Based on Data.Map.mapKeys, because Vector is not a functor we
-- need to eliminate pairs when two k1 are mapped to the same k2.
mapKeys :: (Enum k1, Enum k2) => (k1 -> k2) -> Order k1 a -> Order k2 a
mapKeys f o = Order (EnumMap.mapKeys f (_map o)) (fmap f (_vec o))

ilookup :: (Enum k, Ord k) => k -> Order k a -> Maybe (Int, a)
ilookup k o =
    case EnumMap.lookup k (_map o) of
      Nothing -> Nothing
      Just a -> Just (Foldable.length (fst (ListLike.break (== k) (_vec o))), a)

member :: (Enum k) => k -> Order k v -> Bool
member k o = EnumMap.member k (_map o)

sortBy :: forall k v. Enum k => ((k, v) -> (k, v) -> Ordering) -> Order k v -> Order k v
sortBy cmp o = Order (_map o) (Vector.fromList $ List.sortBy cmp' $ Vector.toList $ _vec o)
  where
    cmp' :: k -> k -> Ordering
    cmp' k1 k2 = cmp (k1, _map o ! k1) (k2, _map o ! k2)

vectorUncons :: Vector k -> Maybe (k, Vector k)
vectorUncons x = if Vector.null x then Nothing else Just (Vector.head x, Vector.tail x)

appendEnum :: (Enum k, Ord k) => v -> Order k v -> Order k v
appendEnum v o = o <> one (next o, v)
#endif

-- Quickcheck

-- Build an arbitrary order and a valid insert position for that order
instance (Ord k, Enum k, Arbitrary k, Arbitrary v) => Arbitrary (InsertPosition k v) where
  arbitrary = do
      o <- arbitrary :: Gen (Order k v)
      InsertPosition o <$> choose (0, Foldable.length o)

instance (Ordered o k v, Arbitrary (o v), Arbitrary k, Arbitrary v) => Arbitrary (ElementPosition o k v) where
  arbitrary = do
      o <- arbitrary :: Gen (o v)
      case Foldable.length o of
        0 -> return $ ElementPosition o Nothing
        n -> ElementPosition o <$> (Just <$> choose (0, pred n))

tests :: IO Result
tests = do
  mconcat <$> sequence
    [ quickCheckResult $ withMaxSuccess 100 (prop_keys @(Order Key) @String)
    , quickCheckResult $ withMaxSuccess 100 (prop_keys @(AssocList Key) @String)
    , quickCheckResult $ withMaxSuccess 100 (prop_lookup @(Order Key) @String)
    , quickCheckResult $ withMaxSuccess 100 (prop_lookupKey @(Order Key) @String)
    , quickCheckResult $ withMaxSuccess 100 (prop_lookupPair @(Order Key) @String)
    , quickCheckResult $ withMaxSuccess 100 (prop_splitAt @(Order Key) @String)
    , quickCheckResult $ withMaxSuccess 100 (prop_next @(Order Key) @String)
    , quickCheckResult $ withMaxSuccess 100 (prop_uncons @(Order Key) @String)
    , quickCheckResult $ withMaxSuccess 100 (prop_null @(Order Key) @String)
    , quickCheckResult $ withMaxSuccess 100 (prop_singleton @(Order Key) @String)
    , quickCheckResult $ withMaxSuccess 100 (prop_toPairs_fromPairs @(Order Key) @String)
    , quickCheckResult $ withMaxSuccess 100 (prop_delete @(Order Key) @String)
    , quickCheckResult $ withMaxSuccess 100 (prop_insertAt @(Order Key) @String)
    , quickCheckResult $ withMaxSuccess 100 (prop_insert_delete @(Order Key) @String)
    , quickCheckResult $ withMaxSuccess 100 (prop_insert_delete_pos @(Order Key) @String)
    , quickCheckResult $ withMaxSuccess 100 (prop_fromPairs @(Order Key) @String)
    , quickCheckResult $ withMaxSuccess 100 (prop_fromPairs @(AssocList Key) @String)
    , quickCheckResult $ withMaxSuccess 100 (prop_pos_insertAt @(Order Key) @())
    ] >>= throwResult
