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
  , tests
  ) where
{-
    ( Order(_map, _vec)
    , toPairList
    , fromPairsUnsafe
    , fromPairs
    , overPairs
    , ioverPairs
    -- * module Data.Order.One
    , IndexedOrder, pairs, keys, keysSet, values, pos
    , AList
    , AssocList(..)
    , One(OneItem, one)
    , delete
    , singleton
    , lookup
    , partition
    , filter
    -- , splitAt, take, drop
    -- * Operators
    , ilookup, view
    , next, Data.Order.mapKeys
    , Data.Order.member, permute, permuteUnsafe
    , sortBy, appendEnum
    -- * Positional operations
    , lookupKey, lookupPair
    , insertPairAt, insertPairAtWith
    , deleteAt
    , appendPair, appendElem
    , prependPair, prependElem
    , Data.Order.takeWhile
    -- * Is/Has classes and instances
{-
    , IsMap(..)
    , HasMap(..)
    , IsOrder(..)
    , HasOrder(..)
-}
    , tests
    ) where
-}

import Control.Lens hiding (uncons, view)
import qualified Data.Foldable as Foldable
import qualified Data.List as List (sortBy)
import qualified Data.ListLike as ListLike
import Data.EnumMap as EnumMap ((!), EnumMap)
import qualified Data.EnumMap as EnumMap
import Data.Foldable (foldr)
import Data.List as List (nub)
import qualified Data.ListLike as LL
import Data.Maybe (isNothing)
import Data.Monoid
import Data.Order.AssocList
import Data.Order.One
import Data.Order.Order
import Data.SafeCopy (SafeCopy(..), safeGet, safePut)
import qualified Data.Semigroup as Sem
import Data.Serialize (Serialize(..))
import qualified Data.Set as Set (fromList, insert, member)
import Data.Typeable (Proxy(Proxy), Typeable, typeRep)
import Data.Vector (Vector) -- hiding ((!), break, drop, dropWhile, foldr, fromList, head, length, null, sequence, singleton, take, toList)
import qualified Data.Vector as Vector
import Extra.QuickCheck
import qualified GHC.Exts as GHC
import Prelude hiding (break, drop, filter, foldMap, length, lookup, map, take, zip)
import Test.QuickCheck
import Text.PrettyPrint.HughesPJClass (Pretty(pPrint), text)

prop_fromPairs :: Order Char String -> Bool
prop_fromPairs o = fromPairs (pairs o) == o

-- | Lookup key by position.  A lookup function appears in
-- containers in version 0.5.8.
lookupKey :: (Ord k, Enum k) => Int -> Order k a -> Maybe k
lookupKey i o | i < 0 || i >= Foldable.length (keys o) = Nothing
lookupKey i o = Just (ListLike.index (keys o) i)

data InsertPosition k v = InsertPosition (Order k v) Int deriving Show

data ElementPosition k v = ElementPosition (Order k v) (Maybe Int) deriving Show

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

{-
filter :: Enum k => (k -> a -> Bool) -> Order k a -> Order k a
filter f o = Order (EnumMap.filterWithKey f (_map o)) (Vector.filter (\k -> f k (_map o ! k)) (_vec o))

partition :: Enum k => (k -> Bool) -> Order k a -> (Order k a, Order k a)
partition f o =
  let (m1, m2) = EnumMap.partitionWithKey (\k _ -> f k) (_map o)
      (v1, v2) = Vector.partition f (_vec o) in
  (Order m1 v1, Order m2 v2)

break :: (Enum k, Ord k) => ((k, a) -> Bool) -> Order k a -> (Order k a, Order k a)
break f o =
  (Order mbefore before, Order mafter after)
  where
    f' k = f (k, _map o ! k)
    (before, after) = Vector.break f' (_vec o)
    beforeSet = foldr Set.insert mempty before
    (mbefore, mafter) = EnumMap.partitionWithKey (\k _ -> Set.member k beforeSet) (_map o)

splitAt :: (Enum k, Ord k) => Int -> Order k a -> (Order k a, Order k a)
splitAt i o =
  (Order mbefore before, Order mafter after)
  where
    (before, after) = Vector.splitAt i (_vec o)
    beforeSet = foldr Set.insert mempty before
    (mbefore, mafter) = EnumMap.partitionWithKey (\k _ -> Set.member k beforeSet) (_map o)
-}

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

-- | Like 'Data.Set.minView', if k is present returns the position,
-- associated value, and the order with that value removed.
view :: (Enum k, Ord k) => k -> Order k a -> Maybe (Int, a, Order k a)
view k o =
  case ilookup k o of
    Just (i, a) -> Just (i, a, Order (EnumMap.delete k (_map o)) (ListLike.filter (/= k) (_vec o)))
    Nothing -> Nothing

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

newtype LL a = LL {unLL :: a} deriving (Eq, Ord)

instance Monoid a => Monoid (LL a) where
  mempty = LL mempty
#if !(MIN_VERSION_base(4,11,0))
  mappend = (<>)
#endif

instance Sem.Semigroup a => Sem.Semigroup (LL a) where
  (<>) (LL a) (LL b) = LL (a <> b)

-- A ListLike instance hidden inside the newtype LL.
instance (Ord k, Enum k, LL.FoldableLL (LL (Order k v)) (k, v)) => LL.ListLike (LL (Order k v)) (k, v) where
  singleton :: (k, v) -> LL (Order k v)
  singleton = LL . one
  null :: LL (Order k v) -> Bool
  null = Foldable.null . unLL
  uncons :: LL (Order k v) -> Maybe ((k, v), LL (Order k v))
  uncons (LL o) = over (_Just . _2) LL (uncons o)

  drop :: Int -> LL (Order k a) -> LL (Order k a)
  drop n = LL . drop n . unLL

  take :: Int -> LL (Order k a) -> LL (Order k a)
  take n = LL . take n . unLL

  splitAt :: Int -> LL (Order k a) -> (LL (Order k a), LL (Order k a))
  splitAt n (LL o) = over _1 LL $ over _2 LL $ Data.Order.One.splitAt n o

instance (Ord k, Enum k) => GHC.IsList (LL (Order k v)) where
  type Item (LL (Order k v)) = (k, v)
  fromList = LL . fromPairsUnsafe
  toList = toPairList . unLL

instance (Enum k, Ord k, Monoid (Order k v)) => LL.FoldableLL (LL (Order k v)) (k, v) where
    foldl f r0 (LL xs) = Foldable.foldl (\r k -> f r (k, _map xs ! k)) r0 (_vec xs)
    foldr f r0 (LL xs) = Foldable.foldr (\k r -> f (k, _map xs ! k) r) r0 (_vec xs)

-- Quickcheck

-- Build an arbitrary order and a valid insert position for that order
instance (Ord k, Enum k, Arbitrary k, Arbitrary v) => Arbitrary (InsertPosition k v) where
  arbitrary = do
      o <- arbitrary :: Gen (Order k v)
      InsertPosition o <$> choose (0, Foldable.length o)

instance (Ord k, Enum k, Arbitrary k, Arbitrary v) => Arbitrary (ElementPosition k v) where
  arbitrary = do
      o <- arbitrary :: Gen (Order k v)
      case Foldable.length o of
        0 -> return $ ElementPosition o Nothing
        n -> ElementPosition o <$> (Just <$> choose (0, pred n))

instance (Enum k, Ord k, {-Show k,-} Arbitrary k, Arbitrary v) => Arbitrary (Order k v) where
  arbitrary = do
      (ks :: [k]) <- (sized pure >>= \n -> vectorOf n arbitrary) >>= shuffle
      let ks' = nub ks
      (vs :: [v]) <- vector (ListLike.length ks')
      return (fromPairs (ListLike.zip ks' vs :: [(k, v)]) :: Order k v)

prop_lookupKey :: (k ~ Char, a ~ String) => InsertPosition k a -> a -> Bool
prop_lookupKey (InsertPosition o i) a =
    lookupKey i o' == Just k && lookupKey (Foldable.length o) o == Nothing
    where o' = insertPairAt i (k, a) o
          k = next o

-- If we insert (next o, a) into o at position i, we should then find a at
-- k in the new order and we should not find it in the original
-- order.
prop_lookup :: (k ~ Char, a ~ String) => InsertPosition k a -> a -> Bool
prop_lookup (InsertPosition o i) a =
    lookup k o' == Just a && lookup k o == Nothing
    where o' = insertPairAt i (k, a) o
          k = next o

prop_lookupPair :: (k ~ Char, a ~ String) => InsertPosition k a -> a -> Bool
prop_lookupPair (InsertPosition o i) a =
    lookupPair i o' == Just (k, a) && lookupPair (Foldable.length o) o == Nothing
    where o' = insertPairAt i (k, a) o
          k = next o

prop_keys :: Order Char String -> Bool
prop_keys o = EnumMap.keysSet (_map o) == Set.fromList (Foldable.toList (_vec o))

prop_splitAt :: (k ~ Char, a ~ String) => ElementPosition k a -> Bool
prop_splitAt (ElementPosition o i) =
    let (a, b) = ListLike.splitAt (maybe 0 id i) (LL o) in
    o == unLL (a <> b)

prop_next :: Order Char String -> Bool
prop_next o = isNothing (ListLike.elemIndex (next o) (keys o))

prop_uncons :: Order Char Int -> Bool
prop_uncons o =
   o == maybe mempty (\(pair, o') -> one pair <> o') (uncons o)

prop_null :: Order Char Int -> Bool
prop_null o = Foldable.null o == isNothing (ListLike.uncons (LL o))

prop_singleton :: (Char, Int) -> Bool
prop_singleton pair = ListLike.uncons (ListLike.singleton pair) == Just (pair, LL (mempty :: Order Char Int))

-- | Map and list should contain the same keys with no duplicates
prop_toPairs_fromPairs :: Order Int String -> Bool
prop_toPairs_fromPairs o =
    fromPairsUnsafe (toPairs o :: Vector (Int, String)) == o

prop_delete :: Order Int String -> Property
prop_delete o | Foldable.length o == 0 = property True
prop_delete o =
    forAll (choose (0, Foldable.length o - 1)) $ \i ->
    Foldable.length (deleteAt i o) == Foldable.length o - 1

prop_insertAt :: (Int, String) -> Order Int String -> Property
prop_insertAt v@(k, _) o =
    forAll (choose (0, Foldable.length o)) $ \i ->
    Data.Order.member k o || (Foldable.length (insertPairAt i v o) == Foldable.length o + 1)

{-
prop_insertAt_deleteAt :: (Int, String) -> Order Int String -> Property
prop_insertAt_deleteAt v@(k, _) o =
  forAll (choose (0, Foldable.length o)) $ \i ->
  if Data.Order.member k o
  then deleteAt i o == deleteAt i (insertPairAt i v (deleteAt i o))
  else o == deleteAt i (insertPairAt i v o)
-}

-- | Use an explicit generator to create a valid list position.
prop_insert_delete :: (Int, String) -> Order Int String -> Property
prop_insert_delete (k, a) o =
    forAll (choose (0, Foldable.length o)) $ \i ->
        Data.Order.member k o || (view k (insertPairAt i (k, a) o) == Just (i, a, o))

prop_insert_delete_pos :: (Int, String) -> Order Int String -> Property
prop_insert_delete_pos v@(k, _) o =
    forAll (choose (0, Foldable.length o)) $ \i ->
        Data.Order.member k o || (deleteAt i (insertPairAt i v o) == o)

tests :: IO Result
tests = do
  mconcat <$> sequence
    [ quickCheckResult $ withMaxSuccess 100 prop_keys
    , quickCheckResult $ withMaxSuccess 100 prop_lookup
    , quickCheckResult $ withMaxSuccess 100 prop_lookupKey
    , quickCheckResult $ withMaxSuccess 100 prop_lookupPair
    , quickCheckResult $ withMaxSuccess 100 prop_splitAt
    , quickCheckResult $ withMaxSuccess 100 prop_next
    , quickCheckResult $ withMaxSuccess 100 prop_uncons
    , quickCheckResult $ withMaxSuccess 100 prop_null
    , quickCheckResult $ withMaxSuccess 100 prop_singleton
    , quickCheckResult $ withMaxSuccess 100 prop_toPairs_fromPairs
    , quickCheckResult $ withMaxSuccess 100 prop_delete
    , quickCheckResult $ withMaxSuccess 100 prop_insertAt
    , quickCheckResult $ withMaxSuccess 100 prop_insert_delete
    , quickCheckResult $ withMaxSuccess 100 prop_insert_delete_pos
    , quickCheckResult $ withMaxSuccess 100 prop_fromPairs
    ] >>= throwResult
