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
    ( Order(_map, _vec)
#if 0
    , toPairList
#endif
    , toPairs
    , fromPairs
    , overPairs
    , ioverPairs
    -- * Operators
    , keys, values, pos, Data.Order.lookup, ilookup, view
    , Data.Order.next, Data.Order.mapKeys
    , Data.Order.member, permute, permuteUnsafe
    , sortBy
    -- * Positional operations
    , lookupKey, lookupPair
    , insertAt, insertPairAt
    , deleteAt
    , appendPair, appendElem
    , prependPair, prependElem
    -- * Is/Has classes and instances
{-
    , IsMap(..)
    , HasMap(..)
    , IsOrder(..)
    , HasOrder(..)
-}
    , tests
    ) where

import Control.Lens hiding (uncons, view)
import qualified Data.Foldable as Foldable
import qualified Data.List as List (sortBy)
import Data.ListLike (FoldableLL, ListLike)
import qualified Data.ListLike as ListLike
import Data.EnumMap as EnumMap ((!))
import qualified Data.EnumMap as EnumMap
import Data.List as List (nub)
--import Data.Map (Map)
--import qualified Data.Map as Map (fromList{-, toList-})
import Data.Maybe (isNothing)
import Data.Monoid
import Data.Order.Type (fromPairs, Order(..), overPairs, ioverPairs, toPairs, unsafeOrder)
import Data.SafeCopy (SafeCopy(..), safeGet, safePut)
import Data.Serialize (Serialize(..))
import qualified Data.Semigroup as Sem
import qualified Data.Set as Set (fromList)
import Data.Typeable (Proxy(Proxy), Typeable, typeRep)
import Data.Vector hiding ((!), break, dropWhile, foldr, fromList, head, length, null, sequence, singleton, toList)
import qualified Data.Vector as Vector
import Extra.QuickCheck
import qualified GHC.Exts as GHC
import Prelude hiding (break, foldMap, length, lookup, map, splitAt, zip)
import Test.QuickCheck
import Text.PrettyPrint.HughesPJClass (Pretty(pPrint), text)

instance SafeCopy (Order k v) => Serialize (Order k v) where
    put = safePut
    get = safeGet

instance forall k v. (Ord k, Enum k, Show k, Show v, Typeable k, Typeable v) => Show (Order k v) where
  show o = "fromPairs " <> show (Vector.toList (toPairs o)) <> " :: Order (" <> show (typeRep (Proxy :: Proxy k)) <> ") (" <> show (typeRep (Proxy :: Proxy v)) <> ")"

instance forall k v. (Ord k, Enum k, Pretty k, Pretty v, Typeable k, Typeable v) => Pretty (Order k v) where
  pPrint o = text "Order " <> pPrint (Vector.toList (toPairs o))

-- Fold over the values only
-- @@
-- λ> foldMap id (fromList [(1, "a"), (2, "b")])
-- "ab"
-- @@
instance (Enum k, Ord k) => Foldable (Order k) where
    foldMap f o = Foldable.foldMap (\k -> f (_map o ! k)) (_vec o)

-- Fold over keys and values
-- @@
-- λ> ifoldMap (\k v-> show k ++ v) (fromList [(2, "a"), (5, "b")])
-- "2a5b"
-- @@
instance (Enum k, Ord k) => FoldableWithIndex k (Order k) where
    ifoldMap f o = Foldable.foldMap (\k -> f k (_map o ! k)) (_vec o)

-- @@
-- λ> ifoldMap (\k v-> ListLike.concat (ListLike.replicate k v :: [String])) (fromPairs (ListLike.fromList [(2, "a"), (5, "b")]))
-- "aabbbbb"
-- @@
instance Enum k => FunctorWithIndex k (Order k) where
    imap f o = unsafeOrder (EnumMap.mapWithKey f (_map o)) (_vec o)

prop_fromPairs :: Order Char String -> Bool
prop_fromPairs o = fromPairs (toPairs o) == o

-- | Lookup key by position.  A lookup function appears in
-- containers in version 0.5.8.
lookupKey :: Eq k => Int -> Order k a -> Maybe k
lookupKey i o | i < 0 || i >= Foldable.length (keys o) = Nothing
lookupKey i o = Just (ListLike.index (keys o) i)

data InsertPosition k v = InsertPosition (Order k v) Int deriving Show

data ElementPosition k v = ElementPosition (Order k v) (Maybe Int) deriving Show

-- | Like Map.lookup.
lookup :: (Enum k) => k -> Order k a -> Maybe a
lookup k o = EnumMap.lookup k (_map o)

-- | Lookup pair by position
lookupPair :: (Eq k, Enum k) => Int -> Order k a  -> Maybe (k, a)
lookupPair i o = lookupKey i o >>= (\k -> fmap (k,) (Data.Order.lookup k o))

insertAt :: (Enum k, Ord k) => Int -> a -> Order k a -> (Order k a, k)
insertAt n a o = let k = next o in (insertPairAt n (k, a) o, k)

insertPairAt :: (Enum k, Ord k) => Int -> (k, a) -> Order k a -> Order k a
insertPairAt n (k, a) o =
  unsafeOrder
    (EnumMap.insert k a (_map o))
    (uncurry (<>) $ over _2 (Vector.cons k) $ (Vector.splitAt n (_vec o)))
-- insertPairAt n (k, a) o = uncurry (<>) $ over _2 (ListLike.singleton (k, a) <>) $ ListLike.splitAt n o

deleteAt :: (Enum k, Ord k) => Int -> Order k a -> Order k a
deleteAt n o | Vector.length (_vec o) <= n = o
deleteAt n o =
  let (a, b) = Vector.splitAt n (_vec o) in
    let (k, b') = uncons b in
      unsafeOrder (EnumMap.delete k (_map o)) (a <> b')
  where
    uncons v = let (h, t) = Vector.splitAt 1 v in (Vector.head h, t)
-- deleteAt n = uncurry (<>) . over _2 (ListLike.drop 1) . ListLike.splitAt n

singleton :: (Enum k, Ord k) => k -> a -> Order k a
singleton k a = unsafeOrder (EnumMap.singleton k a) (Vector.singleton k)

appendPair :: (Enum k, Ord k) => Order k a -> (k, a) -> Order k a
appendPair m (k, a) = m <> Data.Order.singleton k a

appendElem :: (Enum k, Ord k) => Order k a -> a -> (Order k a, k)
appendElem o a = let k = next o in (appendPair o (k, a), k)

prependPair :: (Enum k, Ord k) => (k, a) -> Order k a -> Order k a
prependPair (k, a) o = Data.Order.singleton k a <> o

prependElem :: (Enum k, Ord k) => a -> Order k a -> (Order k a, k)
prependElem a o = let k = next o in (prependPair (k, a) o, k)

-- | Return the keys in order.
keys :: Order k a -> Vector k
keys = _vec

-- | Return the position of a key, Nothing if absent.
pos :: Eq k => k -> Order k a -> Maybe Int
pos k o =
    case ListLike.break (== k) (_vec o) of
      (_, x) | ListLike.null x -> Nothing
      (x, _) -> Just (ListLike.length x)

-- | Return the values in order.
values :: (Eq k, Enum k) => Order k a -> [a]
values x = fmap (\k -> _map x ! k) (GHC.toList (keys x))

-- | Based on Data.Map.mapKeys, because Vector is not a functor we
-- need to eliminate pairs when two k1 are mapped to the same k2.
mapKeys :: (Enum k1, Enum k2, Eq k2) => (k1 -> k2) -> Order k1 a -> Order k2 a
mapKeys f o = unsafeOrder (EnumMap.mapKeys f (_map o)) (fmap f (_vec o))

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
    Just (i, a) -> Just (i, a, unsafeOrder (EnumMap.delete k (_map o)) (ListLike.filter (/= k) (_vec o)))
    Nothing -> Nothing

member :: (Enum k) => k -> Order k v -> Bool
member k o = EnumMap.member k (_map o)

instance (Ord k, Eq v) => Eq (Order k v) where
  a == b = _map a == _map b && _vec a == _vec b

instance (Enum k, Ord k, Eq v, Ord v) => Ord (Order k v) where
    compare a b = compare (_vec a) (_vec b) <> compare (_map a) (_map b)

-- @@
-- λ> traverse (++ "!") (Data.Order.fromPairs [('a', "1"),('b',"2")] :: Order Char String)
-- [fromList [('a','1'),('b','2')], fromList [('a','1'),('b','!')],fromList [('a','!'),('b','2')],fromList [('a','!'),('b','!')]] :: [Order Char Char]
-- @@
instance (Enum k, Ord k) => Traversable (Order k) where
    traverse f o = unsafeOrder <$> traverse f (_map o) <*> pure (_vec o)

instance (Enum k, Ord k) => TraversableWithIndex k (Order k) where
    itraverse f o = unsafeOrder <$> itraverse (\k a -> f (toEnum k) a) (_map o) <*> pure (_vec o)

-- | An Order has two notions of an 'Index', the 'Int' index of the
-- list and the @k@ index of the map.  Here is the 'Ixed' instance
-- for the latter, which only needs to address the _map field.
-- @@
-- λ> set (ix 'a') 30 (fromPairs [('a',10),('b',20)])
-- fromPairs [('a',30),('b',20)] :: Order (Char) (Integer)
-- @@

type instance Index (Order k a) = k
type instance IxValue (Order k a) = a
instance Enum k => Ixed (Order k a) where
    ix k f o =
        case EnumMap.lookup k (_map o) of
          Just a -> fmap (\a' -> unsafeOrder (EnumMap.insert k a' (_map o)) (_vec o)) (f a)
          Nothing -> pure o

-- | 'At' instance.
-- @@
-- λ> set (at 'a') Nothing (fromPairs [('a',10),('b',20)])
-- fromPairs [('b',20)] :: Order (Char) (Integer)
-- λ> set (at 'b') (Just 40) (fromPairs [('a',10),('b',20)])
-- fromPairs [('a',10),('b',40)] :: Order (Char) (Integer)
-- @@
-- New elements appear at the end (positions of existing elements do
-- not change):
-- @@
-- λ> set (at 'x') (Just 30) (fromPairs [('a',10),('b',20)])
-- fromPairs [('a',10),('b',20),('x',30)] :: Order (Char) (Integer)
-- @@
instance (Enum k, Eq k) => At (Order k a) where
    at k f o =
        case EnumMap.lookup k (_map o) of
          Just a ->
              fmap (maybe (unsafeOrder (EnumMap.delete k (_map o)) (ListLike.filter (/= k) (_vec o)))
                          (\a' -> unsafeOrder (EnumMap.insert k a' (_map o)) (_vec o)))
                   (f (Just a))
          Nothing ->
              fmap (maybe o
                          (\a' -> unsafeOrder (EnumMap.insert k a' (_map o)) (_vec o <> ListLike.singleton k)))
                   (f Nothing)

instance (Enum k, Ord k, Monoid (Order k v)) => FoldableLL (Order k v) (k, v) where
    foldl f r0 xs = Foldable.foldl (\r k -> f r (k, _map xs ! k)) r0 (_vec xs)
    foldr f r0 xs = Foldable.foldr (\k r -> f (k, _map xs ! k) r) r0 (_vec xs)

-- | Return the next available key
-- @@
-- λ> next (fromPairs (Vector.fromList [(2, "a"), (5, "b")]))
-- 6
-- λ> next (fromPairs (Data.Vector.fromList []) :: Order Int String)
-- 0
-- @@
next :: Enum k => Order k a -> k
next o = head (dropWhile (`EnumMap.member` (_map o)) [toEnum 0 ..])
-- next (Order m _) = maybe (toEnum 0) (succ . toEnum . fst) (Set.maxView (EnumMap.keysSet m))

-- | Replace the current ordering with the given key list.  Duplicate
-- keys are ignored, missing keys are appended.
permuteUnsafe :: forall k v. (Enum k, Ord k) => Vector k -> Order k v -> Order k v
permuteUnsafe neworder o =
    unsafeOrder (_map o) (present <> missing)
    where
      -- Remove unknown keys
      present :: Vector k
      present = GHC.fromList (List.nub (GHC.toList (ListLike.filter (`EnumMap.member` _map o) neworder)))
      -- Collect the values that are missing from the new key order
      missing :: Vector k
      missing = GHC.fromList (EnumMap.keys (foldr EnumMap.delete (_map o) present))

-- | A permute function that verifies neworder is a valid permutation.
permute :: forall k v. (Enum k, Ord k) => Vector k -> Order k v -> Either String (Order k v)
permute neworder o =
    case ListLike.partition (`EnumMap.member` _map o) neworder of
      (_, b) | Foldable.length b /= 0 -> Left "invalid keys"
      (a, _) | Foldable.length a < Foldable.length (_vec o) -> Left "missing keys"
      (a, _) | ListLike.nub a /= a -> Left "duplicate keys"
      _ -> Right (unsafeOrder (_map o) neworder)

sortBy :: forall k v. (Enum k, Ord k) => ((k, v) -> (k, v) -> Ordering) -> Order k v -> Order k v
sortBy cmp o = unsafeOrder (_map o) (Vector.fromList $ List.sortBy cmp' $ Vector.toList $ _vec o)
  where
    cmp' :: k -> k -> Ordering
    cmp' k1 k2 = cmp (k1, _map o ! k1) (k2, _map o ! k2)

-- Quickcheck

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

#if 0
prop_splitAt :: (k ~ Char, a ~ String) => ElementPosition k a -> Bool
prop_splitAt (ElementPosition o i) =
    let (a, b) = ListLike.splitAt (maybe 0 id i) o in
    o == a <> b

prop_next :: Order Char String -> Bool
prop_next o = isNothing (ListLike.elemIndex (next o) (keys o))

prop_uncons :: Order Char Int -> Bool
prop_uncons o =
   o == maybe mempty (\(pair, o') -> ListLike.singleton pair <> o') (ListLike.uncons o)

prop_null :: Order Char Int -> Bool
prop_null o = null o == isNothing (ListLike.uncons o)

prop_singleton :: (Char, Int) -> Bool
prop_singleton pair = ListLike.uncons (ListLike.singleton pair) == Just (pair, mempty :: Order Char Int)
#endif

-- | Map and list should contain the same keys with no duplicates
prop_toPairs_fromPairs :: Order Int String -> Bool
prop_toPairs_fromPairs o =
    fromPairs (toPairs o :: Vector (Int, String)) == o

prop_delete :: Order Int String -> Property
prop_delete o | Foldable.length o == 0 = property True
prop_delete o =
    forAll (choose (0, Foldable.length o - 1)) $ \i ->
    Foldable.length (deleteAt i o) == Foldable.length o - 1

prop_insertAt :: (Int, String) -> Order Int String -> Property
prop_insertAt v@(k, _) o =
    forAll (choose (0, Foldable.length o)) $ \i ->
    Data.Order.member k o || (Foldable.length (insertPairAt i v o) == Foldable.length o + 1)

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
    [quickCheckResult prop_keys,
     quickCheckResult prop_lookup,
     quickCheckResult prop_lookupKey,
     quickCheckResult prop_lookupPair,
     -- quickCheckResult prop_splitAt,
     -- quickCheckResult prop_next,
     -- quickCheckResult prop_uncons,
     -- quickCheckResult prop_null,
     -- quickCheckResult prop_singleton,
     quickCheckResult prop_toPairs_fromPairs,
     quickCheckResult prop_delete,
     quickCheckResult prop_insertAt,
     quickCheckResult prop_insert_delete,
     quickCheckResult prop_insert_delete_pos,
     quickCheckResult prop_fromPairs
    ] >>= throwResult
