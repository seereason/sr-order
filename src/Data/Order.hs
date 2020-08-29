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
    , toPairList
    -- * Builder
    , fromPairs
    -- * Operators
    , keys, values, pos, Data.Order.lookup, ilookup, view
    , Data.Order.next, Data.Order.mapKeys
    , Data.Order.member, permute, permuteUnsafe
    -- * Positional operations
    , lookupKey, lookupPair
    , insertAt, insertPairAt
    , deleteAt
    , appendPair, appendElem
    , prependPair, prependElem
    -- * Is/Has classes and instances
    , IsMap(..)
    , HasMap(..)
    , IsOrder(..)
    , HasOrder(..)

    , tests
    ) where

import Control.Lens hiding (uncons, view)
import qualified Data.Foldable as Foldable
import Data.Foldable as Foldable hiding (toList)
import Data.ListLike (break, singleton, uncons)
import qualified Data.ListLike as LL
import Data.EnumMap as EnumMap ((!), keysSet, mapKeys, mapWithKey, member)
import qualified Data.EnumMap as EnumMap
import Data.List as List (nub)
import Data.Map (Map)
import qualified Data.Map as Map (fromList{-, toList-})
import Data.Maybe (isNothing)
import Data.Monoid
import Data.Order.Type (Order(..), unsafeOrder)
import Data.SafeCopy (SafeCopy(..), safeGet, safePut)
import Data.Serialize (Serialize(..))
import qualified Data.Semigroup as Sem
import qualified Data.Set as Set (fromList)
import Data.Typeable (Proxy(Proxy), Typeable, typeRep)
import Data.UList (UList, umap)
import Extra.QuickCheck
import GHC.Exts
import Prelude hiding (break, foldMap, length, lookup, map, splitAt, zip)
import Test.QuickCheck
import Text.PrettyPrint.HughesPJClass (Pretty(pPrint), text)

instance (Ord k, Enum k) => IsList (Order k v) where
  type Item (Order k v) = (k, v)
  fromList = fromPairs
  toList = GHC.Exts.toList . toPairs

instance SafeCopy (Order k v) => Serialize (Order k v) where
    put = safePut
    get = safeGet

instance forall k v. (Ord k, Enum k, Show k, Show v, Typeable k, Typeable v) => Show (Order k v) where
  show o = "fromPairs " ++ show (toList o) ++ " :: Order (" ++ show (typeRep (Proxy :: Proxy k)) ++ ") (" ++ show (typeRep (Proxy :: Proxy v)) ++ ")"

instance forall k v. (Ord k, Enum k, Pretty k, Pretty v, Typeable k, Typeable v) => Pretty (Order k v) where
  pPrint o = text "Order " <> pPrint (toList o)

instance (Enum k, Ord k) => Sem.Semigroup (Order k v) where
    (<>) a b = unsafeOrder (mappend (_map a) (_map b)) (_vec a <> _vec b)
    -- ^ If there are any common @k@ values in the shared
    -- map the elements from the second is omitted.  For
    -- this reason it is suggested that, when in doubt,
    -- the @k@ type be mapped to @Either k k@:
    -- @@
    --   mapKeys Left a <> mapKeys Right b
    -- @@

instance (Enum k, Ord k) => Monoid (Order k v) where
    mempty = unsafeOrder mempty mempty
#if !(MIN_VERSION_base(4,11,0))
    mappend = (<>)
#endif

-- Fold over the values only
-- @@
-- λ> foldMap id (fromList [(1, "a"), (2, "b")])
-- "ab"
-- @@
instance (Enum k, Ord k) => Foldable (Order k) where
    foldMap f o = foldMap (\k -> f (_map o ! k)) (_vec o)

-- Fold over keys and values
-- @@
-- λ> ifoldMap (\k v-> show k ++ v) (fromList [(2, "a"), (5, "b")])
-- "2a5b"
-- @@
instance (Enum k, Ord k) => FoldableWithIndex k (Order k) where
    ifoldMap f o = foldMap (\k -> f k (_map o ! k)) (_vec o)

-- @@
-- λ> ifoldMap (\k v-> LL.concat (LL.replicate k v :: [String])) (fromPairs (LL.fromList [(2, "a"), (5, "b")]))
-- "aabbbbb"
-- @@
instance Enum k => FunctorWithIndex k (Order k) where
    imap f o = unsafeOrder (EnumMap.mapWithKey f (_map o)) (_vec o)

-- | Can be fully constructed from a Map
class (Ixed o, Eq (Index o)) => IsMap o where
    fromMap :: Map (Index o) (IxValue o) -> o

-- | Contains at least as much information as a Map
class HasMap o where
    toMap :: o -> Map (Index o) (IxValue o)

-- | Can be fully constructed from an Order
class IsOrder o where
    fromOrder :: Order (Index o) (IxValue o) -> o

-- | Contains at least as much information as an Order
class (Eq (Index o), Enum (Index o)) => HasOrder o where
    toOrder :: o -> Order (Index o) (IxValue o)
    toPairs :: o -> [(Index o, IxValue o)]
    toPairs = toPairs' . toOrder

toPairList :: (Ord k, Enum k) => Order k v -> [(k, v)]
toPairList = LL.toList

-- pos :: HasOrder o => o -> Index o -> Maybe Int
-- pos = Order.pos . toOrder

-- Order k v instances
instance IsOrder  (Order k v) where fromOrder = id
instance (Ord k, Enum k) => HasOrder (Order k v) where toOrder = id
instance (Ord k, Enum k) => HasMap   (Order k v) where toMap = Map.fromList . GHC.Exts.toList . toPairs

-- Map k v instances
instance Ord k =>           IsMap   (Map k v) where fromMap = id
instance (Ord k, Enum k) => IsOrder (Map k v) where fromOrder = Map.fromList . GHC.Exts.toList . toPairs
instance                    HasMap  (Map k v) where toMap = id

-- We can't write instances for [(k, v)] due to existing Ixed [a]
-- instance.  We could create a wrapper, there's some work to convert
-- integer index to key.
{-
newtype AList k v = AList {unAList :: [(k, v)]}
instance At (AList k v) where at k = lens unAList AList . (maybe _Nothing at (findIndex undefined :: Lens' [(k, v)] (Maybe v))  {-lens AList unAList . (at k :: Lens [(k, v)] (Maybe (k, v)))-}
instance Ixed (AList k v) where ix k = lens AList unAList . ix k
type instance Index (AList k v) = k
type instance IxValue (AList k v) = v
instance (Ord k, Enum k) => IsMap (AList k v) where fromMap = AList . Map.toList
instance (Ord k, Enum k) => HasMap (AList k v) where toMap = Map.fromList . unAList
instance (Ord k, Enum k) => IsOrder (AList k v) where fromOrder = AList . GHC.Exts.toList . toPairs
instance (Ord k, Enum k) => HasOrder (AList k v) where toOrder = fromPairs . unAList
-}

fromPairs :: forall t k a. (Eq k, Enum k, Foldable t) => t (k, a) -> Order k a
fromPairs pairs =
    unsafeOrder
      (fromList (fmap (over _1 fromEnum) pairs'))
      (fromList (fmap fst pairs'))
    where
      pairs' :: [(k, a)]
      pairs' = Foldable.toList pairs

toPairs' :: (Eq k, Enum k) => Order k a -> [(k, a)]
toPairs' o = fmap (\k -> (k, _map o ! k)) (toList (_vec o))

prop_fromPairs :: Order Char String -> Bool
prop_fromPairs o = fromPairs (toPairs o) == o

-- | Lookup key by position.  A lookup function appears in
-- containers in version 0.5.8.
lookupKey :: Eq k => Int -> Order k a -> Maybe k
lookupKey i o | i < 0 || i >= length (keys o) = Nothing
lookupKey i o = Just (LL.index (keys o) i)

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
insertPairAt n (k, a) o = uncurry (<>) $ over _2 (LL.singleton (k, a) <>) $ LL.splitAt n o

deleteAt :: (Enum k, Ord k) => Int -> Order k a -> Order k a
deleteAt n = uncurry (<>) . over _2 (LL.drop 1) . LL.splitAt n

appendPair :: (Enum k, Ord k) => Order k a -> (k, a) -> Order k a
appendPair m (k, a) = m <> LL.singleton (k, a)

appendElem :: (Enum k, Ord k) => Order k a -> a -> (Order k a, k)
appendElem o a = let k = next o in (appendPair o (k, a), k)

prependPair :: (Enum k, Ord k) => (k, a) -> Order k a -> Order k a
prependPair (k, a) o = LL.singleton (k, a) <> o

prependElem :: (Enum k, Ord k) => a -> Order k a -> (Order k a, k)
prependElem a o = let k = next o in (prependPair (k, a) o, k)

-- | Return the keys in order.
keys :: Order k a -> UList k
keys = _vec

-- | Return the position of a key, Nothing if absent.
pos :: Eq k => k -> Order k a -> Maybe Int
pos k o =
    case LL.break (== k) (_vec o) of
      (_, x) | LL.null x -> Nothing
      (x, _) -> Just (LL.length x)

-- | Return the values in order.
values :: (Eq k, Enum k) => Order k a -> [a]
values x = fmap (\k -> _map x ! k) (toList (keys x))

-- | Based on Data.Map.mapKeys, because UList is not a functor we
-- need to eliminate pairs when two k1 are mapped to the same k2.
mapKeys :: (Enum k1, Enum k2, Eq k2) => (k1 -> k2) -> Order k1 a -> Order k2 a
mapKeys f o = unsafeOrder (EnumMap.mapKeys f (_map o)) (umap f (_vec o))

ilookup :: (Enum k, Ord k) => k -> Order k a -> Maybe (Int, a)
ilookup k o =
    case EnumMap.lookup k (_map o) of
      Nothing -> Nothing
      Just a -> Just (length (fst (break (== k) (_vec o))), a)

-- | Like 'Data.Set.minView', if k is present returns the position,
-- associated value, and the order with that value removed.
view :: (Enum k, Ord k) => k -> Order k a -> Maybe (Int, a, Order k a)
view k o =
  case ilookup k o of
    Just (i, a) -> Just (i, a, unsafeOrder (EnumMap.delete k (_map o)) (LL.filter (/= k) (_vec o)))
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
              fmap (maybe (unsafeOrder (EnumMap.delete k (_map o)) (LL.filter (/= k) (_vec o)))
                          (\a' -> unsafeOrder (EnumMap.insert k a' (_map o)) (_vec o)))
                   (f (Just a))
          Nothing ->
              fmap (maybe o
                          (\a' -> unsafeOrder (EnumMap.insert k a' (_map o)) (_vec o <> singleton k)))
                   (f Nothing)

instance (Ord k, Enum k, LL.FoldableLL (Order k v) (k, v)) => LL.ListLike (Order k v) (k, v) where
  singleton :: (k, v) -> Order k v
  singleton (k, v) = unsafeOrder (EnumMap.singleton k v) [k]
  null :: Order k v -> Bool
  null o = LL.null (_vec o)
  uncons :: Order k v -> Maybe ((k, v), Order k v)
  uncons o =
    case LL.uncons (_vec o) of
      Just (k0, ks') ->
        let (mk, mks) = EnumMap.partitionWithKey (\k _ -> k == k0) (_map o) in
          Just (LL.head (EnumMap.toList @k mk), unsafeOrder mks ks')
      Nothing -> Nothing

  drop :: Int -> Order k a -> Order k a
  drop n o =
    let (a, b) = LL.splitAt n (_vec o) in
    unsafeOrder (Foldable.foldr EnumMap.delete (_map o) a) b

  take :: Int -> Order k a -> Order k a
  take n o =
    let (a, b) = LL.splitAt n (_vec o) in
    unsafeOrder (Foldable.foldr EnumMap.delete (_map o) b) a

  splitAt :: Int -> Order k a -> (Order k a, Order k a)
  splitAt n = over _1 fromPairs . over _2 fromPairs . LL.splitAt n . toPairs

instance (Enum k, Ord k, Monoid (Order k v)) => LL.FoldableLL (Order k v) (k, v) where
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
permuteUnsafe :: forall k v. (Enum k, Ord k) => UList k -> Order k v -> Order k v
permuteUnsafe neworder o =
    unsafeOrder (_map o) (present <> missing)
    where
      -- Remove unknown keys
      present :: UList k
      present = fromList (List.nub (toList (LL.filter (`EnumMap.member` _map o) neworder)))
      -- Collect the values that are missing from the new key order
      missing :: UList k
      missing = fromList (EnumMap.keys (foldr EnumMap.delete (_map o) present))

-- | A permute function that verifies neworder is a valid permutation.
permute :: forall k v. (Enum k, Ord k) => UList k -> Order k v -> Either String (Order k v)
permute neworder o =
    case LL.partition (`EnumMap.member` _map o) neworder of
      (_, b) | length b /= 0 -> Left "invalid keys"
      (a, _) | length a < length (_vec o) -> Left "missing keys"
      (a, _) | LL.nub a /= a -> Left "duplicate keys"
      _ -> Right (unsafeOrder (_map o) neworder)

instance (Ord k, Enum k, Arbitrary k, Arbitrary v) => Arbitrary (InsertPosition k v) where
  arbitrary = do
      o <- arbitrary :: Gen (Order k v)
      InsertPosition o <$> choose (0, length o)

instance (Ord k, Enum k, Arbitrary k, Arbitrary v) => Arbitrary (ElementPosition k v) where
  arbitrary = do
      o <- arbitrary :: Gen (Order k v)
      case length o of
        0 -> return $ ElementPosition o Nothing
        n -> ElementPosition o <$> (Just <$> choose (0, pred n))

instance (Enum k, Ord k, {-Show k,-} Arbitrary k, Arbitrary v) => Arbitrary (Order k v) where
  arbitrary = do
      (ks :: [k]) <- (sized pure >>= \n -> vectorOf n arbitrary) >>= shuffle
      let ks' = nub ks
      (vs :: [v]) <- vector (LL.length ks')
      return (fromPairs (LL.zip ks' vs :: [(k, v)]) :: Order k v)

prop_lookupKey :: (k ~ Char, a ~ String) => InsertPosition k a -> a -> Bool
prop_lookupKey (InsertPosition o i) a =
    lookupKey i o' == Just k && lookupKey (length o) o == Nothing
    where o' = insertPairAt i (k, a) o
          k = next o

prop_lookup :: (k ~ Char, a ~ String) => InsertPosition k a -> a -> Bool
prop_lookup (InsertPosition o i) a =
    lookup k o' == Just a && lookup k o == Nothing
    where o' = insertPairAt i (k, a) o
          k = next o

prop_lookupPair :: (k ~ Char, a ~ String) => InsertPosition k a -> a -> Bool
prop_lookupPair (InsertPosition o i) a =
    lookupPair i o' == Just (k, a) && lookupPair (length o) o == Nothing
    where o' = insertPairAt i (k, a) o
          k = next o

prop_splitAt :: (k ~ Char, a ~ String) => ElementPosition k a -> Bool
prop_splitAt (ElementPosition o i) =
    let (a, b) = LL.splitAt (maybe 0 id i) o in
    o == a <> b

prop_keys :: Order Char String -> Bool
prop_keys o = EnumMap.keysSet (_map o) == Set.fromList (Foldable.toList (_vec o))

prop_next :: Order Char String -> Bool
prop_next o = isNothing (LL.elemIndex (next o) (keys o))

prop_uncons :: Order Char Int -> Bool
prop_uncons o =
   o == maybe mempty (\(pair, o') -> singleton pair <> o') (uncons o)

prop_null :: Order Char Int -> Bool
prop_null o = null o == isNothing (uncons o)

prop_singleton :: (Char, Int) -> Bool
prop_singleton pair = uncons (singleton pair) == Just (pair, mempty :: Order Char Int)

-- | Map and list should contain the same keys with no duplicates
prop_toPairs_fromPairs :: Order Int String -> Bool
prop_toPairs_fromPairs o =
    fromList (toList o :: [(Int, String)]) == o

prop_delete :: Order Int String -> Property
prop_delete o | length o == 0 = property True
prop_delete o =
    forAll (choose (0, length o - 1)) $ \i ->
    length (deleteAt i o) == length o - 1

prop_insertAt :: (Int, String) -> Order Int String -> Property
prop_insertAt v@(k, _) o =
    forAll (choose (0, length o)) $ \i ->
    Data.Order.member k o || (length (insertPairAt i v o) == length o + 1)

-- | Use an explicit generator to create a valid list position.
prop_insert_delete :: (Int, String) -> Order Int String -> Property
prop_insert_delete (k, a) o =
    forAll (choose (0, length o)) $ \i ->
        Data.Order.member k o || (view k (insertPairAt i (k, a) o) == Just (i, a, o))

prop_insert_delete_pos :: (Int, String) -> Order Int String -> Property
prop_insert_delete_pos v@(k, _) o =
    forAll (choose (0, length o)) $ \i ->
        Data.Order.member k o || (deleteAt i (insertPairAt i v o) == o)

tests :: IO Result
tests = do
  mconcat <$> sequence
    [quickCheckResult prop_next,
     quickCheckResult prop_keys,
     quickCheckResult prop_next,
     quickCheckResult prop_lookup,
     quickCheckResult prop_lookupKey,
     quickCheckResult prop_lookupPair,
     quickCheckResult prop_splitAt,
     quickCheckResult prop_uncons,
     quickCheckResult prop_null,
     quickCheckResult prop_singleton,
     quickCheckResult prop_toPairs_fromPairs,
     quickCheckResult prop_delete,
     quickCheckResult prop_insertAt,
     quickCheckResult prop_insert_delete,
     quickCheckResult prop_insert_delete_pos,
     quickCheckResult prop_fromPairs
    ] >>= throwResult
