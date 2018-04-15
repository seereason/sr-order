{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS -Wall #-}

-- | A combination of a Map and a Sequence - that is, each element of the
-- Map has a unique sequence number between zero and the number of
-- elements in the map, and this sequence can be modified.  There is a
-- ListLike instance that treats it as a sequence of (k, a) pairs.

module Data.Order
    ( Order(Order)
    , Order_0(Order_0)
    -- * Lenses
    , map, vec
    -- * Operators
    , keys, values, pos, lookup, ilookup, view
    , mapKeys
    , member, delete, permute
    -- * Positional operations
    , Data.Order.insertAt
    , Data.Order.deleteAt
    , Data.Order.splitAt
    , append, prepend
    -- * Allocate new keys
    -- * QuickCheck property
    , prop_same_keys
    ) where

import Control.Lens hiding (view)
import Data.Data (Data)
import qualified Data.Foldable as Foldable
import Data.Foldable as Foldable hiding (toList)
import qualified Data.ListLike as LL
import Data.ListLike as LL (filter, ListLike, FoldableLL, fromListLike, nub, sort, zip)
import Data.Map.Strict as Map (Map)
import qualified Data.Map.Strict as Map
import Data.EnumMap as EnumMap ((!), EnumMap(..), fromList, mapWithKey)
import qualified Data.EnumMap as EnumMap
import Data.Monoid
import Data.Order_0
import Data.SafeCopy (base, contain, deriveSafeCopy, extension, Migrate(..), SafeCopy(..), safeGet, safePut)
import Data.Sequence hiding (fromList, length, splitAt, sort, zip)
import qualified Data.Sequence as Sequence
import Data.Serialize (Serialize(get, put))
-- import Data.Traversable as Traversable
import Data.Typeable (Proxy(Proxy), Typeable, typeRep)
import qualified Data.Vector as Vector
import GHC.Generics
import Instances.TH.Lift ()
import Language.Haskell.TH.Lift (deriveLiftMany)
import Prelude hiding (foldMap, length, lookup, map, splitAt, zip)
import Test.QuickCheck (Arbitrary(arbitrary), shuffle, sized, vector, vectorOf)

-- type L a = [a]
type L a = Seq a
-- type L a = Vector a

data Order k v =
  Order
    { _map :: EnumMap k v
    , _vec :: L k
    } deriving (Data, Typeable, Generic, Functor, Read)

instance (SafeCopy k, SafeCopy v, Ord k, Enum k) => Migrate (Order k v) where
  type MigrateFrom (Order k v) = Order_0 k v
  migrate (Order_0 mp ks _) = Order (EnumMap.fromList (Map.toList mp)) (fromListLike ks)

instance (Ord k, Enum k, Show k, Show v, Typeable k, Typeable v) => Show (Order k v) where
    show o = "fromListLike " ++ show (LL.fromListLike o :: [(k, v)]) ++ " :: Order (" ++ show (typeRep (Proxy :: Proxy k)) ++ ") (" ++ show (typeRep (Proxy :: Proxy v)) ++ ")"

$(makeLenses ''Order)

splitAt :: (Enum k, Ord k) => Int -> Order k a -> (Order k a, Order k a)
splitAt n = over _1 LL.fromListLike . over _2 LL.fromListLike . Vector.splitAt n . LL.fromListLike

insertAt :: (Enum k, Ord k) => Int -> (k, a) -> Order k a -> Order k a
insertAt n (k, a) = uncurry (<>) . over _2 (prepend (k, a)) . splitAt n

deleteAt :: (Enum k, Ord k) => Int -> Order k a -> Order k a
deleteAt n = uncurry (<>) . over _2 (LL.drop 1) . LL.splitAt n

append :: (Enum k, Ord k) => (k, a) -> Order k a -> Order k a
append (k, a) m = insertAt (length m) (k, a) m

prepend :: (Enum k, Ord k) => (k, a) -> Order k a -> Order k a
prepend (k, a) (Order m v) = Order (EnumMap.insert k a m) (LL.cons k v)

-- | Return the keys in order.
keys :: Order k a -> L k
keys = _vec

-- | Return the position of a key, Nothing if absent.
pos :: Eq k => k -> Order k a -> Maybe Int
pos k (Order _ v) =
    case LL.break (== k) v of
      (_, x) | LL.null x -> Nothing
      (x, _) -> Just (LL.length x)

-- | Return the values in order.
values :: (Enum k, Ord k) => Order k a -> L a
values x = fmap (\k -> _map x ! k) (keys x)

prop_same_keys :: Order Int String -> Bool
prop_same_keys o = sort (LL.fromListLike (_vec o) :: [Int]) == sort (nub (EnumMap.keys (_map o) :: [Int]))

-- | Based on Data.Map.mapKeys
mapKeys :: (Enum k1, Enum k2, Ord k2) => (k1 -> k2) -> Order k1 a -> Order k2 a
mapKeys f (Order m v) = Order (EnumMap.mapKeys f m) (fmap f v)

-- | Like Map.lookup.
lookup :: (Enum k, Ord k) => k -> Order k a -> Maybe a
lookup k (Order m _) = EnumMap.lookup k m

ilookup :: (Enum k, Ord k) => k -> Order k a -> Maybe (Int, a)
ilookup k (Order m v) =
    case EnumMap.lookup k m of
      Nothing -> Nothing
      Just a -> Just (LL.length (fst (LL.break (== k) v)), a)

-- | Like 'Data.Set.minView', if k is present returns the position,
-- associated value, and the order with that value removed.
view :: (Enum k, Ord k) => k -> Order k a -> Maybe (Int, a, Order k a)
view k o =
  case ilookup k o of
    Nothing -> Nothing
    Just (i, a) -> Just (i, a, delete k o)

member :: (Enum k, Ord k) => k -> Order k v -> Bool
member k o = EnumMap.member k (_map o)

-- | Based on Data.EnumMap.delete
delete :: (Enum k, Ord k) => k -> Order k a -> Order k a
delete k (Order m v) =
    if EnumMap.member k m
    then Order (EnumMap.delete k m) (Sequence.filter (/= k) v)
    else Order m v

instance (Ord k, Eq v) => Eq (Order k v) where
  a == b = _map a == _map b && _vec a == _vec b

instance (Enum k, Ord k, Eq v, Ord v) => Ord (Order k v) where
    compare a b = compare (_vec a) (_vec b) <> compare (_map a) (_map b)

instance (Enum k, Ord k) => Monoid (Order k v) where
    mempty = Order mempty mempty
    mappend a b = Order (mappend (_map a) (_map b)) (_vec a <> _vec b)
    -- ^ If there are any common @k@ values in the shared
    -- map the elements from the second is omitted.  For
    -- this reason it is suggested that, when in doubt,
    -- the @k@ type be mapped to @Either k k@:
    -- @@
    --   mapKeys Left a <> mapKeys Right b
    -- @@

-- @@
-- λ> traverse (++ "!") (fromList
-- @@
instance (Enum k, Ord k) => Traversable (Order k) where
    traverse f (Order m v) = Order <$> traverse f m <*> pure v

instance (Enum k, Ord k) => TraversableWithIndex k (Order k) where
    itraverse f (Order m v) = Order <$> itraverse f m <*> pure v

-- Fold over the values only
-- @@
-- λ> foldMap id (fromList [(1, "a"), (2, "b")])
-- "ab"
-- @@
instance (Enum k, Ord k) => Foldable (Order k) where
    foldMap f (Order m v) = foldMap (\k -> f (m ! k)) v

-- Fold over keys and values
-- @@
-- λ> ifoldMap (\k v-> show k ++ v) (fromList [(2, "a"), (5, "b")])
-- "2a5b"
-- @@
instance (Enum k, Ord k) => FoldableWithIndex k (Order k) where
    ifoldMap f (Order m v) = foldMap (\k -> f k (m ! k)) v

-- @@
-- λ> ifoldMap (\k v-> LL.concat (LL.replicate k v :: [String])) (fromPairs (LL.fromList [(2, "a"), (5, "b")]))
-- "aabbbbb"
-- @@
instance Enum k => FunctorWithIndex k (Order k) where
    imap f (Order m v) = Order (EnumMap.mapWithKey f m) v

type instance Index (Order k a) = k
type instance IxValue (Order k a) = a
instance (Enum k, Ord k) => Ixed (Order k a) where
    ix i f o@(Order m v) =
        case EnumMap.lookup i m of
          Just a -> f a <&> \a' -> Order (EnumMap.insert i a' m) v
          Nothing -> pure o

instance (Enum k, Ord k, Serialize k, Serialize v) => Serialize (Order k v) where
    put o = put (_map o, _vec o)
    get = do (m, v) <- get; return $ Order m v

instance (Enum k, Ord k, {-Show k,-} Arbitrary k, Arbitrary v) => Arbitrary (Order k v) where
  arbitrary = do
      (ks :: [k]) <- (sized pure >>= \n -> vectorOf n arbitrary) >>= shuffle
      let ks' = nub ks
      vs <- vector (length ks')
      return (LL.fromList (zip ks' vs))

instance (Enum k, Ord k, Monoid (Order k v)) => LL.ListLike (Order k v) (k, v) where
    uncons m =
        case LL.uncons (keys m) of
          Nothing -> Nothing
          Just (k, ks) -> Just ((k, _map m ! k), Order {_map = EnumMap.delete k (_map m), _vec = ks})
    null = EnumMap.null . _map
    singleton (k, a) = Order {_map = EnumMap.singleton k a, _vec = singleton k}
{-
    head m = case uncons (order m) of
               Just (hd, _) -> elems m ! hd
               _ -> error "OrderMap.head"
    tail m = case uncons (order m) of
               Just (hd, tl) -> m {order = tl, elems = EnumMap.delete hd (elems m), next = next m}
               _ -> error "OrderMap.tail"
-}

instance (Enum k, Ord k, Monoid (Order k v)) => LL.FoldableLL (Order k v) (k, v) where
    foldl f r0 xs = Foldable.foldl (\r k -> f r (k, _map xs ! k)) r0 (_vec xs)
    foldr f r0 xs = Foldable.foldr (\k r -> f (k, _map xs ! k) r) r0 (_vec xs)

#if 0
-- Could not deduce (Ord k) arising from a use of ‘extension’
$(deriveSafeCopy 1 'extension ''Order)
#else
instance (Ord k, Enum k, SafeCopy k, SafeCopy a) => SafeCopy (Order k a) where
    putCopy (Order m v) =
        contain $ do safePut m
                     safePut v
    getCopy =
        contain $ do m <- safeGet
                     v <- safeGet
                     return $ Order m v
    version = 1
    kind = extension
    errorTypeName _ = "Order"
#endif

$(deriveLiftMany [''Order])


-- | Replace the current ordering with the given key list.  Duplicate
-- keys are ignored, missing keys are appended.
permute :: forall k v. (Enum k, Ord k) => L k -> Order k v -> Order k v
permute neworder (Order m _v) =
    Order m (present <> missing)
    where
      -- Remove unknown keys
      present :: L k
      present = LL.nub (LL.filter (`EnumMap.member` m) neworder)
      -- Collect the values that are missing from the new key order
      missing :: L k
      missing = LL.fromList (EnumMap.keys (LL.foldr EnumMap.delete m present))

deriving instance Serialize v => Serialize (EnumMap k v)
$(deriveLiftMany [''EnumMap])
$(deriveSafeCopy 1 'base ''EnumMap)
