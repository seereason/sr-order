{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS -Wall #-}

-- | A combination of a Map and a Vector - that is, each element of the
-- Map has a unique sequence number between zero and the number of
-- elements in the map, and this sequence can be modified.  There is a
-- ListLike instance that treats it as a sequence of (k, a) pairs.

module Data.Order
    ( Order(Order)
    -- * Lenses
    , map, vec
    -- * Sequence access
    , keys, values
    -- * Map over keys
    , mapKeys
    -- * Key operations
    , member, delete
    -- * Positional operations
    , Data.Order.insertAt
    , Data.Order.deleteAt
    , Data.Order.splitAt
    , append, prepend
    -- * QuickCheck property
    , prop_same_keys
    ) where

import Control.Lens
import Data.Data (Data)
import qualified Data.Foldable as Foldable
import Data.Foldable as Foldable hiding (toList)
import qualified Data.ListLike as LL
import Data.ListLike as LL (ListLike, FoldableLL, fromListLike, nub, sort, zip)
import qualified Data.Map.Strict as Map
import Data.Map.Strict as Map ((!), Map, mapWithKey)
import Data.Monoid
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
import Prelude hiding (foldMap, length, map, splitAt, zip)
import Test.QuickCheck (Arbitrary(arbitrary), shuffle, sized, vector, vectorOf)

data Order_0 k v =
    Order_0 (Map k v) [k] k
    deriving (Data, Typeable, Generic, Functor)

-- type L a = [a]
type L a = Seq a
-- type L a = Vector a

data Order k v =
  Order
    { _map :: Map k v
    , _vec :: L k
    } deriving (Data, Typeable, Generic, Functor, Read)

instance (SafeCopy k, SafeCopy v, Ord k, Enum k) => Migrate (Order k v) where
  type MigrateFrom (Order k v) = Order_0 k v
  migrate (Order_0 mp ks _) = Order mp (fromListLike ks)

instance (Ord k, Show k, Show v, Typeable k, Typeable v) => Show (Order k v) where
    show o = "fromListLike " ++ show (LL.fromListLike o :: [(k, v)]) ++ " :: Order (" ++ show (typeRep (Proxy :: Proxy k)) ++ ") (" ++ show (typeRep (Proxy :: Proxy v)) ++ ")"

$(makeLenses ''Order)

splitAt :: Ord k => Int -> Order k a -> (Order k a, Order k a)
splitAt n = over _1 LL.fromListLike . over _2 LL.fromListLike . Vector.splitAt n . LL.fromListLike

insertAt :: Ord k => Int -> (k, a) -> Order k a -> Order k a
insertAt n (k, a) = uncurry (<>) . over _2 (prepend (k, a)) . splitAt n

deleteAt :: Ord k => Int -> Order k a -> Order k a
deleteAt n = uncurry (<>) . over _2 (LL.drop 1) . LL.splitAt n

append :: Ord k => (k, a) -> Order k a -> Order k a
append (k, a) m = insertAt (length m) (k, a) m

prepend :: Ord k => (k, a) -> Order k a -> Order k a
prepend (k, a) (Order m v) = Order (Map.insert k a m) (LL.cons k v)

keys :: Order k a -> L k
keys = _vec

values :: Ord k => Order k a -> L a
values x = fmap (\k -> _map x ! k) (keys x)

prop_same_keys :: Order Int String -> Bool
prop_same_keys o = sort (LL.fromListLike (_vec o) :: [Int]) == sort (nub (Map.keys (_map o) :: [Int]))

-- | Based on Data.Map.mapKeys
mapKeys :: Ord k2 => (k1 -> k2) -> Order k1 a -> Order k2 a
mapKeys f (Order m v) = Order (Map.mapKeys f m) (fmap f v)

member :: Ord k => k -> Order k v -> Bool
member k o = Map.member k (_map o)

-- | Based on Data.Map.delete
delete :: Ord k => k -> Order k a -> Order k a
delete k (Order m v) =
    if Map.member k m
    then Order (Map.delete k m) (Sequence.filter (/= k) v)
    else Order m v

instance (Ord k, Eq v) => Eq (Order k v) where
  a == b = _map a == _map b && _vec a == _vec b

instance (Ord k, Eq v, Ord v) => Ord (Order k v) where
    compare a b = compare (_vec a) (_vec b) <> compare (_map a) (_map b)

instance Ord k => Monoid (Order k v) where
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
instance Ord k => Traversable (Order k) where
    traverse f (Order m v) = Order <$> traverse f m <*> pure v

instance Ord k => TraversableWithIndex k (Order k) where
    itraverse f (Order m v) = Order <$> itraverse f m <*> pure v

-- Fold over the values only
-- @@
-- λ> foldMap id (fromList [(1, "a"), (2, "b")])
-- "ab"
-- @@
instance Ord k => Foldable (Order k) where
    foldMap f (Order m v) = foldMap (\k -> f (m ! k)) v

-- Fold over keys and values
-- @@
-- λ> ifoldMap (\k v-> show k ++ v) (fromList [(2, "a"), (5, "b")])
-- "2a5b"
-- @@
instance Ord k => FoldableWithIndex k (Order k) where
    ifoldMap f (Order m v) = foldMap (\k -> f k (m ! k)) v

-- @@
-- λ> ifoldMap (\k v-> LL.concat (LL.replicate k v :: [String])) (fromPairs (LL.fromList [(2, "a"), (5, "b")]))
-- "aabbbbb"
-- @@
instance FunctorWithIndex k (Order k) where
    imap f (Order m v) = Order (Map.mapWithKey f m) v

instance (Ord k, Serialize k, Serialize v) => Serialize (Order k v) where
    put o = put (_map o, _vec o)
    get = do (m, v) <- get; return $ Order m v

instance (Ord k, Arbitrary k, Arbitrary v) => Arbitrary (Order k v) where
  arbitrary = do
      (ks :: [k]) <- (sized pure >>= \n -> vectorOf n arbitrary) >>= shuffle
      let ks' = nub ks
      vs <- vector (length ks')
      return (LL.fromList (zip ks' vs))

instance (Ord k, Monoid (Order k v)) => LL.ListLike (Order k v) (k, v) where
    uncons m =
        case LL.uncons (keys m) of
          Nothing -> Nothing
          Just (k, ks) -> Just ((k, _map m ! k), Order {_map = Map.delete k (_map m), _vec = ks})
    null = Map.null . _map
    singleton (k, a) = Order {_map = Map.singleton k a, _vec = singleton k}
{-
    head m = case uncons (order m) of
               Just (hd, _) -> elems m ! hd
               _ -> error "OrderMap.head"
    tail m = case uncons (order m) of
               Just (hd, tl) -> m {order = tl, elems = Map.delete hd (elems m), next = next m}
               _ -> error "OrderMap.tail"
-}

instance (Ord k, Monoid (Order k v)) => LL.FoldableLL (Order k v) (k, v) where
    foldl f r0 xs = Foldable.foldl (\r k -> f r (k, _map xs ! k)) r0 (_vec xs)
    foldr f r0 xs = Foldable.foldr (\k r -> f (k, _map xs ! k) r) r0 (_vec xs)

#if 0
-- Could not deduce (Ord k) arising from a use of ‘getSafeGet’
$(deriveSafeCopy 0 'base ''Order_0)
-- Could not deduce (Ord k) arising from a use of ‘extension’
$(deriveSafeCopy 1 'extension ''Order)
#else
instance (Ord k, Enum k, SafeCopy k, SafeCopy a) => SafeCopy (Order_0 k a) where
    putCopy (Order_0 elems_ order_ next_) =
        contain $ do safePut elems_
                     safePut order_
                     safePut next_
    getCopy =
        contain $ do elems_ <- safeGet
                     order_ <- safeGet
                     next_ <- safeGet
                     return $ Order_0 elems_ order_ next_
    version = 0
    kind = base
    errorTypeName _ = "Order_0"

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

$(deriveLiftMany [''Order_0, ''Order])
