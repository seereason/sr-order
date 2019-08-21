{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ConstraintKinds #-}
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
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS -Wall -Wredundant-constraints #-}
{-# LANGUAGE UndecidableInstances #-}

-- | A combination of a Map and a Sequence - that is, each element of the
-- Map has a unique sequence number between zero and the number of
-- elements in the map, and this sequence can be modified.  There is a
-- ListLike instance that treats it as a sequence of (k, a) pairs.

module Data.Order.Order
    ( Order(Order, _map, _vec)
    , Order_1(..)
    , IsOrder(fromOrder)
    , HasOrder(toOrder, toPairs)
    , fromPairs
    , fromPairs'
    , toPairs'
    ) where

import Control.Lens hiding (view)
import Data.Data (Data)
import qualified Data.Foldable as Foldable
import Data.Foldable as Foldable hiding (toList)
import Data.EnumMap as EnumMap (toList)
import Data.List as List (nub)
import Data.Map as Map ((!), Map)
--import Data.Order.HasKey (KeyType)
import Data.Order.HasMap (HasMap(toMap))
import Data.Order.Old (Order_1(Order_1))
import qualified Data.Map as Map (delete, fromList, insert, lookup, mapWithKey)
import Data.Monoid
import Data.SafeCopy (contain, extension, Migrate(..), SafeCopy(..), SafeCopy', safeGet, safePut)
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Data.Serialize (Serialize(..))
import qualified Data.Semigroup as Sem
import Data.Typeable (Proxy(Proxy), Typeable, typeRep)
import GHC.Exts as GHC
import GHC.Generics (Generic)
import Instances.TH.Lift ()
import Language.Haskell.TH.Lift (deriveLiftMany)
import Prelude hiding (foldMap, length, lookup, map, splitAt, zip)
#if !__GHCJS__
import Test.QuickCheck (Arbitrary(arbitrary), shuffle, sized, vector, vectorOf)
#endif

data Order k v =
  Order
    { _map :: Map k v
    , _vec :: Seq k
    } deriving (Generic, Data, Typeable, Functor, Read)

#if 0
$(deriveSafeCopy 2 'extension ''Order)
#else
instance (Ord k, Enum k, SafeCopy' k, SafeCopy' v) => SafeCopy (Order k v) where
    putCopy (Order m v) =
         contain $ do safePut m
                      safePut v
    getCopy =
         contain $ do m <- safeGet
                      v <- safeGet
                      return $ Order m v
    version = 2
    kind = extension
    errorTypeName _ = "Order"
#endif

-- The only change is the HasKey constraint.
instance (Ord k, Enum k, SafeCopy' k, SafeCopy' v) => Migrate (Order k v) where
  type MigrateFrom (Order k v) = Order_1 k v
  migrate (Order_1 mp vec) = Order (Map.fromList (EnumMap.toList mp)) vec

instance (Ord k, Enum k) => IsList (Order k v) where
  type Item (Order k v) = (k, v)
  fromList = fromPairs
  toList = GHC.toList . toPairs

fromPairs :: forall t k a. (Ord k, Foldable t) => t (k, a) -> Order k a
fromPairs pairs =
    Order (Map.fromList pairs')
          (GHC.fromList (fmap fst pairs'))
    where
      pairs' :: [(k, a)]
      pairs' = Foldable.toList pairs

fromPairs' :: forall t k a. (Functor t, Ord k, Foldable t) => t (k, a) -> Order k (k, a)
fromPairs' = fromPairs . fmap (\(k, a) -> (k, (k, a)))

toPairs' :: (Ord k) => Order k a -> Seq (k, a)
toPairs' (Order m v) = fmap (\k -> (k, m ! k)) v

instance (Ord k, Enum k, SafeCopy k, Typeable k, SafeCopy v, Typeable v) => Serialize (Order k v) where
    put = safePut
    get = safeGet

instance forall k v. (Ord k, Enum k, Show k, Show v, Typeable k, Typeable v) => Show (Order k v) where
    show o = "fromPairs " ++ show (GHC.toList o) ++ " :: Order (" ++ show (typeRep (Proxy :: Proxy k)) ++ ") (" ++ show (typeRep (Proxy :: Proxy v)) ++ ")"

instance (Enum k, Ord k) => Sem.Semigroup (Order k v) where
    (<>) a b = Order (mappend (_map a) (_map b)) (_vec a <> _vec b)
    -- ^ If there are any common @k@ values in the shared
    -- map the elements from the second is omitted.  For
    -- this reason it is suggested that, when in doubt,
    -- the @k@ type be mapped to @Either k k@:
    -- @@
    --   mapKeys Left a <> mapKeys Right b
    -- @@

instance (Enum k, Ord k) => Monoid (Order k v) where
    mempty = Order mempty mempty
#if !(MIN_VERSION_base(4,11,0))
    mappend = (<>)
#endif

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
    imap f (Order m v) = Order (Map.mapWithKey f m) v

instance (Ord k, Eq v) => Eq (Order k v) where
  a == b = _map a == _map b && _vec a == _vec b

instance (Enum k, Ord k, Eq v, Ord v) => Ord (Order k v) where
    compare a b = compare (_vec a) (_vec b) <> compare (_map a) (_map b)

-- Order k v instances
instance (Ord k, Enum k) => HasMap   (Order k v) where toMap = Map.fromList . GHC.toList . toPairs

-- @@
-- λ> traverse (++ "!") (Data.Order.fromPairs [('a', "1"),('b',"2")] :: Order Char String)
-- [fromList [('a','1'),('b','2')], fromList [('a','1'),('b','!')],fromList [('a','!'),('b','2')],fromList [('a','!'),('b','!')]] :: [Order Char Char]
-- @@
instance (Enum k, Ord k) => Traversable (Order k) where
    traverse f (Order m v) = Order <$> traverse f m <*> pure v

instance (Enum k, Ord k) => TraversableWithIndex k (Order k) where
    itraverse f (Order m v) = Order <$> itraverse f m <*> pure v

-- | An Order has two notions of an 'Index', the 'Int' index of the
-- list and the @k@ index of the map.  Here is the 'Ixed' instance
-- for the latter, which only needs to address the _map field.
-- @@
-- λ> set (ix 'a') 30 (fromPairs [('a',10),('b',20)])
-- fromPairs [('a',30),('b',20)] :: Order (Char) (Integer)
-- @@

type instance Index (Order k a) = k
type instance IxValue (Order k a) = a
instance Ord k => Ixed (Order k a) where
    ix k f o@(Order mp sq) =
        case Map.lookup k mp of
          Just a -> fmap (\a' -> Order (Map.insert k a' mp) sq) (f a)
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
instance (Ord k, Eq k) => At (Order k a) where
    at k f o@(Order mp sq) =
        case Map.lookup k mp of
          Just a ->
              fmap (maybe (Order (Map.delete k mp) (Seq.filter (/= k) sq))
                          (\a' -> Order (Map.insert k a' mp) sq))
                   (f (Just a))
          Nothing ->
              fmap (maybe o
                          (\a' -> Order (Map.insert k a' mp) (sq <> Seq.singleton k)))
                   (f Nothing)

#if !__GHCJS__
instance (Enum k, Ord k, {-Show k,-} Arbitrary k, Arbitrary v) => Arbitrary (Order k v) where
  arbitrary = do
      (ks :: [k]) <- (sized pure >>= \n -> vectorOf n arbitrary) >>= shuffle
      let ks' = Seq.fromList (nub ks)
      vs <- vector (length ks')
      return (fromPairs (Seq.zip ks' (Seq.fromList vs)))
#endif

#if 0
instance (Enum k, Ord k, Monoid (Order k v)) => LL.ListLike (Order k v) (k, v) where
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

instance (Enum k, Ord k, Monoid (Order k v)) => LL.FoldableLL (Order k v) (k, v) where
    foldl f r0 xs = Foldable.foldl (\r k -> f r (k, _map xs ! k)) r0 (_vec xs)
    foldr f r0 xs = Foldable.foldr (\k r -> f (k, _map xs ! k) r) r0 (_vec xs)
#endif

-- | Can be fully constructed from an Order
class IsOrder o where
    fromOrder :: Order (Index o) (IxValue o) -> o

-- | Contains at least as much information as an Order
class (Ord (Index o), Enum (Index o)) => HasOrder o where
    toOrder :: o -> Order (Index o) (IxValue o)
    toPairs :: o -> Seq (Index o, IxValue o)
    toPairs = toPairs' . toOrder

instance IsOrder  (Order k v) where fromOrder = id
instance (Ord k, Enum k) => IsOrder (Map k v) where fromOrder = Map.fromList . GHC.toList . toPairs
instance (Ord k, Enum k) => HasOrder (Order k v) where toOrder = id

$(deriveLiftMany [''Order])
