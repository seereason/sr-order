{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS -Wall #-}

-- | A combination of a Map and a Sequence - that is, each element of the
-- Map has a unique sequence number between zero and the number of
-- elements in the map, and this sequence can be modified.  There is a
-- ListLike instance that treats it as a sequence of (k, a) pairs.

module Data.Order
    ( Order(Order, _map, _vec)
    , toPairList
    -- * Builder
    , fromPairs
    -- * Operators
    , keys, values, pos, Data.Order.lookup, ilookup, view
    , Data.Order.next, mapKeys
    , Data.Order.member, permute, permuteUnsafe
    -- * Positional operations
    , lookupKey, lookupPair
    , Data.Order.insertAt, insertAtUnsafe
    , Data.Order.deleteAt
    , splitAt, Data.Order.drop, Data.Order.take
    , append, prepend, appendUnsafe, prependUnsafe
    -- * Allocate new keys
#if 0
#if !__GHCJS__
    -- * QuickCheck property
    , tests
    , prop_next
    , prop_keys
    , prop_fromPairs
    , prop_lookup
    , prop_lookupKey
    , prop_lookupPair
    , prop_splitAt
#endif
#endif
    -- * Is/Has classes and instances
    , IsMap(..)
    , HasMap(..)
    , IsOrder(..)
    , HasOrder(..)
    ) where

import Control.Lens hiding (view)
import Data.Data (Data)
import qualified Data.Foldable as Foldable
import Data.Foldable as Foldable hiding (toList)
import qualified Data.ListLike as LL
--import Data.ListLike as LL (filter, ListLike, FoldableLL, fromListLike, nub, sort, zip)
import Data.EnumMap as EnumMap ((!), EnumMap, mapWithKey, member)
import qualified Data.EnumMap as EnumMap
import Data.List as List (nub)
import Data.Map (Map)
import qualified Data.Map as Map (fromList{-, toList-})
import Data.Monoid
import Data.SafeCopy (base, contain, SafeCopy(..), safeGet, safePut)
import Data.Sequence (Seq((:<|)))
import qualified Data.Sequence as Seq
import Data.Serialize (Serialize(..))
--import qualified Data.Set as Set
--import Data.IntSet as Set (maxView)
import qualified Data.Semigroup as Sem
import Data.Typeable (Proxy(Proxy), Typeable, typeRep)
import GHC.Exts
import GHC.Generics (Generic)
-- import qualified Data.Vector as Vector
import Instances.TH.Lift ()
import Language.Haskell.TH.Lift (deriveLiftMany)
import Prelude hiding (foldMap, length, lookup, map, splitAt, zip)
#if !__GHCJS__
import Data.EnumMap as EnumMap (keysSet)
import Data.Maybe (isNothing)
import qualified Data.Set as Set (fromList)
import Test.QuickCheck (Arbitrary(arbitrary), choose, Gen, quickCheckResult, Result, shuffle, sized, vector, vectorOf)
#endif

data Order k v =
  Order
    { _map :: EnumMap k v
    , _vec :: Seq k
    } deriving (Generic, Data, Typeable, Functor, Read)

instance (Ord k, Enum k) => IsList (Order k v) where
  type Item (Order k v) = (k, v)
  fromList = fromPairs
  toList = GHC.Exts.toList . toPairs

instance (Ord k, Enum k, SafeCopy k, Typeable k, SafeCopy v, Typeable v) => Serialize (Order k v) where
    put = safePut
    get = safeGet

#if 0
-- Could not deduce (Ord k) arising from a use of ‘extension’
$(deriveSafeCopy 1 'extension ''Order)
#else
instance (Ord k, Enum k, SafeCopy k, Typeable k, SafeCopy a, Typeable a) => SafeCopy (Order k a) where
    putCopy (Order m v) =
        contain $ do safePut m
                     safePut v
    getCopy =
        contain $ do m <- safeGet
                     v <- safeGet
                     return $ Order m v
    version = 1
    kind = base
    errorTypeName _ = "Order"
#endif

instance forall k v. (Ord k, Enum k, Show k, Show v, Typeable k, Typeable v) => Show (Order k v) where
    show o = "fromPairs " ++ show (toList o) ++ " :: Order (" ++ show (typeRep (Proxy :: Proxy k)) ++ ") (" ++ show (typeRep (Proxy :: Proxy v)) ++ ")"

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
    imap f (Order m v) = Order (EnumMap.mapWithKey f m) v

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
class (Ord (Index o), Enum (Index o)) => HasOrder o where
    toOrder :: o -> Order (Index o) (IxValue o)
    toPairs :: o -> Seq (Index o, IxValue o)
    toPairs = toPairs' . toOrder

toPairList :: (Ord k, Enum k) => Order k v -> [(k, v)]
toPairList = toList . toPairs

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
#if 0
newtype AList k v = AList {unAList :: [(k, v)]}
instance At (AList k v) where at k = lens unAList AList . (maybe _Nothing at (findIndex undefined :: Lens' [(k, v)] (Maybe v))  {-lens AList unAList . (at k :: Lens [(k, v)] (Maybe (k, v)))-}
instance Ixed (AList k v) where ix k = lens AList unAList . ix k
type instance Index (AList k v) = k
type instance IxValue (AList k v) = v
instance (Ord k, Enum k) => IsMap (AList k v) where fromMap = AList . Map.toList
instance (Ord k, Enum k) => HasMap (AList k v) where toMap = Map.fromList . unAList
instance (Ord k, Enum k) => IsOrder (AList k v) where fromOrder = AList . GHC.Exts.toList . toPairs
instance (Ord k, Enum k) => HasOrder (AList k v) where toOrder = fromPairs . unAList
#endif

fromPairs :: forall t k a. (Enum k, Foldable t) => t (k, a) -> Order k a
fromPairs pairs =
    Order (fromList (fmap (over _1 fromEnum) pairs'))
          (fromList (fmap fst pairs'))
    where
      pairs' :: [(k, a)]
      pairs' = Foldable.toList pairs

toPairs' :: (Enum k) => Order k a -> Seq (k, a)
toPairs' (Order m v) = fmap (\k -> (k, m ! k)) v

prop_fromPairs :: Order Char String -> Bool
prop_fromPairs o = fromPairs (toPairs o) == o

-- | Lookup key by position.  A lookup function appears in
-- containers in version 0.5.8.
lookupKey :: Int -> Order k a -> Maybe k
lookupKey i o | i < 0 || i >= length (keys o) = Nothing
lookupKey i o = Just (Seq.index (keys o) i)

data InsertPosition k v = InsertPosition (Order k v) Int deriving Show

#if !__GHCJS__
instance (Ord k, Enum k, Arbitrary k, Arbitrary v) => Arbitrary (InsertPosition k v) where
  arbitrary = do
      o <- arbitrary :: Gen (Order k v)
      InsertPosition o <$> choose (0, length o)
#endif

data ElementPosition k v = ElementPosition (Order k v) (Maybe Int) deriving Show

#if !__GHCJS__
instance (Ord k, Enum k, Arbitrary k, Arbitrary v) => Arbitrary (ElementPosition k v) where
  arbitrary = do
      o <- arbitrary :: Gen (Order k v)
      case length o of
        0 -> return $ ElementPosition o Nothing
        n -> ElementPosition o <$> (Just <$> choose (0, pred n))

prop_lookupKey :: (k ~ Char, a ~ String) => InsertPosition k a -> a -> Bool
prop_lookupKey (InsertPosition o i) a =
    lookupKey i o' == Just k && lookupKey (length o) o == Nothing
    where o' = insertAtUnsafe i (k, a) o
          k = next o
#endif

-- | Like Map.lookup.
lookup :: (Enum k) => k -> Order k a -> Maybe a
lookup k (Order m _) = EnumMap.lookup k m

#if !__GHCJS__
prop_lookup :: (k ~ Char, a ~ String) => InsertPosition k a -> a -> Bool
prop_lookup (InsertPosition o i) a =
    lookup k o' == Just a && lookup k o == Nothing
    where o' = insertAtUnsafe i (k, a) o
          k = next o
#endif

-- | Lookup pair by position
lookupPair :: (Enum k) => Int -> Order k a  -> Maybe (k, a)
lookupPair i o = lookupKey i o >>= (\k -> fmap (k,) (Data.Order.lookup k o))

#if !__GHCJS__
prop_lookupPair :: (k ~ Char, a ~ String) => InsertPosition k a -> a -> Bool
prop_lookupPair (InsertPosition o i) a =
    lookupPair i o' == Just (k, a) && lookupPair (length o) o == Nothing
    where o' = insertAtUnsafe i (k, a) o
          k = next o
#endif

splitAt :: (Enum k, Ord k) => Int -> Order k a -> (Order k a, Order k a)
splitAt n = over _1 fromPairs . over _2 fromPairs . Seq.splitAt n . toPairs

#if !__GHCJS__
prop_splitAt :: (k ~ Char, a ~ String) => ElementPosition k a -> Bool
prop_splitAt (ElementPosition o i) =
    let (a, b) = splitAt (maybe 0 id i) o in
    o == a <> b
#endif

-- Does not check whether k is present
insertAtUnsafe :: (Enum k, Ord k) => Int -> (k, a) -> Order k a -> Order k a
insertAtUnsafe n (k, a) o = uncurry (<>) . over _2 (prependUnsafe (k, a)) . splitAt n $ o

insertAt :: (Enum k, Ord k) => Int -> a -> Order k a -> (Order k a, k)
insertAt n a o = let k = next o in (insertAtUnsafe n (k, a) o, k)

drop :: Enum k => Int -> Order k a -> Order k a
drop n (Order m v) =
  let (a, b) = Seq.splitAt n v in
  Order (Foldable.foldr EnumMap.delete m a) b

take :: Enum k => Int -> Order k a -> Order k a
take n (Order m v) =
  let (a, b) = Seq.splitAt n v in
  Order (Foldable.foldr EnumMap.delete m b) a

deleteAt :: (Enum k, Ord k) => Int -> Order k a -> Order k a
deleteAt n = uncurry (<>) . over _2 (Data.Order.drop 1) . splitAt n

-- Does not check whether k is present
appendUnsafe :: (Enum k, Ord k) => (k, a) -> Order k a -> Order k a
appendUnsafe (k, a) m = Data.Order.insertAtUnsafe (length m) (k, a) m

append :: (Enum k, Ord k) => a -> Order k a -> (Order k a, k)
append a m = let k = next m in (Data.Order.appendUnsafe (k, a) m, k)

-- Does not check whether k is present
prependUnsafe :: (Enum k) => (k, a) -> Order k a -> Order k a
prependUnsafe (k, a) (Order m v) = Order (EnumMap.insert k a m) (Seq.singleton k <> v)

prepend :: (Enum k) => a -> Order k a -> (Order k a, k)
prepend a o = let k = next o in (prependUnsafe (k, a) o, k)

-- | Return the keys in order.
keys :: Order k a -> Seq k
keys = _vec

#if !__GHCJS__
prop_keys :: Order Char String -> Bool
prop_keys (Order m v) = EnumMap.keysSet m == Set.fromList (Foldable.toList v)
#endif

-- | Return the next available key
-- @@
-- λ> next (fromPairs (Vector.fromList [(2, "a"), (5, "b")]))
-- 6
-- λ> next (fromPairs (Data.Vector.fromList []) :: Order Int String)
-- 0
-- @@
next :: Enum k => Order k a -> k
next (Order m _) = head (dropWhile (`EnumMap.member` m) [toEnum 0 ..])
-- next (Order m _) = maybe (toEnum 0) (succ . toEnum . fst) (Set.maxView (EnumMap.keysSet m))

#if !__GHCJS__
prop_next :: Order Char String -> Bool
prop_next o = isNothing (Seq.elemIndexL (next o) (keys o))
#endif

-- | Return the position of a key, Nothing if absent.
pos :: Eq k => k -> Order k a -> Maybe Int
pos k (Order _ v) =
    case Seq.breakl (== k) v of
      (_, x) | Seq.null x -> Nothing
      (x, _) -> Just (Seq.length x)

-- | Return the values in order.
values :: (Enum k) => Order k a -> Seq a
values x = fmap (\k -> _map x ! k) (keys x)

-- | Based on Data.Map.mapKeys
mapKeys :: {-(Enum k1, Enum k2, Ord k2) =>-} (k1 -> k2) -> Order k1 a -> Order k2 a
mapKeys f (Order m v) = Order m (fmap f v)

ilookup :: (Enum k, Ord k) => k -> Order k a -> Maybe (Int, a)
ilookup k (Order m v) =
    case EnumMap.lookup k m of
      Nothing -> Nothing
      Just a -> Just (Seq.length (fst (Seq.breakl (== k) v)), a)

-- | Like 'Data.Set.minView', if k is present returns the position,
-- associated value, and the order with that value removed.
view :: (Enum k, Ord k) => k -> Order k a -> Maybe (Int, a, Order k a)
view k o@(Order m v) =
  case ilookup k o of
    Just (i, a) -> Just (i, a, Order (EnumMap.delete k m) (Seq.filter (/= k) v))
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
    traverse f (Order m v) = Order <$> traverse f m <*> pure v

instance (Enum k, Ord k) => TraversableWithIndex k (Order k) where
    itraverse f (Order m v) = Order <$> itraverse (\k a -> f (toEnum k) a) m <*> pure v

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
    ix k f o@(Order mp sq) =
        case EnumMap.lookup k mp of
          Just a -> fmap (\a' -> Order (EnumMap.insert k a' mp) sq) (f a)
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
    at k f o@(Order mp sq) =
        case EnumMap.lookup k mp of
          Just a ->
              fmap (maybe (Order (EnumMap.delete k mp) (Seq.filter (/= k) sq))
                          (\a' -> Order (EnumMap.insert k a' mp) sq))
                   (f (Just a))
          Nothing ->
              fmap (maybe o
                          (\a' -> Order (EnumMap.insert k a' mp) (sq <> Seq.singleton k)))
                   (f Nothing)

#if !__GHCJS__
instance (Enum k, Ord k, {-Show k,-} Arbitrary k, Arbitrary v) => Arbitrary (Order k v) where
  arbitrary = do
      (ks :: [k]) <- (sized pure >>= \n -> vectorOf n arbitrary) >>= shuffle
      let ks' = Seq.fromList (nub ks)
      vs <- vector (length ks')
      return (fromPairs (Seq.zip ks' (Seq.fromList vs)))
#endif

instance (Ord k, Enum k, LL.FoldableLL (Order k v) (k, v)) => LL.ListLike (Order k v) (k, v) where
  singleton :: (k, v) -> Order k v
  singleton (k, v) = Order (EnumMap.singleton k v) [k]
  null :: Order k v -> Bool
  null (Order _ ks) = LL.null ks
  uncons :: Order k v -> Maybe ((k, v), Order k v)
  uncons (Order mp (k0 :<| ks)) =
    let (mk, mks) = EnumMap.partitionWithKey (\k _ -> k == k0) mp in
      Just (LL.head (EnumMap.toList mk), Order mks ks)
  uncons (Order _ _) = Nothing

instance (Enum k, Ord k, Monoid (Order k v)) => LL.FoldableLL (Order k v) (k, v) where
    foldl f r0 xs = Foldable.foldl (\r k -> f r (k, _map xs ! k)) r0 (_vec xs)
    foldr f r0 xs = Foldable.foldr (\k r -> f (k, _map xs ! k) r) r0 (_vec xs)

$(deriveLiftMany [''Order])

-- | Replace the current ordering with the given key list.  Duplicate
-- keys are ignored, missing keys are appended.
permuteUnsafe :: forall k v. (Enum k, Ord k) => Seq k -> Order k v -> Order k v
permuteUnsafe neworder (Order m _v) =
    Order m (present <> missing)
    where
      -- Remove unknown keys
      present :: Seq k
      present = Seq.fromList (List.nub (toList (Seq.filter (`EnumMap.member` m) neworder)))
      -- Collect the values that are missing from the new key order
      missing :: Seq k
      missing = Seq.fromList (EnumMap.keys (foldr EnumMap.delete m present))

-- deriving instance Serialize v => Serialize (EnumMap k v)
-- $(deriveLiftMany [''EnumMap])
-- $(deriveSafeCopy 1 'base ''EnumMap)

-- | A permute function that verifies neworder is a valid permutation.
permute :: forall k v. (Enum k, Ord k) => Seq k -> Order k v -> Either String (Order k v)
permute neworder (Order m v) =
    case Seq.partition (`EnumMap.member` m) neworder of
      (_, b) | Seq.length b /= 0 -> Left "invalid keys"
      (a, _) | Seq.length a < Seq.length v -> Left "missing keys"
      (a, _) | List.nub (toList a) /= toList a -> Left "duplicate keys"
      _ -> Right (Order m neworder)

#if 0
#if !__GHCJS__
tests :: IO Result
tests = do
  msum [ quickCheckResult prop_next
       , quickCheckResult prop_keys
       , quickCheckResult prop_fromPairs
       , quickCheckResult prop_lookup
       , quickCheckResult prop_lookupKey
       , quickCheckResult prop_lookupPair
       , quickCheckResult prop_splitAt ]
#endif
#endif
