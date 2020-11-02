-- | Constraints for manipulating things with an order and possibly a
-- distinct index, e.g. [(k, a)].

{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Order.One
  ( One(OneItem, one)
  , IndexedOrder
  , AList
  , pairs
  , toPairList
  , fromPairs
  , keys
  , keysSet
  , values
  , pos
  , cons
  , uncons
  , delete
  , deleteAt
  , singleton
  , splitAt
  , break
  , takeWhile
  , dropWhile
  , take
  , drop
  , lookupPair
  , lookup
  , partition
  , filter
  , next
  , insertAt
  , insertPairAt
  , insertPairAtWith
  , append
  , overPairs
  , ioverPairs
  , permute
  , Data.Order.One.sortBy
  ) where

import Control.Lens hiding (cons, uncons)
import Data.List ((\\), nub, sortBy)
import Data.Maybe (fromJust)
import Data.Order.AssocList
import Data.Set as Set (insert, Set)
import Prelude hiding (break, drop, dropWhile, filter, lookup, splitAt, take, takeWhile)

-- | Copied from relude
class One x where
  -- | Type of single element of the structure.
  type OneItem x

  -- | Create a list, map, 'T.Text', etc from a single element.
  one :: OneItem x -> x

instance One [a] where
  type OneItem [a] = a
  one = (:[])

instance One (AssocList k a) where
  type OneItem (AssocList k a) = (k, a)
  one (k, a) = AssocList [(k, a)]

-- type IndexedOrder o k v = (o ~ Order k)
type IndexedOrder (o :: * -> *) k v =
  (FoldableWithIndex k o,
   Ixed (o v), Index (o v) ~ k, IxValue (o v) ~ v,
   Eq k, Ord k,
   Monoid (o v),
   One (o v),
   OneItem (o v) ~ (Index (o v), v),
   At (o v))

type AList o v = (IndexedOrder o (Index (o v)) v)

-- | Return the key value pairs in order.
pairs :: IndexedOrder o k v => o v -> [(k, v)]
pairs o = ifoldr (\k v r -> (k, v) : r) [] o

toPairList :: IndexedOrder o k v => o v -> [(k, v)]
toPairList = pairs

fromPairs :: (AList o v, k ~ Index (o v)) => [(k, v)] -> o v
fromPairs [] = mempty
fromPairs ((k, v) : more) = one (k, v) <> fromPairs more

-- | Return the keys in order.
keys :: IndexedOrder o k v => o v -> [k]
keys o = ifoldr (\k _ r -> k : r) [] o

keysSet :: IndexedOrder o k v => o v -> Set k
keysSet o = ifoldr (\k _ r -> Set.insert k r) mempty o

-- | Return the values in order.
values :: IndexedOrder o k v => o v -> [v]
values = ifoldr (\_ v r -> v : r) []

-- | Use ifoldr rather than ifoldr' so that long lists get short circuited:
-- % foldr' (\a r -> a == 10 || r) False [1..]
-- C-c C-cInterrupted.
-- % foldr (\a r -> a == 10 || r) False [1..]
-- True
pos :: IndexedOrder o k v => k -> o v -> Maybe Int
pos k o =
  either Just (const Nothing) $
    ifoldr (\k' _ (r :: Either Int Int) ->
               either
                 (\i -> if k == k' then Right i else Left (succ i))
                 Right
                 r) (Left 0) o

delete :: At t => Index t -> t -> t
delete k o = set (at k) Nothing o

deleteAt :: AList o v => Int -> o v -> o v
deleteAt n o =
  before <> maybe mempty snd (uncons after)
  where
    (before, after) = splitAt n o

singleton :: (One x, OneItem x ~ (a, b)) => a -> b -> x
singleton k a = one (k, a)

-- Unsafe - doesn't check whether k is present on o.  But that's what cons does, amirite?
cons :: (Monoid (o v), One (o v), OneItem (o v) ~ (k, v)) => (k, v) -> o v -> o v
cons (k, a) o = one (k, a) <> o

-- uncons :: forall k a. Enum k => Order k a -> Maybe ((k, a), Order k a)
uncons :: forall o k v. (AList o v, k ~ Index (o v)) => o v -> Maybe ((Index (o v), v), o v)
uncons o | null o = Nothing
uncons o =
  maybe Nothing (\(k, v) -> Just ((k, v), delete k o)) $ ifoldl' f Nothing o
  where
    f :: Index (o v) -> Maybe (k, v) -> IxValue (o v) -> Maybe (k, v)
    f k Nothing v = Just (k, v)
    f _ (Just r) _ = Just r

splitAt :: (AList o v) => Int -> o v -> (o v, o v)
splitAt i o | i <= 0 = (mempty, o)
splitAt i o =
  case uncons o of
    Nothing -> (mempty, mempty)
    Just (pr, o') ->
      let (before, after) = splitAt (pred i) o' in
        (cons pr before, after)
      -- over _1 (cons (one pr)) (splitAt (pred i) o')

-- | Return the next available key
-- @@
-- λ> next (AssocList [(2, "a"), (5, "b")])
-- 6
-- λ> next (AssocList ([] :: [(Int, String)]))
-- 0
-- @@
next :: (AList o v, k ~ Index (o v), Enum k) => o v -> k
next = foldr max (toEnum 0) . fmap succ . keys

insertAt :: (AList o v, k ~ Index (o v), Enum k) => Int -> v -> o v -> (o v, k)
insertAt n a o =
  (insertPairAt n (k, a) o, k)
  where k = next o

-- insertAt :: (Ord k, Enum k) => Int -> a -> Order k a -> (Order k a, k)
insertPairAt :: (AList o v, k ~ Index (o v)) => Int -> (k, v) -> o v -> o v
insertPairAt n (k, a) o =
  let (before, after) = splitAt n o in
  before <> one (k, a) <> after

-- | Insert a pair at position n, unless the key is already present in
-- which case update it with the combinator.
insertPairAtWith ::
  (AList o v, k ~ Index (o v))
  => (v -> v -> v)
  -> Int
  -> (k, v)
  -> o v
  -> o v
insertPairAtWith f n (k, new) o =
  case view (at k) o of
    Nothing -> insertPairAt n (k, new) o
    Just old -> set (at k) (Just (f old new)) o

append :: (AList o v, k ~ Index (o v), Enum k) => o v -> v -> (o v, k)
append o v = let k = next o in (o <> one (next o, v), k)

take :: (AList o v) => Int -> o v -> o v
take n = fst . splitAt n

drop :: (AList o v) => Int -> o v -> o v
drop n = snd . splitAt n

lookupPair :: (AList o v, k ~ Index (o v)) => Int -> o v -> Maybe (k, v)
lookupPair n = fmap fst . uncons . take 1 . drop n

-- | Like Map.lookup.
lookup :: (AList o v) => Index (o v) -> o v -> Maybe v
lookup k = view (at k)

break :: (AList o v, k ~ Index (o v)) => ((k, v) -> Bool) -> o v -> (o v, o v)
break f o =
  case uncons o of
    Nothing -> (mempty, mempty)
    Just (pr, o') ->
      case f pr of
        True -> (mempty, cons pr o')
        False ->
          let (before, after) = break f o' in
            (cons pr before, after)

takeWhile :: (AList o v, k ~ Index (o v)) => ((k, v) -> Bool) -> o v -> o v
takeWhile p = fst . break (not . p)

dropWhile :: (AList o v, k ~ Index (o v)) => ((k, v) -> Bool) -> o v -> o v
dropWhile p = snd . break (not . p)

partition ::
  forall o k v. (AList o v, k ~ Index (o v))
  => (Int -> k -> v -> Bool)
  -> o v
  -> (o v, o v)
partition f o =
  snd $ ifoldr g (0, (mempty, mempty)) o
  where
    g :: k -> v -> (Int, (o v, o v)) -> (Int, (o v, o v))
    g k v (i, (a, b)) =
      case f i k v of
        True -> (succ i, (cons (k, v) a, b))
        False -> (succ i, (a, cons (k, v) b))

filter :: (AList o v, k ~ Index (o v)) => (Int -> k -> v -> Bool) -> o v -> o v
filter f o = fst (partition f o)

overPairs ::
  forall o v k o' v' k'.
  (AList o v, AList o' v', k ~ Index (o v), k' ~ Index (o' v'))
  => ((k, v) -> (k', v'))
  -> o v
  -> o' v'
overPairs f o = ifoldr g mempty o
  where
    g :: k -> v -> o' v' -> o' v'
    g k v r = one (f (k, v)) <> r

ioverPairs ::
  forall o v k o' v' k'.
  (AList o v, AList o' v', k ~ Index (o v), k' ~ Index (o' v'))
  => (Int -> (k, v) -> (k', v'))
  -> o v
  -> o' v'
ioverPairs f o = foldr g mempty (zip [0..] (pairs o))
  where
    g :: (Int, (k, v)) -> o' v' -> o' v'
    g (i, (k, v)) r = one (f i (k, v)) <> r

-- | Replace the current ordering with the given key list.  Duplicate
-- keys are ignored, missing keys are appended.  FIXME: should this be
-- a method?  The Order type might override this with some benefit.
permute :: (AList o v, k ~ Index (o v)) => [k] -> o v -> o v
permute neworder o =
  foldr (\k r -> maybe r (\v -> cons (k, v) r) (lookup k o)) mempty neworder'
  where
    neworder' = nub neworder <> (keys o \\ neworder)

-- FIXME: uses fromJust
sortBy ::
  forall o k v. (AList o v, k ~ Index (o v))
  => ((k, v) -> (k, v) -> Ordering)
  -> o v
  -> o v
sortBy cmp o =
  permute (Data.List.sortBy cmp' (keys o)) o
  where
    cmp' :: k -> k -> Ordering
    cmp' k1 k2 = cmp (k1, fromJust (view (at k1) o)) (k2, fromJust (view (at k2) o))
