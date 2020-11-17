-- | Constraints for manipulating things with an order and possibly a
-- distinct index, e.g. [(k, a)].

{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Order.One
  ( One(OneItem, one)
  -- * Ordered
  , Ordered
  , Ordered'
  , pairs
  , toPairList
  , keys
  , keysSet
  , fromPairs
  , values
  , member
  , pos
  , singleton
  , cons
  -- * With At constraint
  , uncons
  , delete
  , deleteAt
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
  , insertPairAt
  , insertPairAtWith
  , overPairs
  , ioverPairs
  , permute
  , Data.Order.One.sortBy
  -- * With Enum constraint
  , next
  , insertAt
  , append
  , prop_pos_insertAt
  ) where

import Control.Lens hiding (cons, Indexed, uncons)
import Data.List ((\\), nub, sortBy)
import Data.Maybe (fromJust)
import Data.Order.AssocList
import Data.Order.Order
import Data.Set as Set (insert, Set)
import Prelude hiding (break, drop, dropWhile, filter, lookup, splitAt, take, takeWhile)
import Test.QuickCheck

-- | Copied from relude
class One x where
  -- | Type of single element of the structure.
  type OneItem x

  -- | Create a list, map, 'T.Text', etc from a single element.
  one :: OneItem x -> x

instance One [a] where
  type OneItem [a] = a
  one = (:[])

-- | This newtype can satisfy 'At (o k)' because its 'Index' is not
-- forced to be @Index [(k, a)] ~ Int@ as a list's is.
instance One (AssocList k a) where
  type OneItem (AssocList k a) = (k, a)
  one (k, a) = AssocList [(k, a)]

-- Ordered currently pinned to Order
-- instance (Eq k, Ord k) => Ordered (AssocList k) k v

-- | A list-like collection with an index and an ordering.  The index
-- is @k ~ Index (o v)@, while the ordering is implied by the
-- operation of the 'Foldable' and 'FoldableWithIndex' instances.  It
-- may be that the index and the ordering are the same, e.g. for [a]
-- where @Index [a] ~ Int@.
{-
type Ordered (o :: * -> *) v =
  (FoldableWithIndex (Index (o v)) o,
   Ixed (o v),
   IxValue (o v) ~ v,
   Monoid (o v),
   One (o v),
   OneItem (o v) ~ (Index (o v), v),
   Eq (Index (o v)), Ord (Index (o v)))
-}

-- Some weak constraints, I use these to see what is missing
type Ordered' (o :: * -> *) k v = (FoldableWithIndex k o, Eq k, Ord k)

toPairList :: Ordered o k v => o v -> [(k, v)]
toPairList = pairs

class (FoldableWithIndex (Index (o v)) o,
       Ixed (o v),
       Index (o v) ~ k,
       IxValue (o v) ~ v,
       Monoid (o v),
       One (o v),
       OneItem (o v) ~ (k, v),
       Eq k, Ord k,
       o ~ Order k
      ) => Ordered o k v where

  -- | Return the key value pairs in order.
  pairs :: o v -> [(k, v)]
  pairs o = ifoldr (\k v r -> (k, v) : r) [] o

  -- FIXME: this is incredibly slow: pos 3 (fromPairs (fmap (,()) [0..10000]))
  -- Could this be lazy?  Depends on the semigroup instance I guess.
  fromPairs :: [(k, v)] -> o v
  fromPairs [] = mempty
  fromPairs ((k, v) : more) = one (k, v) <> fromPairs more

  -- | Return the keys in order.
  keys :: o v -> [k]
  keys o = ifoldr (\k _ r -> k : r) [] o

  keysSet :: o v -> Set k
  keysSet o = ifoldr (\k _ r -> Set.insert k r) mempty o

  -- | Return the values in order.
  values :: o v -> [v]
  values o = ifoldr (\_ v r -> v : r) [] o

  member :: Index (o v) -> o v -> Bool
  member k o = has (ix k) o

  -- | Use ifoldl rather than ifoldl' so that long lists get short circuited:
  -- % foldr' (\a r -> a == 10 || r) False [1..]
  -- C-c C-cInterrupted.
  -- % foldr (\a r -> a == 10 || r) False [1..]
  -- True
  pos :: k -> o v -> Maybe Int
  pos k o = go 0 o
    where
      go :: Int -> Order k v -> Maybe Int
      go n o =
        case uncons o of
          Nothing -> Nothing
          Just ((k', v), o') ->
            if k == k' then Just n else go (succ n) o'

  singleton :: k ~ Index (o v) => k -> v -> o v
  singleton k v = one (k, v)

  -- Unsafe - doesn't check whether k is present on o.  But that's what cons does, amirite?
  cons :: k ~ Index (o v) => (k, v) -> o v -> o v
  cons (k, a) o = one (k, a) <> o

  -- | Like Map.lookup.
  lookup :: Index (o v) -> o v -> Maybe v
  lookup k = preview (ix k)

  -- This could be more efficient with an At instance
  delete :: k -> o v -> o v
  delete k o = filter (\_ k' _ -> k /= k') o
  -- delete k o = set (at k) Nothing o

  uncons :: o v -> Maybe ((Index (o v), v), o v)
  uncons o | null o = Nothing
  uncons o =
    maybe Nothing (\(k, v) -> Just ((k, v), delete k o)) $ ifoldl' f Nothing o
    where
      -- f :: Index (o v) -> Maybe (k, v) -> IxValue (o v) -> Maybe (k, v)
      f k Nothing v = Just (k, v)
      f _ (Just r) _ = Just r

  break :: ((k, v) -> Bool) -> o v -> (o v, o v)
  break f o =
    case uncons o of
      Nothing -> (mempty, mempty)
      Just (pr, o') ->
        case f pr of
          True -> (mempty, cons pr o')
          False ->
            let (before, after) = break f o' in
              (cons pr before, after)

  takeWhile :: ((k, v) -> Bool) -> o v -> o v
  takeWhile p = fst . break (not . p)

  dropWhile :: ((k, v) -> Bool) -> o v -> o v
  dropWhile p = snd . break (not . p)

  partition ::
       (Int -> k -> v -> Bool)
    -> o v
    -> (o v, o v)
  partition f o =
    snd $ ifoldr g (0, (mempty, mempty)) o
    where
      -- g :: k -> v -> (Int, (o v, o v)) -> (Int, (o v, o v))
      g k v (i, (a, b)) =
        case f i k v of
          True -> (succ i, (cons (k, v) a, b))
          False -> (succ i, (a, cons (k, v) b))

  filter :: (Int -> k -> v -> Bool) -> o v -> o v
  -- filter f o = fst (partition f o)
  filter f o = snd $ ifoldr (\k v (n, o') -> (succ n, if f n k v then cons (k, v) o' else o')) (0, mempty) o

  -- | Replace the current ordering with the given key list.  Duplicate
  -- keys are ignored, missing keys are appended.  FIXME: should this be
  -- a method?  The Order type might override this with some benefit.
  permute :: [k] -> o v -> o v
  permute neworder o =
    foldr (\k r -> maybe r (\v -> cons (k, v) r) (lookup k o)) mempty neworder'
    where
      neworder' = nub neworder <> (keys o \\ neworder)

  splitAt :: Int -> o v -> (o v, o v)
  splitAt i o | i <= 0 = (mempty, o)
  splitAt i o =
    case uncons o of
      Nothing -> (mempty, mempty)
      Just (pr, o') ->
        let (before, after) = splitAt (pred i) o' in
          (cons pr before, after)

  deleteAt :: Int -> o v -> o v
  deleteAt n o =
    before <> maybe mempty snd (uncons after)
    where
      (before, after) = splitAt n o

  -- | Insert @v@ at a specified position in @o@, and associate it with
  -- the key @k@.
  insertPairAt :: Int -> (k, v) -> o v -> o v
  insertPairAt n (k, a) o =
    let (before, after) = splitAt n o in
    before <> one (k, a) <> after

  -- | Insert a pair at position n, unless the key is already present in
  -- which case update it with the combinator.
  insertPairAtWith ::
    At (o v)
    => (v -> v -> v)
    -> Int
    -> (k, v)
    -> o v
    -> o v
  insertPairAtWith f n (k, new) o =
    case view (at k) o of
      Nothing -> insertPairAt n (k, new) o
      Just old -> set (at k) (Just (f old new)) o

  take :: Int -> o v -> o v
  take n = fst . splitAt n

  drop :: Int -> o v -> o v
  drop n = snd . splitAt n

  lookupPair :: Int -> o v -> Maybe (k, v)
  lookupPair n = fmap fst . uncons . take 1 . drop n

  -- FIXME: uses fromJust
  sortBy ::
    At (o v)
    => ((k, v) -> (k, v) -> Ordering)
    -> o v
    -> o v
  sortBy cmp o =
    permute (Data.List.sortBy cmp' (keys o)) o
    where
      cmp' k1 k2 = cmp (k1, fromJust (view (at k1) o)) (k2, fromJust (view (at k2) o))

  -- | Return the next available key
  -- @@
  -- λ> next (AssocList [(2, "a"), (5, "b")])
  -- 6
  -- λ> next (AssocList ([] :: [(Int, String)]))
  -- 0
  -- @@
  -- Note that this will fail if one of the keys equals maxBound
  next :: Enum k => o v -> k
  next = foldr max (toEnum 0) . fmap succ . keys

  insertAt :: Enum k => Int -> v -> o v -> (o v, k)
  insertAt n a o =
    (insertPairAt n (k, a) o, k)
    where k = next o

  append :: Enum k => o v -> v -> (o v, k)
  append o v = let k = next o in (o <> one (k, v), k)

instance (Ord k, Enum k) => One (Order k v) where
  type OneItem (Order k v) = (k, v)
  one (k, v) = fromPairsUnsafe @[] [(k, v)]

instance (Eq k, Ord k, Enum k) => Ordered (Order k) k v where
  -- Override methods that could benefit from the At instance
  delete k o = set (at k) Nothing o
  -- we should also do good uncons, splitAt, and break implementations here

overPairs ::
  forall o v k o' v' k'.
  (Ordered o k v, Ordered o' k' v')
  => ((k, v) -> (k', v'))
  -> o v
  -> o' v'
overPairs f o = ifoldr g mempty o
  where
    -- g :: k -> v -> o' v' -> o' v'
    g k v r = one (f (k, v)) <> r

ioverPairs ::
  forall o v k o' v' k'.
  (Ordered o k v, Ordered o' k' v')
  => (Int -> (k, v) -> (k', v'))
  -> o v
  -> o' v'
ioverPairs f o = foldr g mempty (zip [0..] (pairs o))
  where
    -- g :: (Int, (k, v)) -> o' v' -> o' v'
    g (i, (k, v)) r = one (f i (k, v)) <> r

{-
-- This doesn't work because it provokes v ~ (Int, v).  Don't even
-- try.

instance (FoldableWithIndex (Index [v]) [],
          Ixed [v],
          Index [v] ~ k,
          IxValue [v] ~ v,
          Monoid [v],
          One [v],
          OneItem [v] ~ (k, v),
          Eq k, Ord k) => Ordered [] Int v where
-}

{-
instance Ordered Set UserId ()

instance One (Set UserId) where
  type OneItem (Set UserId) = (UserId, ())
  one = Set.singleton
-}

prop_pos_insertAt :: Char -> Order Char () -> Property
prop_pos_insertAt c o =
  forAll (choose (0, length o)) $ \n ->
  let (o', k) = insertAt n () o in
  pos k o' == Just n
