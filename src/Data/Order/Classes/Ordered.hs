-- | Constraints for manipulating things with an order and possibly a
-- distinct index, e.g. [(k, a)].

{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Order.Classes.Ordered
  ( Ordered(pairs,
            toMap,
            toVec,
            keys,
            keysSet,
            fromPairs,
            fromMapAndVec,
            values,
            member,
            pos,
            keyView,
            Data.Order.Classes.Ordered.singleton,
            cons,
            uncons,
            delete,
            deleteAt,
            splitAt,
            splitAtExactMay,
            break,
            takeWhile,
            dropWhile,
            take,
            takeExactMay,
            drop,
            dropExactMay,
            lookupPair,
            lookup,
            partition,
            filter,
            insertPairAt,
            insertPairAtWith,
            permute,
            Data.Order.Classes.Ordered.sortBy,
            next,
            insertAt,
            append,
            difference,
            union,
            unions,
            valid,
            repair)
  , Ordered'

  , toPairList -- aka pairs
  , headMay
  , tailMay
  , overPairs
  , ioverPairs
  , lookupKey
  , elems

  , prop_fromPairs
  , prop_keys
  , prop_splitAt
  , prop_next
  , prop_lookupKey
  , prop_lookup
  , prop_lookupPair
  , prop_uncons
  , prop_null
  , prop_singleton
  , prop_toPairs_fromPairs
  , prop_delete
  , prop_insertAt
  , prop_insert_delete
  , prop_insert_delete_pos
  , prop_pos_insertAt
  ) where

--import Debug.Trace
import Control.Lens hiding (cons, Indexed, uncons)
import Data.List ((\\), nub, sortBy)
import Data.Maybe (fromJust, isNothing)
import Data.Order.Classes.One (One(OneItem, one))
import Data.Proxy
import Data.Map (Map)
import qualified Data.Map as Map (insert, lookup)
import Data.Set as Set (fromList, insert, notMember, Set)
import Data.Typeable (Typeable)
import Data.Vector (Vector)
import qualified Data.Vector as Vector (cons)
import Prelude hiding (break, drop, dropWhile, filter, lookup, splitAt, take, takeWhile)
import Test.QuickCheck

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
       -- At (o v),
       Monoid (o v),
       One (o v),
       OneItem (o v) ~ (k, v),
       Eq k, Ord k,
       Typeable k, Typeable v, Typeable (o v)
      ) => Ordered o k v where

  -- | Return the key value pairs in order.
  pairs :: o v -> [(k, v)]
  pairs o = ifoldr (\k v r -> (k, v) : r) [] o
  {-# INLINE pairs #-}

  toMap :: o v -> Map k v
  toMap o = ifoldr (\k v r -> Map.insert k v r) mempty o
  {-# INLINE toMap #-}

  toVec :: o v -> Vector k
  toVec o = ifoldr (\k _ r -> Vector.cons k r) mempty o
  {-# INLINE toVec #-}

  -- FIXME: this is incredibly slow: pos 3 (fromPairs (fmap (,()) [0..10000]))
  -- Could this be lazy?  Depends on the semigroup instance I guess.
  fromPairs :: [(k, v)] -> o v
  fromPairs [] = mempty
  fromPairs ((k, v) : more) = one (k, v) <> fromPairs more
  {-# INLINE fromPairs #-}

  -- Keys missing from m will be omitted.
  fromMapAndVec :: Map k v -> Vector k -> o v
  fromMapAndVec m ks =
    foldr (\k o -> maybe o (\v -> cons (k, v) o) (Map.lookup k m)) mempty ks

  -- | Return the keys in order.
  keys :: o v -> [k]
  keys o = ifoldr (\k _ r -> k : r) [] o
  {-# INLINE keys #-}

  keysSet :: o v -> Set k
  keysSet o = ifoldr (\k _ r -> Set.insert k r) mempty o
  {-# INLINE keysSet #-}

  -- | Return the values in order.
  values :: o v -> [v]
  values o = ifoldr (\_ v r -> v : r) [] o
  {-# INLINE values #-}

  member :: Index (o v) -> o v -> Bool
  member k o = has (ix k) o
  {-# INLINE member #-}

  -- | Use ifoldl rather than ifoldl' so that long lists get short circuited:
  -- % foldr' (\a r -> a == 10 || r) False [1..]
  -- C-c C-cInterrupted.
  -- % foldr (\a r -> a == 10 || r) False [1..]
  -- True
  pos :: k -> o v -> Maybe Int
  pos k o0 = go 0 o0
    where
      go :: Int -> o v -> Maybe Int
      go n o =
        case uncons o of
          Nothing -> Nothing
          Just ((k', _v), o') ->
            if k == k' then Just n else go (succ n) o'
  {-# INLINE pos #-}

  -- | Like 'Data.Set.minView', if k is present returns the position,
  -- associated value, and the order with that value removed.  (was view)
  keyView :: k -> o v -> Maybe (Int, v, o v)
  keyView k o =
    case (lookup k o, pos k o) of
      (Just v, Just i) -> Just (i, v, delete k o)
      _ -> Nothing

  singleton :: k ~ Index (o v) => k -> v -> o v
  singleton k v = one (k, v)

  -- Unsafe - doesn't check whether k is present on o.  But that's what cons does, amirite?
  cons :: k ~ Index (o v) => (k, v) -> o v -> o v
  cons (k, a) o = one (k, a) <> o

  -- | Like Map.lookup.
  lookup :: k ~ Index (o v) => k -> o v -> Maybe v
  lookup k = preview (ix k)

  delete :: k -> o v -> o v
  -- delete k o = filter (\_ k' _ -> k /= k') o
  delete k o = filter (\_ k' _ -> k /= k') o
    -- set (at k) Nothing o

  -- > fmap fst (uncons (fromPairs (fmap (,()) ([0..1000000] :: [Int])) :: AssocList Int ()))
  -- Just (0,())
  uncons :: o v -> Maybe ((Index (o v), v), o v)
  uncons o | null o = Nothing
  uncons o =
    -- use ifoldl' to avoid stack overflow
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

  -- | Replace the current ordering with the given key list.
  -- Duplicate keys are ignored, missing keys are appended.  The Order
  -- type might override this with some benefit.
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

  splitAtExactMay :: Int -> o v -> Maybe (o v, o v)
  splitAtExactMay n o | 0 > n || n > length o = Nothing
  splitAtExactMay n o = Just $ splitAt n o

  take :: Int -> o v -> o v
  take n = fst . splitAt n

  takeExactMay :: Int -> o v -> Maybe (o v)
  takeExactMay n o = fmap fst (splitAtExactMay n o)

  drop :: Int -> o v -> o v
  drop n = snd . splitAt n

  dropExactMay :: Int -> o v -> Maybe (o v)
  dropExactMay n = fmap snd . splitAtExactMay n

  deleteAt :: Int -> o v -> o v
  deleteAt n o =
    before <> maybe mempty snd (uncons after)
    where
      (before, after) = splitAt n o

  -- | Insert @v@ at a specified position in @o@, and associate it with
  -- the key @k@.
  --
  -- o=[('a',1),('b',2),('c',3),('d',4),('e',5)]
  -- InsertPairAt i=4 ('f',6) -> [('a',1),('b',2),('c',3),('d',4),('f',6),('e',5)]
  -- oldi=2, oldv=3
  -- InsertPairAt i=2 ('c',6) -> [('a',1),('b',2),('c',6),('d',4),('e',5)]
  -- InsertPairAt i=1 ('c',6) -> [('a',1),('c',6),('b',2),        ('d',4),('e',5)]
  -- InsertPairAt i=4 ('c',6) -> [('a',1),('b',2),        ('d',4),('c',6),('e',5)]
  insertPairAt :: Int -> (k, v) -> o v -> o v
  insertPairAt i (k, v) o =
    case keyView k o of
      Nothing ->
        let (before, after) = splitAt i o in
          before <> one (k, v) <> after
      Just (oldi, _oldv, o') ->
        insertPairAt (if i > oldi then i - 1 else i) (k, v) o'

  -- | Insert a pair at position n, unless the key is already present in
  -- which case update it with the combinator.
  insertPairAtWith ::
    Ixed (o v)
    => (v -> v -> v)
    -> Int
    -> (k, v)
    -> o v
    -> o v
  insertPairAtWith f i (k, v) o =
    case keyView k o of
      Nothing -> insertPairAt i (k, v) o
      Just (oldi, oldv, o') ->
        insertPairAt (if i > oldi then i - 1 else i) (k, f oldv v) o'

  insertAt :: Enum k => Int -> v -> o v -> (o v, k)
  insertAt n a o =
    (insertPairAt n (k, a) o, k)
    where k = next o

  lookupPair :: Int -> o v -> Maybe (k, v)
  lookupPair n o = fst <$> (uncons =<< dropExactMay n o)

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

  append :: Enum k => o v -> v -> (o v, k)
  append o v = let k = next o in (o <> one (k, v), k)

  difference :: o v -> o v -> o v
  difference a b =
    let bks = keysSet b in
    filter (\_ k _ -> Set.notMember k bks) a

  union :: o v -> o v -> o v
  union a b = a <> difference b a

  unions :: [o v] -> o v
  unions os =
    fst $ foldr f (mempty, mempty) os
    where
      f :: o v -> (o v, Set k) -> (o v, Set k)
      f o (r, ks) =
        let r' :: o v
            r' = r <> filter (\_ k _ -> Set.notMember k ks) o
            ks' = ks <> keysSet o in
        (r', ks')

  valid :: o v -> Bool
  repair :: o v -> o v

headMay :: Ordered o k v => o v -> Maybe (k, v)
headMay o = fmap fst (uncons o)

tailMay :: Ordered o k v => o v -> Maybe (o v)
tailMay o = fmap snd (uncons o)

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

-- | Lookup key by position.  A lookup function appears in
-- containers in version 0.5.8.
lookupKey :: Ordered o k v => Int -> o v -> Maybe k
lookupKey i o = fmap fst (lookupPair i o)

elems :: (Ordered o k v, Typeable k, Typeable v) => o v -> [v]
elems = values

prop_fromPairs :: forall o v k. (Ordered o k v, Eq (o v), Eq k, Ord k, Eq v) => o v -> Bool
prop_fromPairs o = fromPairs (pairs o) == o

prop_keys :: forall o v k. (Ordered o (Index (o v)) v, k ~ Index (o v)) => o v -> Bool
prop_keys o = keysSet o == Set.fromList (keys o)

prop_splitAt :: forall o v k. (Ordered o k v, k ~ Index (o v), Eq (o v)) => o v -> Property
prop_splitAt o =
  forAll (choose (0, length o)) $ \i ->
  let (a, b) = Data.Order.Classes.Ordered.splitAt i o in
    o == (a <> b)

prop_next :: forall o v k. (Ordered o k v, Enum k) => o v -> Bool
prop_next o = isNothing (pos (next o) o)

prop_lookupKey :: forall o v k. (Ordered o k v, Arbitrary (o v), Enum k) => o v -> v -> Property
prop_lookupKey o a =
  forAll (choose (0, length o)) $ \i ->
  let o' :: o v
      o' = insertPairAt i (k, a) o
      k = next o in
  lookupKey i o' == Just k && lookupKey (length o) o == Nothing

-- If we insert (next o, a) into o at position i, we should then find a at
-- k in the new order and we should not find it in the original
-- order.
prop_lookup :: forall o v k. (Ordered o k v, Arbitrary (o v), Enum k, Eq v) => o v -> v -> Property
prop_lookup o v =
  forAll (choose (0, length o)) $ \i ->
  let o' = insertPairAt i (k, v) o
      k = next o in
    lookup k o' == Just v && lookup k o == Nothing

-- If we insert a pair at a position, lookupPair of that position must return the pair.
prop_lookupPair :: forall o v k. (Ordered o k v, Eq (o v), Enum k, Eq v, Show k, Show v, Show (o v)) => o v -> v -> Property
prop_lookupPair o a =
  forAll (choose (0, length ({-trace ("o=" <> show o)-} o))) $ \i ->
  let o' = insertPairAt ({-trace ("i=" <> show i)-} i) ({-trace ("pair=" <> show (k, a))-} (k, a)) o
      k = next o in
  lookupPair i ({-trace ("o'=" <> show o')-} o') == Just (k, a)

prop_uncons :: forall o v k. (Ordered o k v, Eq (o v), Eq v) => o v -> Bool
prop_uncons o = o == maybe mempty (\(pair, o') -> one pair <> o') (uncons o)

prop_null :: forall o v k. Ordered o k v => o v -> Bool
prop_null o = null o == isNothing (uncons o)

instance Arbitrary (Proxy a) where arbitrary = pure Proxy

prop_singleton :: forall o v k. (Ordered o k v, k ~ Index (o v), Eq v, Eq (o v)) => Proxy (o v) -> (k, v) -> Bool
prop_singleton _ pair = uncons (one pair :: o v) == Just (pair, mempty)

-- | Map and list should contain the same keys with no duplicates
prop_toPairs_fromPairs ::
  forall o v k. (Ordered o k v, Eq (o v), Arbitrary (o v), Arbitrary k, Arbitrary v)
  => o v
  -> Bool
prop_toPairs_fromPairs o =
    fromPairs (pairs o) == o

prop_delete :: forall o v k. (Ordered o k v) => o v -> Property
prop_delete o | length o == 0 = property True
prop_delete o =
    forAll (choose (0, length o - 1)) $ \i ->
    length (deleteAt i o) == length o - 1

prop_insertAt :: forall o v k. (Ordered o k v) => (k, v) -> o v -> Property
prop_insertAt v@(k, _) o =
    forAll (choose (0, length o)) $ \i ->
    member k o || (length (insertPairAt i v o) == length o + 1)

{-
prop_insertAt_deleteAt :: (Int, String) -> Order Int String -> Property
prop_insertAt_deleteAt v@(k, _) o =
  forAll (choose (0, Foldable.length o)) $ \i ->
  if Data.Order.member k o
  then deleteAt i o == deleteAt i (insertPairAt i v (deleteAt i o))
  else o == deleteAt i (insertPairAt i v o)
-}

-- | Use an explicit generator to create a valid list position.
prop_insert_delete :: forall o v k. (Ordered o k v, Eq (o v), Eq v) => (k, v) -> o v -> Property
prop_insert_delete (k, a) o =
    forAll (choose (0, length o)) $ \i ->
        member k o || (keyView k (insertPairAt i (k, a) o) == Just (i, a, o))

prop_insert_delete_pos :: forall o v k. (Ordered o k v, Eq (o v)) => (k, v) -> o v -> Property
prop_insert_delete_pos v@(k, _) o =
    forAll (choose (0, length o)) $ \i ->
        member k o || (deleteAt i (insertPairAt i v o) == o)

prop_pos_insertAt :: forall o v k. (Ordered o k v, Enum k) => v -> o v -> Property
prop_pos_insertAt v o =
  forAll (choose (0, length o)) $ \n ->
  let (o', k) = insertAt n v o in
  pos k o' == Just n
