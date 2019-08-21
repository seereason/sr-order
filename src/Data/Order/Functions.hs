{-# LANGUAGE CPP, RankNTypes, ScopedTypeVariables, TupleSections, TypeFamilies #-}

module Data.Order.Functions
  where

import Control.Lens hiding (view)
import qualified Data.Foldable as Foldable
import Data.Foldable as Foldable hiding (toList)
import Data.List as List (nub)
import Data.Map as Map ((!))
import qualified Data.Map as Map (delete, insert, keys, lookup, mapKeys, member)
import Data.Monoid
import Data.Order.Order
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import GHC.Exts as GHC
import Instances.TH.Lift ()
import Prelude hiding (foldMap, length, lookup, map, splitAt, zip)
#if !__GHCJS__
import Data.Map as Map (keysSet)
import Data.Maybe (isNothing)
import Data.Set as Set (fromList)
import Test.QuickCheck (Arbitrary(arbitrary), choose, Gen, quickCheckResult, Result)
#endif

toPairList :: (Ord k, Enum k) => Order k v -> [(k, v)]
toPairList = GHC.toList . toPairs

-- pos :: HasOrder o => o -> Index o -> Maybe Int
-- pos = Order.pos . toOrder

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

prop_lookupKey :: (k ~ Char, a ~ (Char, String)) => InsertPosition k a -> a -> Bool
prop_lookupKey (InsertPosition o i) a =
    lookupKey i o' == Just k && lookupKey (length o) o == Nothing
    where o' = insertAtUnsafe i (k, a) o
          k = next o
#endif

-- | Like Map.lookup.
lookup :: (Ord k) => k -> Order k a -> Maybe a
lookup k (Order m _) = Map.lookup k m

#if !__GHCJS__
prop_lookup :: (k ~ Char, a ~ String) => InsertPosition k a -> a -> Bool
prop_lookup (InsertPosition o i) a =
    Data.Order.Functions.lookup k o' == Just a && Data.Order.Functions.lookup k o == Nothing
    where o' = insertAtUnsafe i (k, a) o
          k = next o
#endif

-- | Lookup pair by position
lookupPair :: (Ord k) => Int -> Order k a  -> Maybe (k, a)
lookupPair i o = lookupKey i o >>= (\k -> fmap (k,) (Data.Order.Functions.lookup k o))

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

drop :: (Ord k) => Int -> Order k a -> Order k a
drop n (Order m v) =
  let (a, b) = Seq.splitAt n v in
  Order (Foldable.foldr Map.delete m a) b

take :: (Ord k) => Int -> Order k a -> Order k a
take n (Order m v) =
  let (a, b) = Seq.splitAt n v in
  Order (Foldable.foldr Map.delete m b) a

deleteAt :: (Enum k, Ord k) => Int -> Order k a -> Order k a
deleteAt n = uncurry (<>) . over _2 (Data.Order.Functions.drop 1) . splitAt n

-- Does not check whether k is present
appendUnsafe :: (Enum k, Ord k) => (k, a) -> Order k a -> Order k a
appendUnsafe (k, a) m = Data.Order.Functions.insertAtUnsafe (length m) (k, a) m

append :: (Enum k, Ord k) => a -> Order k a -> (Order k a, k)
append a m = let k = next m in (Data.Order.Functions.appendUnsafe (k, a) m, k)

-- Does not check whether k is present
prependUnsafe :: (Ord k) => (k, a) -> Order k a -> Order k a
prependUnsafe (k, a) (Order m v) = Order (Map.insert k a m) (Seq.singleton k <> v)

prepend :: (Enum k, Ord k) => a -> Order k a -> (Order k a, k)
prepend a o = let k = next o in (prependUnsafe (k, a) o, k)

-- | Return the keys in order.
keys :: Order k a -> Seq k
keys = _vec

#if !__GHCJS__
prop_keys :: Order Char String -> Bool
prop_keys (Order m v) = Map.keysSet m == Set.fromList (Foldable.toList v)
#endif

-- | Return the next available key
-- @@
-- λ> next (fromPairs (Vector.fromList [(2, "a"), (5, "b")]))
-- 6
-- λ> next (fromPairs (Data.Vector.fromList []) :: Order Int String)
-- 0
-- @@
next :: (Enum k, Ord k) => Order k a -> k
next (Order m _) = head (dropWhile (`Map.member` m) [toEnum 0 ..])
-- next (Order m _) = maybe (toEnum 0) (succ . toEnum . fst) (Set.maxView (Map.keysSet m))

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
values :: (Ord k) => Order k a -> Seq a
values x = fmap (\k -> _map x ! k) (keys x)

-- | Based on Data.Map.mapKeys
mapKeys :: (Ord k2) => (k1 -> k2) -> Order k1 a -> Order k2 a
mapKeys f (Order m v) = Order (Map.mapKeys f m) (fmap f v)

ilookup :: Ord k => k -> Order k a -> Maybe (Int, a)
ilookup k (Order m v) =
    case Map.lookup k m of
      Nothing -> Nothing
      Just a -> Just (Seq.length (fst (Seq.breakl (== k) v)), a)

-- | Like 'Data.Set.minView', if k is present returns the position,
-- associated value, and the order with that value removed.
view :: (Ord k) => k -> Order k a -> Maybe (Int, a, Order k a)
view k o@(Order m v) =
  case ilookup k o of
    Just (i, a) -> Just (i, a, Order (Map.delete k m) (Seq.filter (/= k) v))
    Nothing -> Nothing

member :: Ord k => k -> Order k v -> Bool
member k o = Map.member k (_map o)

-- | Replace the current ordering with the given key list.  Duplicate
-- keys are ignored, missing keys are appended.
permuteUnsafe :: forall k v. Ord k => Seq k -> Order k v -> Order k v
permuteUnsafe neworder (Order m _v) =
    Order m (present <> missing)
    where
      -- Remove unknown keys
      present :: Seq k
      present = Seq.fromList (List.nub (GHC.toList (Seq.filter (`Map.member` m) neworder)))
      -- Collect the values that are missing from the new key order
      missing :: Seq k
      missing = Seq.fromList (Map.keys (foldr Map.delete m present))

-- deriving instance Serialize v => Serialize (EnumMap k v)
-- $(deriveLiftMany [''EnumMap])
-- $(deriveSafeCopy 1 'base ''EnumMap)

-- | A permute function that verifies neworder is a valid permutation.
permute :: forall k v. Ord k => Seq k -> Order k v -> Either String (Order k v)
permute neworder (Order m v) =
    case Seq.partition (`Map.member` m) neworder of
      (_, b) | Seq.length b /= 0 -> Left "invalid keys"
      (a, _) | Seq.length a < Seq.length v -> Left "missing keys"
      (a, _) | List.nub (GHC.toList a) /= GHC.toList a -> Left "duplicate keys"
      _ -> Right (Order m neworder)

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
