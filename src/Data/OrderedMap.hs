{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS -Wall #-}

module Data.OrderedMap
    ( OrderedMap(..)
    , OrderError(..)
    , putItem
    , showOrder
    , permute
    , appendItems
    , toList
    , fromElements
    , asList
    , find
    , findWithKey
    , keyToPos
    , lens_omat
    ) where

import Control.Lens (_1, Ixed, Index, IxValue, _Just, lens, over, Traversal')
import Data.Data (Data)
import Data.Default (Default)
import Data.List as List (elem, filter, notElem, partition)
import Data.Map as Map (Map, (!))
import qualified Data.Map as Map
import Data.SafeCopy (base)
import Data.SafeCopy.Derive (deriveSafeCopy)
import GHC.Generics (Generic)
import Language.Haskell.TH.TypeGraph.Serialize (deriveSerialize)

data OrderError
    = InvalidKey
    | InvalidPermutation
    | DuplicateKey
    | OutOfRange Int
    | EmptyOrder -- ^ Expected an order with at least one element
    deriving (Data, Eq, Ord, Show, Generic)

$(deriveSerialize [t|OrderError|])
$(deriveSafeCopy 1 'base [t|OrderError|])

-- | Minimum implementation: fromMapListKey, toMap, toKeys, nextKey, newKey
class (Ixed o, Ord (Index o), Enum (Index o), Default o) => OrderedMap o where
    size :: o -> Int
    size = Map.size . toMap

    lookByKey :: Index o -> o -> Maybe (IxValue o)
    lookByKey k o = Map.lookup k (toMap o)
    -- | Indexed version of lookByKey.
    ilookByKey :: Index o -> o -> Maybe (Int, IxValue o)
    ilookByKey k o =
        case Map.lookup k (toMap o) of
          Just v -> Just (length (takeWhile (/= k) (toKeys o)), v)
          Nothing -> Nothing
    lookByPos :: Int -> o -> Maybe (Index o, IxValue o)
    lookByPos pos o = case drop pos (toKeys o) of
                          k : _ -> fmap (k,) (lookByKey k o)
                          _ -> Nothing

    nextKey :: o -> Index o
    newKey :: o -> (Index o, o)

    empty :: o
    empty = fromMapAndList mempty mempty
    fromPairs :: [(Index o, IxValue o)] -> o
    fromPairs ps = fromMapAndList (Map.fromList ps) (fmap fst ps)
    -- | (unsafe - correspondence between map and list keys not enforced)
    fromMapAndList :: Map (Index o) (IxValue o) -> [Index o] -> o
    fromMapAndList mp ks = fromMapListKey mp ks (maximum (toEnum 0: fmap succ ks))
    -- | (even less safe - a bogus next key value could be supplied)
    fromMapListKey :: Map (Index o) (IxValue o) -> [Index o] -> Index o -> o

    toPairs :: o -> [(Index o, IxValue o)]
    toPairs o = let mp = toMap o in map (\k -> (k, mp ! k)) (toKeys o)
    toMap :: o -> Map (Index o) (IxValue o)
    -- toMap = Map.fromList . toPairs
    toKeys :: o -> [Index o]
    -- toKeys = map fst . toPairs

    -- | Set the value associated with a key.  If the key is not
    -- present a new entry is created (where?)
    alter :: (Maybe (IxValue o) -> Maybe (IxValue o)) -> Index o -> o -> o
    alter f k o = fromMapListKey (Map.alter f k (toMap o)) (toKeys o) (nextKey o)

    -- | Unsafe permute function - does not check whether first argument
    -- is a permutation of the existing keys.
    permuteUnsafe :: [Index o] -> o -> o
    permuteUnsafe keys o = fromMapListKey (toMap o) keys (nextKey o)

    -- | Move the head element to a specified position.
    moveHeadUnsafe :: Int -> o -> o
    moveHeadUnsafe 0 o = o
    moveHeadUnsafe n o =
        case toKeys o of
          [] -> o
          (k : ks) ->
              let (ks1, ks2) = splitAt n ks in
              permuteUnsafe (ks1 ++ [k] ++ ks2) o

    moveHead :: Int -> o -> Either OrderError o
    moveHead _ o | size o == 0 = Left EmptyOrder
    moveHead n o | n > size o = Left (OutOfRange n {-o-})
    moveHead n o = Right (moveHeadUnsafe n o)

    viewByKey :: Index o -> o -> Either OrderError (Int, IxValue o, o)
    viewByKey k o =
        -- Invariant: order keys are unique
        case break ((== k) . fst) (toPairs o) of
          (_, []) -> Left InvalidKey
          (pre, (_, x) : post) -> Right (length pre, x, fromPairs (pre ++ post))

    -- | Delete if present
    deleteByKeyMaybe :: Index o -> o -> o
    deleteByKeyMaybe k o =
        fromMapListKey (Map.delete k (toMap o)) (filter (/= k) (toKeys o)) (nextKey o)
    deleteByKey :: Index o -> o -> Either OrderError o
    deleteByKey k o =
        maybe (Left (InvalidKey {-k o-}))
              (\_ -> Right (deleteByKeyMaybe k o))
              (Map.lookup k (toMap o))

    deleteByPosUnsafe :: Int -> o -> o
    deleteByPosUnsafe n o =
        case drop n (toKeys o) of
          k : _ -> deleteByKeyMaybe k o
          _ -> o
    deleteByPos :: Int -> o -> Either OrderError o
    deleteByPos n o | n > size o = Left (OutOfRange n {-o-})
    deleteByPos n o = Right (deleteByPosUnsafe n o)

    -- | Put a new element at the beginning of the order, returning a
    -- pair containing the new OrderedMap and the new key.
    prepend :: IxValue o -> o -> (o, Index o)
    prepend v o = let (k, o') = newKey o in (prependWithKeyUnsafe k v o', k)

    prependWithKey :: Index o -> IxValue o -> o -> Either OrderError o
    prependWithKey k _ o | Map.member k (toMap o) = Left (DuplicateKey {-k o-})
    prependWithKey k v o = Right (prependWithKeyUnsafe k v o)

    prependWithKeyUnsafe :: Index o -> IxValue o -> o -> o
    prependWithKeyUnsafe k _ o | Map.member k (toMap o) = error "prependWithKey"
    prependWithKeyUnsafe k v o =
        fromMapListKey (Map.insert k v (toMap o)) (k : toKeys o) (max (succ k) (nextKey o))

    -- | Insert an element at a specific position.
    insertAtUnsafe :: Int -> IxValue o -> o -> (o, Index o)
    insertAtUnsafe n v o = let (o', k) = prepend v o in (moveHeadUnsafe n o', k)

    insertAt :: Int -> IxValue o -> o -> Either OrderError (o, Index o)
    insertAt n _ o | n > size o = Left (OutOfRange n {-o-})
    insertAt n v o = Right (insertAtUnsafe n v o)

    insertWithKeyUnsafe :: Int -> Index o -> IxValue o -> o -> o
    insertWithKeyUnsafe n k v o = let o' = prependWithKeyUnsafe k v o in moveHeadUnsafe n o'

    insertWithKey :: Int -> Index o -> IxValue o -> o -> Either OrderError o
    insertWithKey n _ _ o | n > size o = Left (OutOfRange n {-o-})
    insertWithKey _ k _ o | Map.member k (toMap o) = Left (DuplicateKey {-k o-})
    insertWithKey n k v o = let o' = prependWithKeyUnsafe k v o in Right (moveHeadUnsafe n o')

    -- | Put a new element at the end of the OrderedMap, returning a pair
    -- containing the new OrderedMap and the new key.
    append :: IxValue o -> o -> (o, Index o)
    append v o = insertAtUnsafe (size o) v o

    appendWithKeyUnsafe :: Index o -> IxValue o -> o -> o
    appendWithKeyUnsafe k v o = insertWithKeyUnsafe (size o) k v o

    appendWithKey :: Index o -> IxValue o -> o -> Either OrderError o
    appendWithKey k _ o | Map.member k (toMap o) = Left (DuplicateKey {-k o-})
    appendWithKey k v o = Right (appendWithKeyUnsafe k v o)

-- | Update the value of an existing item
putItem :: OrderedMap o => Index o -> IxValue o -> o -> o
putItem k a m = alter f k m
         where f Nothing = (error "putItem: bad key")
               f (Just _) = Just a

showOrder :: (OrderedMap o, Show (IxValue o), Show (Index o)) => o -> String
showOrder o = "(fromPairs (" ++ show (toPairs o) ++ "))"

-- | Replace the current ordering with the given key list, which must
-- be the same length and contain the same keys as the order.
permute :: OrderedMap o => [Index o] -> o -> Either OrderError o
permute neworder m =
    reorder $ collect $ sanitize
    where
      -- Make sure the new key order doesn't have any unknown keys
      -- sanitize :: ([k], [k]) -- (known, invalid)
      sanitize = List.partition (`Map.member` (toMap m)) neworder
      -- Collect the values that are missing from the new key order
      -- collect :: ([k], [k]) -> ([k], [k], [k]) -- (present, missing, invalid)
      collect (valid, invalid) =
          let deleted = List.filter (`List.notElem` invalid) (toKeys m) in (valid, deleted, invalid)
      -- Reorder the OrderMap according to the now safe permutation,
      -- also return the portion of the OrderMap not mentioned in the
      -- new order and the list of invalid keys.
      -- reorder :: ([k], [k], [k]) -> (OrderMap k a, OrderMap k a, [k])
      reorder (valid, _missing, invalid) =
          let (validmap, missingmap) = Map.partitionWithKey (\ k _ -> List.elem k valid) (toMap m) in
          if Map.null missingmap && null invalid
          then Right (fromMapAndList validmap valid)
          else Left InvalidPermutation

-- | Append several items
--     λ> let o = (fromPairs [(1,'z')]) in insertItems o ['a','b','c'] :: ([Int], Order Int Char)
--     ([2,3,4],fromMapListKey (fromPairs [(1,'z'),(2,'a'),(3,'b'),(4,'c')]) ([1,2,3,4]) (5))
appendItems :: OrderedMap o => o -> [IxValue o] -> ([Index o], o)
appendItems om xs =
    over _1 reverse $ foldl f ([], om) xs
    where
      f (ks, om') x = let (om'', k) = append x om' in ((k : ks), om'')

-- | Return only the values of the order, discarding the keys.
toList :: OrderedMap o => o -> [IxValue o]
toList = map snd . toPairs

-- | Build an order from a list of values, allocating new all keys.
fromElements :: OrderedMap o => [IxValue o] -> o
fromElements xs = fromPairs (zip (map toEnum [0..]) xs)

-- | Perform an operation on a of an OrderedMap's (key, value) pairs,
-- reassembling the resulting pairs into a new OrderedMap.
asList :: OrderedMap o => ([(Index o, IxValue o)] -> [(Index o, IxValue o)]) -> o -> o
asList f om = fromPairs . f . toPairs $ om

-- | Find the first value (along with the associated key) that
-- satisfies the predicate.
find :: forall o. OrderedMap o => (IxValue o -> Bool) -> o -> Maybe (Index o, IxValue o)
find p m = findWithKey (\_ v -> p v) m

findWithKey :: forall o. OrderedMap o => (Index o -> IxValue o -> Bool) -> o -> Maybe (Index o, IxValue o)
findWithKey p m =
    find' (toKeys m)
    where
      find' :: [Index o] -> Maybe (Index o, IxValue o)
      find' [] = Nothing
      find' (k : more) =
          case Map.lookup k (toMap m) of
            Nothing -> find' more
            Just x | not (p k x) -> find' more
            Just x -> Just (k, x)

keyToPos :: (Eq (Index o), OrderedMap o) => o -> Index o -> Maybe Int
keyToPos o k =
    case span (/= k) (toKeys o) of
      (_, []) -> Nothing
      (pre, _) -> Just (length pre)

-- | Build a lens to focus on the k element of an OrderedMap.
lens_omat :: OrderedMap o => Index o -> Traversal' (o) (IxValue o)
lens_omat k = lens getter setter . _Just
    where
      getter s = Map.lookup k (toMap s)
      setter s a = maybe s (\ a' -> putItem k a' s) a

{-
-- | Like view, but discards the remainder list
view' :: OrderedMap o => Index o -> o -> IxValue o
view' i m = maybe (error "OrderedMap.view'") fst (Data.OrderedMap.view i m)
-}
