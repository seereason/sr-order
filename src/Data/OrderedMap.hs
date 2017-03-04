{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS -Wall #-}

module Data.OrderedMap
    ( OrderedMap(..)
    , putItem
    , Data.OrderedMap.view
    , showOrder
    , insertAt
    , permute
    , deleteItem
    , appendItem
    , insertItems
    , toList
    , fromList
    , asList
    , find
    , lens_omat
    , view'
    ) where

import Control.Lens (Traversal', _Just, lens)
import Data.List as List (elem, filter, partition)
import Data.Map as Map ((!), Map)
import qualified Data.Map as Map
import Prelude hiding (lookup)

class (Eq (OKey o), Ord (OKey o), Enum (OKey o)) => OrderedMap o where
    type OKey o
    type OValue o

    empty :: o
    nextKey :: o -> OKey o
    newKey :: o -> (OKey o, o)
    fromPairs :: [(OKey o, OValue o)] -> o
    toPairs :: o -> [(OKey o, OValue o)]
    fromMapAndList :: Map (OKey o) (OValue o) -> [OKey o] -> o
    fromMapListKey :: Map (OKey o) (OValue o) -> [OKey o] -> OKey o -> o
    toMap :: o -> Map (OKey o) (OValue o)
    -- toMap = Map.fromList . toPairs
    toKeys :: o -> [OKey o]
    -- toKeys = map fst . toPairs

    -- | Unsafe permute function - does not check whether first argument
    -- is a permutation of the existing keys.
    permuteUnsafe :: [OKey o] -> o -> o
    permuteUnsafe keys o = fromMapAndList (toMap o) keys

    -- | Set the value associated with a key
    alter :: (Maybe (OValue o) -> Maybe (OValue o)) -> OKey o -> o -> o
    alter f k o = fromMapAndList (Map.alter f k (toMap o)) (toKeys o)

    lookup :: OKey o -> o -> Maybe (OValue o)
    lookup k o = Map.lookup k (toMap o)
    delete :: OKey o -> o -> o
    delete k o = fromMapAndList (Map.delete k (toMap o)) (toKeys o)
    -- | Put a new element at the beginning of the order, returning a
    -- pair containing the new OrderedMap and the new key.
    prepend :: OValue o -> o -> (o, OKey o)
    prepend v o = let (k, _) = newKey o in (fromMapAndList (Map.insert k v (toMap o)) (k : toKeys o), k)
    -- | Put a new element at the end of the OrderedMap, returning a pair
    -- containing the new OrderedMap and the new key.
    append :: OValue o -> o -> (o, OKey o)
    append v o = let (k, _) = newKey o in (fromMapAndList (Map.insert k v (toMap o)) (toKeys o ++ [k]), k)
    moveHead :: Int -> o -> o
    moveHead 0 o = o
    moveHead n o =
        case toKeys o of
          [] -> o
          (k : ks) -> let (ks1, ks2) = splitAt n ks in
                      permuteUnsafe (ks1 ++ [k] ++ ks2) o

-- | Update the value of an existing item
putItem :: OrderedMap o => OKey o -> OValue o -> o -> o
putItem k a m = alter f k m
         where f Nothing = (error "putItem: bad key")
               f (Just _) = Just a

-- | Partition an OrderedMap into the element at k and the OrderedMap
-- containing the remaining elements.
view :: forall o. OrderedMap o => OKey o -> o -> Maybe (OValue o, o) -- like Data.Set.minView
view k o =
    case lookup k o of
      Just v -> Just (v, delete k o)
      Nothing -> Nothing

showOrder :: (OrderedMap o, Show (OValue o), Show (OKey o)) => o -> String
showOrder o = "(fromPairs (" ++ show (map (\k -> (k, toMap o ! k)) (toKeys o)) ++ "))"

-- | Insert an element at the given position.
insertAt :: OrderedMap o => Int -> OValue o -> o -> o
insertAt n v = moveHead n . fst . prepend v

-- | Replace the current ordering with the given key list.  The
-- result is a triple: (new, missing, invalid).  Missing pairs are
-- those not mentioned in the new list, invalid are those
-- mentioned but not present in the old OrderedMap.
permute :: OrderedMap o => [OKey o] -> o -> (o, [(OKey o, OValue o)], [OKey o])
permute neworder m =
    reorder $ collect $ sanitize
    where
      -- Make sure the new key order doesn't have any unknown keys
      -- sanitize :: ([k], [k]) -- (known, invalid)
      sanitize = List.partition (`List.elem` (toKeys m)) neworder
      -- Collect the values that are missing from the new key order
      -- collect :: ([k], [k]) -> ([k], [k], [k]) -- (present, missing, invalid)
      collect (valid, invalid) =
          let deleted = List.filter (not . (`List.elem` invalid)) (toKeys m) in (valid, deleted, invalid)
      -- Reorder the OrderMap according to the now safe permutation,
      -- also return the portion of the OrderMap not mentioned in the
      -- new order and the list of invalid keys.
      -- reorder :: ([k], [k], [k]) -> (OrderMap k a, OrderMap k a, [k])
      reorder (valid, _missing, invalid) =
          let (validmap, missingmap) = Map.partitionWithKey (\ k _ -> List.elem k valid) (toMap m) in
          (fromMapAndList validmap valid,
           (Map.toList missingmap),
           invalid)

-- | Remove the element at k if present.
deleteItem :: OrderedMap o => OKey o -> o -> o
deleteItem = delete
{-# DEPRECATED deleteItem "Renamed delete " #-}

-- | Put a new element at the end of the order, allocating a new key
-- for it.
appendItem :: OrderedMap o => OValue o -> o -> o
appendItem x = fst . append x

insertItems :: OrderedMap o => o -> [OValue o] -> ([OKey o], o)
insertItems om xs =
    foldr f ([], om) (reverse xs)
    where
      f x (ks, om') = let (om'', k) = append x om' in ((k : ks), om'')

-- | Return only the values of the order, discarding the keys.
toList :: OrderedMap o => o -> [OValue o]
toList = map snd . toPairs

-- | Build an order from a list of values, allocating new all keys.
fromList :: OrderedMap o => [OValue o] -> o
fromList xs = fromPairs (zip (map toEnum [0..]) xs)

-- | Perform an operation on a of an OrderedMap's (key, value) pairs,
-- reassembling the resulting pairs into a new OrderedMap.
asList :: OrderedMap o => ([(OKey o, OValue o)] -> [(OKey o, OValue o)]) -> o -> o
asList f om = fromPairs . f . toPairs $ om

-- | Find the first value (along with the associated key) that
-- satisfies the predicate.
find :: forall o. OrderedMap o => (OValue o -> Bool) -> o -> Maybe (OKey o, OValue o)
find p m =
    find' (toKeys m)
    where
      find' :: [OKey o] -> Maybe (OKey o, OValue o)
      find' [] = Nothing
      find' (k : more) =
          case Map.lookup k (toMap m) of
            Nothing -> find' more
            Just x | not (p x) -> find' more
            Just x -> Just (k, x)

-- | Build a lens to focus on the k element of an OrderedMap.
lens_omat :: OrderedMap o => OKey o -> Traversal' (o) (OValue o)
lens_omat k = lens getter setter . _Just
    where
      getter s = Map.lookup k (toMap s)
      setter s a = maybe s (\ a' -> putItem k a' s) a

-- | Like view, but discards the remainder list
view' :: OrderedMap o => OKey o -> o -> OValue o
view' i m = maybe (error "OrderedMap.view'") fst (Data.OrderedMap.view i m)
