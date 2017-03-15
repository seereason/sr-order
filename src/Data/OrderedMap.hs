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
    , viewByKey
    , showOrder
    , permute
    , insertItems
    , toList
    , fromList
    , asList
    , find
    , lens_omat
    ) where

import Control.Lens (Traversal', _Just, lens)
import Data.List as List (elem, filter, notElem, partition)
import Data.Map as Map (Map)
import qualified Data.Map as Map

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

    size :: o -> Int
    size = Map.size . toMap

    -- | Unsafe permute function - does not check whether first argument
    -- is a permutation of the existing keys.
    permuteUnsafe :: [OKey o] -> o -> o
    permuteUnsafe keys o = fromMapListKey (toMap o) keys (nextKey o)

    -- | Set the value associated with a key
    alter :: (Maybe (OValue o) -> Maybe (OValue o)) -> OKey o -> o -> o
    alter f k o = fromMapListKey (Map.alter f k (toMap o)) (toKeys o) (nextKey o)

    lookByKey :: OKey o -> o -> Maybe (OValue o)
    lookByKey k o = Map.lookup k (toMap o)
    lookByPos :: Int -> o -> Maybe (OValue o)
    lookByPos pos o = case drop pos (toKeys o) of
                          k : _ -> lookByKey k o
                          _ -> Nothing
    deleteByKey :: OKey o -> o -> o
    deleteByKey k o =
        fromMapListKey (Map.delete k (toMap o)) (filter (/= k) (toKeys o)) (nextKey o)
    deleteByPos :: Int -> o -> o
    deleteByPos pos o = case drop pos (toKeys o) of
                          k : _ -> deleteByKey k o
                          _ -> o

    -- | Put a new element at the beginning of the order, returning a
    -- pair containing the new OrderedMap and the new key.
    prepend :: OValue o -> o -> (o, OKey o)
    prepend v o = let (k, o') = newKey o in (prependWithKey k v o', k)

    prependWithKey :: OKey o -> OValue o -> o -> o
    prependWithKey k _ o | Map.member k (toMap o) = error "prependWithKey"
    prependWithKey k v o =
        fromMapListKey (Map.insert k v (toMap o)) (k : toKeys o) (max (succ k) (nextKey o))

    -- | Move the head element to a specified position.
    moveHead :: Int -> o -> o
    moveHead 0 o = o
    moveHead n o =
        case toKeys o of
          [] -> o
          (k : ks) -> let (ks1, ks2) = splitAt n ks in
                      permuteUnsafe (ks1 ++ [k] ++ ks2) o

    -- | Insert an element at a specific position.
    insertAt :: {-OrderedMap o =>-} Int -> OValue o -> o -> (o, OKey o)
    insertAt n v o = let (o', k) = prepend v o in (moveHead n o', k)

    insertWithKey :: {-0OrderedMap o =>-} Int -> OKey o -> OValue o -> o -> o
    insertWithKey n k v o = let o' = prependWithKey k v o in moveHead n o'

    -- | Put a new element at the end of the OrderedMap, returning a pair
    -- containing the new OrderedMap and the new key.
    append :: OValue o -> o -> (o, OKey o)
    append v o = insertAt (size o) v o

    appendWithKey :: OKey o -> OValue o -> o -> o
    appendWithKey k v o = insertWithKey (size o) k v o

-- | Update the value of an existing item
putItem :: OrderedMap o => OKey o -> OValue o -> o -> o
putItem k a m = alter f k m
         where f Nothing = (error "putItem: bad key")
               f (Just _) = Just a

-- | Partition an OrderedMap into the element at k and the OrderedMap
-- containing the remaining elements.
viewByKey :: forall o. OrderedMap o => OKey o -> o -> Maybe (OValue o, o) -- like Data.Set.minView
viewByKey k o =
    case lookByKey k o of
      Just v -> Just (v, deleteByKey k o)
      Nothing -> Nothing

showOrder :: (OrderedMap o, Show (OValue o), Show (OKey o)) => o -> String
showOrder o = "(fromPairs (" ++ show (toPairs o) ++ "))"

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
          (fromMapAndList validmap valid,
           (Map.toList missingmap),
           invalid)

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

{-
-- | Like view, but discards the remainder list
view' :: OrderedMap o => OKey o -> o -> OValue o
view' i m = maybe (error "OrderedMap.view'") fst (Data.OrderedMap.view i m)
-}
