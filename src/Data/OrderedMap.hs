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
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS -Wall #-}

module Data.OrderedMap
    ( OrderedMap(..)
    , OrderError(..)
    , putItem
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

-- | Minimum implementation: OKey, OValue
class (Eq (OKey o), Ord (OKey o), Enum (OKey o)) => OrderedMap o where
    type OKey o
    type OValue o

    size :: o -> Int
    size = Map.size . toMap

    lookByKey :: OKey o -> o -> Maybe (OValue o)
    lookByKey k o = Map.lookup k (toMap o)
    lookByPos :: Int -> o -> Maybe (OValue o)
    lookByPos pos o = case drop pos (toKeys o) of
                          k : _ -> lookByKey k o
                          _ -> Nothing

    nextKey :: o -> OKey o
    newKey :: o -> (OKey o, o)

    empty :: o
    fromPairs :: [(OKey o, OValue o)] -> o
    -- | (unsafe)
    fromMapAndList :: Map (OKey o) (OValue o) -> [OKey o] -> o
    -- | (unsafe)
    fromMapListKey :: Map (OKey o) (OValue o) -> [OKey o] -> OKey o -> o

    toPairs :: o -> [(OKey o, OValue o)]
    toMap :: o -> Map (OKey o) (OValue o)
    -- toMap = Map.fromList . toPairs
    toKeys :: o -> [OKey o]
    -- toKeys = map fst . toPairs

    -- | Set the value associated with a key.  If the key is not
    -- present a new entry is created (where?)
    alter :: (Maybe (OValue o) -> Maybe (OValue o)) -> OKey o -> o -> o
    alter f k o = fromMapListKey (Map.alter f k (toMap o)) (toKeys o) (nextKey o)

    -- | Unsafe permute function - does not check whether first argument
    -- is a permutation of the existing keys.
    permuteUnsafe :: [OKey o] -> o -> o
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

    viewByKey :: OKey o -> o -> Either OrderError (Int, OValue o, o)
    viewByKey k o =
        -- Invariant: order keys are unique
        case break ((== k) . fst) (toPairs o) of
          (_, []) -> Left InvalidKey
          (pre, (_, x) : post) -> Right (length pre, x, fromPairs (pre ++ post))

    -- | Delete if present
    deleteByKeyMaybe :: OKey o -> o -> o
    deleteByKeyMaybe k o =
        fromMapListKey (Map.delete k (toMap o)) (filter (/= k) (toKeys o)) (nextKey o)
    deleteByKey :: OKey o -> o -> Either OrderError o
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
    prepend :: OValue o -> o -> (o, OKey o)
    prepend v o = let (k, o') = newKey o in (prependWithKeyUnsafe k v o', k)

    prependWithKey :: OKey o -> OValue o -> o -> Either OrderError o
    prependWithKey k _ o | Map.member k (toMap o) = Left (DuplicateKey {-k o-})
    prependWithKey k v o = Right (prependWithKeyUnsafe k v o)

    prependWithKeyUnsafe :: OKey o -> OValue o -> o -> o
    prependWithKeyUnsafe k _ o | Map.member k (toMap o) = error "prependWithKey"
    prependWithKeyUnsafe k v o =
        fromMapListKey (Map.insert k v (toMap o)) (k : toKeys o) (max (succ k) (nextKey o))

    -- | Insert an element at a specific position.
    insertAtUnsafe :: Int -> OValue o -> o -> (o, OKey o)
    insertAtUnsafe n v o = let (o', k) = prepend v o in (moveHeadUnsafe n o', k)

    insertAt :: Int -> OValue o -> o -> Either OrderError (o, OKey o)
    insertAt n _ o | n > size o = Left (OutOfRange n {-o-})
    insertAt n v o = Right (insertAtUnsafe n v o)

    insertWithKeyUnsafe :: Int -> OKey o -> OValue o -> o -> o
    insertWithKeyUnsafe n k v o = let o' = prependWithKeyUnsafe k v o in moveHeadUnsafe n o'

    insertWithKey :: Int -> OKey o -> OValue o -> o -> Either OrderError o
    insertWithKey n _ _ o | n > size o = Left (OutOfRange n {-o-})
    insertWithKey _ k _ o | Map.member k (toMap o) = Left (DuplicateKey {-k o-})
    insertWithKey n k v o = let o' = prependWithKeyUnsafe k v o in Right (moveHeadUnsafe n o')

    -- | Put a new element at the end of the OrderedMap, returning a pair
    -- containing the new OrderedMap and the new key.
    append :: OValue o -> o -> (o, OKey o)
    append v o = insertAtUnsafe (size o) v o

    appendWithKeyUnsafe :: OKey o -> OValue o -> o -> o
    appendWithKeyUnsafe k v o = insertWithKeyUnsafe (size o) k v o

    appendWithKey :: OKey o -> OValue o -> o -> Either OrderError o
    appendWithKey k _ o | Map.member k (toMap o) = Left (DuplicateKey {-k o-})
    appendWithKey k v o = Right (appendWithKeyUnsafe k v o)

data OrderError
    = InvalidKey
    | InvalidPermutation
    | DuplicateKey
    | OutOfRange Int
    | EmptyOrder -- ^ Expected an order with at least one element
    deriving (Eq, Ord, Show)

-- | Update the value of an existing item
putItem :: OrderedMap o => OKey o -> OValue o -> o -> o
putItem k a m = alter f k m
         where f Nothing = (error "putItem: bad key")
               f (Just _) = Just a

showOrder :: (OrderedMap o, Show (OValue o), Show (OKey o)) => o -> String
showOrder o = "(fromPairs (" ++ show (toPairs o) ++ "))"

-- | Replace the current ordering with the given key list, which must
-- be the same length and contain the same keys as the order.
permute :: OrderedMap o => [OKey o] -> o -> Either OrderError o
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
