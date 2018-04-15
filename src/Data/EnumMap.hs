{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, ScopedTypeVariables #-}
{-# OPTIONS -Wall -fno-warn-name-shadowing #-}

module Data.EnumMap where

import Control.Lens (_1, _Just, over)
import qualified Data.IntMap as IntMap
import Data.IntMap (IntMap)
import Data.IntSet (IntSet)

type EnumMap k a = IntMap a

fold :: (a -> b -> b) -> b -> EnumMap k a -> b
fold = IntMap.fold

foldWithKey :: forall k a b. Enum k => (k -> a -> b -> b) -> b -> EnumMap k a -> b
foldWithKey f b0 m = IntMap.foldWithKey (\k a b -> f (toEnum k :: k) a b) b0 m

insertWith' :: Enum k => (a -> a -> a) -> k -> a -> EnumMap k a -> EnumMap k a
insertWith' f k a m = IntMap.insertWith' f (fromEnum k) a m

insertWithKey' :: Enum k => (k -> a -> a -> a) -> k -> a -> EnumMap k a -> EnumMap k a
insertWithKey' f k a m = IntMap.insertWithKey' (\k a1 a2 -> f (toEnum k) a1 a2) (fromEnum k) a m

(!) :: Enum k => EnumMap k a -> k -> a
(!) m k = (IntMap.!) m (fromEnum k)

(\\) :: EnumMap k a -> IntMap b -> EnumMap k a
(\\) = (IntMap.\\)

adjust :: Enum k => (a -> a) -> k -> EnumMap k a -> EnumMap k a
adjust f k m = IntMap.adjust f (fromEnum k) m

adjustWithKey :: Enum k => (k -> a -> a) -> k -> EnumMap k a -> EnumMap k a
adjustWithKey f k m = IntMap.adjustWithKey (\k a -> f (toEnum k) a) (fromEnum k) m

alter :: Enum k => (Maybe a -> Maybe a) -> k -> EnumMap k a -> EnumMap k a
alter f k m = IntMap.alter f (fromEnum k) m

assocs :: Enum k => EnumMap k a -> [(k, a)]
assocs m = fmap (over _1 toEnum) (IntMap.assocs m)

delete :: Enum k => k -> EnumMap k a -> EnumMap k a
delete k m = IntMap.delete (fromEnum k) m

deleteFindMax :: Enum k => EnumMap k a -> ((k, a), EnumMap k a)
deleteFindMax m = over (_1 . _1) toEnum (IntMap.deleteFindMax m)

deleteFindMin :: Enum k => EnumMap k a -> ((k, a), EnumMap k a)
deleteFindMin m = over (_1 . _1) toEnum (IntMap.deleteFindMin m)

deleteMax :: EnumMap k a -> EnumMap k a
deleteMax m = IntMap.deleteMax m

deleteMin :: EnumMap k a -> EnumMap k a
deleteMin m = IntMap.deleteMin m

difference :: EnumMap k a -> EnumMap k b -> EnumMap k a
difference = IntMap.difference

differenceWith :: (a -> b -> Maybe a) -> EnumMap k a -> EnumMap k b -> EnumMap k a
differenceWith = IntMap.differenceWith

differenceWithKey :: Enum k => (k -> a -> b -> Maybe a) -> EnumMap k a -> EnumMap k b -> EnumMap k a
differenceWithKey f m1 m2 = IntMap.differenceWithKey (\k a b -> f (toEnum k) a b) m1 m2

elems :: EnumMap k a -> [a]
elems = IntMap.elems

empty :: EnumMap k a
empty = IntMap.empty

filter :: (a -> Bool) -> EnumMap k a -> EnumMap k a
filter = IntMap.filter

filterWithKey :: Enum k => (k -> a -> Bool) -> EnumMap k a -> EnumMap k a
filterWithKey f m = IntMap.filterWithKey (\k a -> f (toEnum k) a) m

findMax :: Enum k => EnumMap k a -> (k, a)
findMax m = over _1 toEnum (IntMap.findMax m)

findMin :: Enum k => EnumMap k a -> (k, a)
findMin m = over _1 toEnum (IntMap.findMin m)

findWithDefault :: Enum k => a -> k -> EnumMap k a -> a
findWithDefault a k m = IntMap.findWithDefault a (fromEnum k) m

foldMapWithKey :: Enum k => Monoid m => (k -> a -> m) -> EnumMap k a -> m
foldMapWithKey f m = IntMap.foldMapWithKey (\k a -> f (toEnum k) a) m

foldl :: (a -> b -> a) -> a -> EnumMap k b -> a
foldl = IntMap.foldl

foldl' :: (a -> b -> a) -> a -> EnumMap k b -> a
foldl' = IntMap.foldl'

foldlWithKey :: Enum k => (a -> k -> b -> a) -> a -> EnumMap k b -> a
foldlWithKey f a m = IntMap.foldlWithKey (\a k b -> f a (toEnum k) b) a m

foldlWithKey' :: Enum k => (a -> k -> b -> a) -> a -> EnumMap k b -> a
foldlWithKey' f a m = IntMap.foldlWithKey' (\a k b -> f a (toEnum k) b) a m

foldr :: (a -> b -> b) -> b -> EnumMap k a -> b
foldr = IntMap.foldr

foldr' :: (a -> b -> b) -> b -> EnumMap k a -> b
foldr' = IntMap.foldr'

foldrWithKey :: Enum k => (k -> a -> b -> b) -> b -> EnumMap k a -> b
foldrWithKey f b m = IntMap.foldrWithKey (\k a b -> f (toEnum k) a b) b m

foldrWithKey' :: Enum k => (k -> a -> b -> b) -> b -> EnumMap k a -> b
foldrWithKey' f b m = IntMap.foldrWithKey' (\k a b -> f (toEnum k) a b) b m

fromAscList :: Enum k => [(k, a)] -> EnumMap k a
fromAscList = IntMap.fromAscList . fmap (over _1 fromEnum)

fromAscListWith :: Enum k => (a -> a -> a) -> [(k, a)] -> EnumMap k a
fromAscListWith f = IntMap.fromAscListWith f . fmap (over _1 fromEnum)

fromAscListWithKey :: Enum k => (k -> a -> a -> a) -> [(k, a)] -> EnumMap k a
fromAscListWithKey f = IntMap.fromAscListWithKey (\k a1 a2 -> f (toEnum k) a1 a2) . fmap (over _1 fromEnum)

fromDistinctAscList :: Enum k => [(k, a)] -> EnumMap k a
fromDistinctAscList = IntMap.fromDistinctAscList . fmap (over _1 fromEnum)

fromList :: Enum k => [(k, a)] -> EnumMap k a
fromList = IntMap.fromList . fmap (over _1 fromEnum)

fromListWith :: Enum k => (a -> a -> a) -> [(k, a)] -> EnumMap k a
fromListWith f = IntMap.fromListWith f . fmap (over _1 fromEnum)

fromListWithKey :: Enum k => (k -> a -> a -> a) -> [(k, a)] -> EnumMap k a
fromListWithKey f = IntMap.fromListWithKey (\k a1 a2 -> f (toEnum k) a1 a2) . fmap (over _1 fromEnum)

fromSet :: Enum k => (k -> a) -> IntSet -> EnumMap k a
fromSet f s = IntMap.fromSet (f . toEnum) s

insert :: Enum k => k -> a -> EnumMap k a -> EnumMap k a
insert k a m = IntMap.insert (fromEnum k) a m

insertLookupWithKey :: Enum k => (k -> a -> a -> a) -> k -> a -> EnumMap k a -> (Maybe a, EnumMap k a)
insertLookupWithKey f k a m = IntMap.insertLookupWithKey (\k a1 a2 -> f (toEnum k) a1 a2) (fromEnum k) a m

insertWith :: Enum k => (a -> a -> a) -> k -> a -> EnumMap k a -> EnumMap k a
insertWith f k a m = IntMap.insertWith f (fromEnum k) a m

insertWithKey :: Enum k => (k -> a -> a -> a) -> k -> a -> EnumMap k a -> EnumMap k a
insertWithKey f k a m = IntMap.insertWithKey (\k a1 a2 -> f (toEnum k) a1 a2) (fromEnum k) a m

intersection :: EnumMap k a -> EnumMap k b -> EnumMap k a
intersection = IntMap.intersection

intersectionWith :: (a -> b -> c) -> EnumMap k a -> EnumMap k b -> IntMap c
intersectionWith = IntMap.intersectionWith

intersectionWithKey :: Enum k => (k -> a -> b -> c) -> EnumMap k a -> EnumMap k b -> IntMap c
intersectionWithKey f = IntMap.intersectionWithKey (\k a b -> f (toEnum k) a b)

isProperSubmapOf :: Eq a => EnumMap k a -> EnumMap k a -> Bool
isProperSubmapOf = IntMap.isProperSubmapOf

isProperSubmapOfBy :: (a -> b -> Bool) -> EnumMap k a -> EnumMap k b -> Bool
isProperSubmapOfBy = IntMap.isProperSubmapOfBy

isSubmapOf :: Eq a => EnumMap k a -> EnumMap k a -> Bool
isSubmapOf = IntMap.isSubmapOf

isSubmapOfBy :: (a -> b -> Bool) -> EnumMap k a -> EnumMap k b -> Bool
isSubmapOfBy = IntMap.isSubmapOfBy

keys :: Enum k => EnumMap k a -> [k]
keys = fmap toEnum . IntMap.keys

keysSet :: EnumMap k a -> IntSet
keysSet = IntMap.keysSet

lookup :: Enum k => k -> EnumMap k a -> Maybe a
lookup k m = IntMap.lookup (fromEnum k) m

lookupGE :: Enum k => k -> EnumMap k a -> Maybe (k, a)
lookupGE k m = over (_Just . _1) toEnum (IntMap.lookupGE (fromEnum k) m)

lookupGT :: Enum k => k -> EnumMap k a -> Maybe (k, a)
lookupGT k m = over (_Just . _1) toEnum (IntMap.lookupGT (fromEnum k) m)

lookupLE :: Enum k => k -> EnumMap k a -> Maybe (k, a)
lookupLE k m = over (_Just . _1) toEnum (IntMap.lookupLE (fromEnum k) m)

lookupLT :: Enum k => k -> EnumMap k a -> Maybe (k, a)
lookupLT k m = over (_Just . _1) toEnum (IntMap.lookupLT (fromEnum k) m)

map :: (a -> b) -> EnumMap k a -> EnumMap k b
map = IntMap.map

mapAccum :: (a -> b -> (a, c)) -> a -> EnumMap k b -> (a, IntMap c)
mapAccum = IntMap.mapAccum

mapAccumRWithKey :: Enum k => (a -> k -> b -> (a, c)) -> a -> EnumMap k b -> (a, IntMap c)
mapAccumRWithKey f a m = IntMap.mapAccumRWithKey (\a k b -> f a (toEnum k) b) a m

mapAccumWithKey :: Enum k => (a -> k -> b -> (a, c)) -> a -> EnumMap k b -> (a, IntMap c)
mapAccumWithKey f a m = IntMap.mapAccumWithKey (\a k b -> f a (toEnum k) b) a m

mapEither :: (a -> Either b c) -> EnumMap k a -> (EnumMap k b, IntMap c)
mapEither = IntMap.mapEither

mapEitherWithKey :: Enum k => (k -> a -> Either b c) -> EnumMap k a -> (EnumMap k b, IntMap c)
mapEitherWithKey f m = IntMap.mapEitherWithKey (\k a -> f (toEnum k) a) m

mapKeys :: Enum k => (k -> k) -> EnumMap k a -> EnumMap k a
mapKeys f m = IntMap.mapKeys (fromEnum . f . toEnum) m

mapKeysMonotonic :: Enum k => (k -> k) -> EnumMap k a -> EnumMap k a
mapKeysMonotonic f m = IntMap.mapKeysMonotonic (fromEnum . f . toEnum) m

mapKeysWith :: Enum k => (a -> a -> a) -> (k -> k) -> EnumMap k a -> EnumMap k a
mapKeysWith f g m = IntMap.mapKeysWith f (fromEnum . g . toEnum) m

mapMaybe :: (a -> Maybe b) -> EnumMap k a -> EnumMap k b
mapMaybe = IntMap.mapMaybe

mapMaybeWithKey :: Enum k => (k -> a -> Maybe b) -> EnumMap k a -> EnumMap k b
mapMaybeWithKey f m = IntMap.mapMaybeWithKey (\k a -> f (toEnum k) a) m

mapWithKey :: Enum k => (k -> a -> b) -> EnumMap k a -> EnumMap k b
mapWithKey f m = IntMap.mapWithKey (\k a -> f (toEnum k) a) m

maxView :: EnumMap k a -> Maybe (a, EnumMap k a)
maxView = IntMap.maxView

maxViewWithKey :: Enum k => EnumMap k a -> Maybe ((k, a), EnumMap k a)
maxViewWithKey = over (_Just . _1 . _1) toEnum . IntMap.maxViewWithKey

member :: Enum k => k -> EnumMap k a -> Bool
member k m = IntMap.member (fromEnum k) m

mergeWithKey :: Enum k => (k -> a -> b -> Maybe c) -> (EnumMap k a -> IntMap c) -> (EnumMap k b -> IntMap c) -> EnumMap k a -> EnumMap k b -> IntMap c
mergeWithKey f g h m1 m2 = IntMap.mergeWithKey (\k a b -> f (toEnum k) a b) g h m1 m2

minView :: EnumMap k a -> Maybe (a, EnumMap k a)
minView = IntMap.minView

minViewWithKey :: Enum k => EnumMap k a -> Maybe ((k, a), EnumMap k a)
minViewWithKey =  over (_Just . _1 . _1) toEnum . IntMap.minViewWithKey

notMember :: Enum k => k -> EnumMap k a -> Bool
notMember k m = IntMap.notMember (fromEnum k) m

null :: EnumMap k a -> Bool
null = IntMap.null

partition :: (a -> Bool) -> EnumMap k a -> (EnumMap k a, EnumMap k a)
partition = IntMap.partition

partitionWithKey :: Enum k => (k -> a -> Bool) -> EnumMap k a -> (EnumMap k a, EnumMap k a)
partitionWithKey f m = IntMap.partitionWithKey (\k a -> f (toEnum k) a) m

showTree :: Show a => EnumMap k a -> String
showTree = IntMap.showTree

showTreeWith :: Show a => Bool -> Bool -> EnumMap k a -> String
showTreeWith = IntMap.showTreeWith

singleton :: Enum k => k -> a -> EnumMap k a
singleton k a = IntMap.singleton (fromEnum k) a

size :: EnumMap k a -> Int
size = IntMap.size

split :: Enum k => k -> EnumMap k a -> (EnumMap k a, EnumMap k a)
split k m = IntMap.split (fromEnum k) m

splitLookup :: Enum k => k -> EnumMap k a -> (EnumMap k a, Maybe a, EnumMap k a)
splitLookup k m = IntMap.splitLookup (fromEnum k) m

splitRoot :: EnumMap k a -> [EnumMap k a]
splitRoot = IntMap.splitRoot

toAscList :: Enum k => EnumMap k a -> [(k, a)]
toAscList = fmap (over _1 toEnum) . IntMap.toAscList

toDescList :: Enum k => EnumMap k a -> [(k, a)]
toDescList = fmap (over _1 toEnum) . IntMap.toDescList

toList :: Enum k => EnumMap k a -> [(k, a)]
toList = fmap (over _1 toEnum) . IntMap.toList

traverseWithKey :: (Applicative t, Enum k) => (k -> a -> t b) -> EnumMap k a -> t (EnumMap k b)
traverseWithKey f m = IntMap.traverseWithKey (\k a -> f (toEnum k) a) m

union :: EnumMap k a -> EnumMap k a -> EnumMap k a
union = IntMap.union

unionWith :: (a -> a -> a) -> EnumMap k a -> EnumMap k a -> EnumMap k a
unionWith = IntMap.unionWith

unionWithKey :: Enum k => (k -> a -> a -> a) -> EnumMap k a -> EnumMap k a -> EnumMap k a
unionWithKey f m1 m2 = IntMap.unionWithKey (\k a1 a2 -> f (toEnum k) a1 a2) m1 m2

unions :: [EnumMap k a] -> EnumMap k a
unions = IntMap.unions

unionsWith :: (a -> a -> a) -> [EnumMap k a] -> EnumMap k a
unionsWith = IntMap.unionsWith

update :: Enum k => (a -> Maybe a) -> k -> EnumMap k a -> EnumMap k a
update f k m = IntMap.update f (fromEnum k) m

updateLookupWithKey :: Enum k => (k -> a -> Maybe a) -> k -> EnumMap k a -> (Maybe a, EnumMap k a)
updateLookupWithKey f k m = IntMap.updateLookupWithKey (\k a -> f (toEnum k) a) (fromEnum k) m

updateMax :: (a -> Maybe a) -> EnumMap k a -> EnumMap k a
updateMax = IntMap.updateMax

updateMaxWithKey :: Enum k => (k -> a -> Maybe a) -> EnumMap k a -> EnumMap k a
updateMaxWithKey f m = IntMap.updateMaxWithKey (\k a -> f (toEnum k) a) m

updateMin :: (a -> Maybe a) -> EnumMap k a -> EnumMap k a
updateMin = IntMap.updateMin

updateMinWithKey :: Enum k => (k -> a -> Maybe a) -> EnumMap k a -> EnumMap k a
updateMinWithKey f m = IntMap.updateMinWithKey (\k a -> f (toEnum k) a) m

updateWithKey :: Enum k => (k -> a -> Maybe a) -> k -> EnumMap k a -> EnumMap k a
updateWithKey f k m = IntMap.updateWithKey (\k a -> f (toEnum k) a) (fromEnum k) m
