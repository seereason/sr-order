{-# LANGUAGE CPP, FlexibleInstances, ScopedTypeVariables, TemplateHaskell, TupleSections, TypeFamilies #-}
{-# OPTIONS -ddump-splices #-}

import Data.List (nub, sort)
import Data.Map as Map
import Data.Order
import Data.OrderedMap as OrderedMap
import Test.QuickCheck

prop_next_exceeds_all_keys :: Order Int String -> Bool
prop_next_exceeds_all_keys o =
    case toKeys o of
      [] -> True
      l -> maximum l < nextKey o

-- | Map and list should contain the same keys with no duplicates
prop_same_keys :: Order Int String -> Bool
prop_same_keys o =
     sort (toKeys o) == nub (sort (Map.keys (toMap o)))

prop_toPairs_fromPairs :: Order Int String -> Bool
prop_toPairs_fromPairs o =
    fromPairs (toPairs o) == o

prop_delete :: Order Int String -> Property
prop_delete o | OrderedMap.size o == 0 = property True
prop_delete o =
    forAll (choose (0, OrderedMap.size o - 1)) $ \pos ->
    fmap OrderedMap.size (deleteByPos pos o) == Right (OrderedMap.size o - 1)

prop_insertAt :: String -> Order Int String -> Property
prop_insertAt v o =
    forAll (choose (0, OrderedMap.size o)) $ \pos ->
    fmap (OrderedMap.size . fst) (insertAt pos v o) == Right (OrderedMap.size o + 1)

-- | Use an explicit generator to create a valid list position.
prop_insert_delete :: String -> Order Int String -> Property
prop_insert_delete v o =
    forAll (choose (0, OrderedMap.size o)) $ \pos ->
        (let Right (o', k) = insertAt pos v o in deleteByKey k o') == Right o

prop_insert_delete_pos :: String -> Order Int String -> Property
prop_insert_delete_pos v o =
    forAll (choose (0, OrderedMap.size o)) $ \pos ->
        (let Right (o', _) = insertAt pos v o in deleteByPos pos o') == Right o

instance Arbitrary (Order Int String) where
    arbitrary = elements [ fromPairs [(1,"1"), (2, "2"), (3, "3"), (4, "4")]
                         -- , fromPairs [(1,"1"), (2, "2"), (4, "4")]
                         -- , fromPairs []
                         ]

prop_alter_test :: Order Int String -> Bool
prop_alter_test o =
    OrderedMap.alter (maybe (Just "new") (\_ -> Just "new")) 3 o == fromMapListKey (Map.fromList [(1,"1"),(2,"2"),(3,"new"),(4,"4")]) ([1,2,3,4]) (5)

return []
tests :: IO Bool
tests = $quickCheckAll

main :: IO ()
main = tests >> return ()
    -- $quickCheckAll >>= \r -> putStrLn (show r)
    -- quickCheck (prop_next_exceeds_all_keys :: Order Int String -> Bool)
    -- quickCheck (prop_insert_delete)
