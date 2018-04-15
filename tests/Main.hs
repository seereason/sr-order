{-# LANGUAGE CPP, FlexibleInstances, ScopedTypeVariables, TemplateHaskell, TupleSections, TypeFamilies #-}

import Data.ListLike as LL hiding (length, fromList, putStrLn)
import Data.Order
import Test.HUnit (assertEqual, Counts(..), runTestTT, showCounts, Test(..))
import Test.QuickCheck

{-
prop_next_exceeds_all_keys :: Order Int String -> Bool
prop_next_exceeds_all_keys o =
    case keys o of
      [] -> True
      l -> maximum l < nextKey o
-}

-- | Map and list should contain the same keys with no duplicates
prop_toPairs_fromPairs :: Order Int String -> Bool
prop_toPairs_fromPairs o =
    fromListLike (fromListLike o :: [(Int, String)]) == o

prop_delete :: Order Int String -> Property
prop_delete o | length o == 0 = property True
prop_delete o =
    forAll (choose (0, length o - 1)) $ \pos ->
    length (Data.Order.deleteAt pos o) == length o - 1

prop_insertAt :: (Int, String) -> Order Int String -> Property
prop_insertAt v@(i, _) o =
    forAll (choose (0, length o)) $ \pos ->
    Data.Order.member i o || (length (insertAt pos v o) == length o + 1)

-- | Use an explicit generator to create a valid list position.
prop_insert_delete :: (Int, String) -> Order Int String -> Property
prop_insert_delete v@(i, _) o =
    forAll (choose (0, length o)) $ \pos ->
        Data.Order.member i o || (Data.Order.delete i (insertAt pos v o) == o)

prop_insert_delete_pos :: (Int, String) -> Order Int String -> Property
prop_insert_delete_pos v@(i, _) o =
    forAll (choose (0, length o)) $ \pos ->
        Data.Order.member i o || (Data.Order.deleteAt pos (Data.Order.insertAt pos v o) == o)

{-
prop_alter_test :: Order Int String -> Bool
prop_alter_test o =
    OrderedMap.alter (maybe (Just "new") (\_ -> Just "new")) 3 o == fromMapListKey (Map.fromList [(1,"1"),(2,"2"),(3,"new"),(4,"4")]) ([1,2,3,4]) (5)
-}

return []
tests :: IO Bool
tests = $quickCheckAll

main :: IO ()
main = do
  tests
  counts <- runTestTT $ TestList $
    []
  case counts of
    Counts {errors = 0, failures = 0} -> putStrLn "OK, passed unit tests"
    _ -> error (showCounts counts)
  return ()
    -- $quickCheckAll >>= \r -> putStrLn (show r)
    -- quickCheck (prop_next_exceeds_all_keys :: Order Int String -> Bool)
    -- quickCheck (prop_insert_delete)
