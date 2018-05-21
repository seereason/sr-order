{-# LANGUAGE CPP, FlexibleInstances, ScopedTypeVariables, TemplateHaskell, TupleSections, TypeFamilies #-}

import Data.Order
import Data.Sequence as L (Seq)
import Test.HUnit (Counts(..), runTestTT, showCounts, Test(..))
import Test.QuickCheck

-- | Map and list should contain the same keys with no duplicates
prop_toPairs_fromPairs :: Order Int String -> Bool
prop_toPairs_fromPairs o =
    fromPairs (toPairs o :: Seq (Int, String)) == o

prop_delete :: Order Int String -> Property
prop_delete o | length o == 0 = property True
prop_delete o =
    forAll (choose (0, length o - 1)) $ \i ->
    length (Data.Order.deleteAt i o) == length o - 1

prop_insertAt :: (Int, String) -> Order Int String -> Property
prop_insertAt v@(k, _) o =
    forAll (choose (0, length o)) $ \i ->
    Data.Order.member k o || (length (insertAt i v o) == length o + 1)

-- | Use an explicit generator to create a valid list position.
prop_insert_delete :: (Int, String) -> Order Int String -> Property
prop_insert_delete (k, a) o =
    forAll (choose (0, length o)) $ \i ->
        Data.Order.member k o || (Data.Order.view k (insertAt i (k, a) o) == Just (i, a, o))

prop_insert_delete_pos :: (Int, String) -> Order Int String -> Property
prop_insert_delete_pos v@(k, _) o =
    forAll (choose (0, length o)) $ \i ->
        Data.Order.member k o || (Data.Order.deleteAt i (Data.Order.insertAt i v o) == o)

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
