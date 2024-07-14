{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS -Wall #-}

-- | A combination of a Map and a Sequence - that is, each element of the
-- Map has a unique sequence number between zero and the number of
-- elements in the map, and this sequence can be modified.  There is a
-- ListLike instance that treats it as a sequence of (k, a) pairs.

module Data.Order
  ( module Data.Order.Types.AssocList
  , module Data.Order.Classes.One
  , module Data.Order.Classes.Ordered
  -- , module Data.Order.MapAndList
  , module Data.Order.Instances.MapAndVector
  , tests
  ) where

import Control.Exception (throw)
import Control.Lens (over, _1)
import qualified Data.Foldable as Foldable
import Data.Int
import Data.Monoid
import Data.Order.Types.AssocList
import Data.Order.Classes.One
import Data.Order.Classes.Ordered
import Data.Order.Instances.MapAndVector
import Data.Set as Set (difference, lookupMin, Set)
import Data.Typeable (Typeable)
import Data.Vector (Vector)
import Extra.QuickCheck ({-instance Monoid Result-})
import GHC.Exts (fromList)
import Prelude hiding (break, drop, filter, foldMap, length, lookup, map, take, zip)
import System.Time.Extra
import Test.QuickCheck

type Key = Integer

data K = A | B | C | D | E | F | G deriving (Eq, Ord, Show, Enum, Bounded)
data V = P | Q | R | S | T | U | V deriving (Eq, Show, Enum, Bounded)

-- Find *any* unused constructor
instance NextKey K where
  nextKey s = case lookupMin (Set.difference ([minBound..maxBound] :: Set K) s) of
                Just a -> a
                Nothing -> error "nextKey K"

instance Arbitrary K where arbitrary = elements [minBound..maxBound]
instance Arbitrary V where arbitrary = elements [minBound..maxBound]
instance Arbitrary a => Arbitrary (Vector a) where arbitrary = fromList <$> arbitrary

data InsertPosition k v = InsertPosition (Order k v) Int deriving Show

data ElementPosition o k v = ElementPosition (o v) (Maybe Int) deriving Show

-- Quickcheck

-- Build an arbitrary order and a valid insert position for that order
instance (Ord k, Typeable k, Typeable v, Arbitrary k, Arbitrary v) => Arbitrary (InsertPosition k v) where
  arbitrary = do
      o <- arbitrary :: Gen (Order k v)
      InsertPosition o <$> choose (0, Foldable.length o)

instance (Ordered o k v, Arbitrary (o v), Arbitrary k, Arbitrary v) => Arbitrary (ElementPosition o k v) where
  arbitrary = do
      o <- arbitrary :: Gen (o v)
      case Foldable.length o of
        0 -> return $ ElementPosition o Nothing
        n -> ElementPosition o <$> (Just <$> choose (0, pred n))

tests :: IO ([Seconds], Result)
tests = do
#if __GHCJS__
  let c = 50
      m = 500
      d = 5000
#else
  let c = 1000
      m = 10000
      d = 100000
#endif
  mconcat <$> sequence
    [ quickCheckResult' $ withMaxSuccess c (prop_toPairs_fromPairs @(Order Key) @String) -- 0.452
    , quickCheckResult' $ withMaxSuccess c (prop_uncons @(Order Key) @String) -- 0.439
    , quickCheckResult' $ withMaxSuccess c (prop_splitAt @(Order Key) @String) -- 0.448
    , quickCheckResult' $ withMaxSuccess c (prop_fromPairs @(Order Key) @String) -- 0.446
    , quickCheckResult' $ withMaxSuccess c (prop_insert_delete @(Order Key) @String) -- 0.306
    , quickCheckResult' $ withMaxSuccess c (prop_insert_delete_pos @(Order Key) @String) -- 0.300
    , quickCheckResult' $ withMaxSuccess m (prop_keys @(Order Key) @String) -- 0.068
    , quickCheckResult' $ withMaxSuccess m (prop_lookup @(Order Key) @String) -- 0.076
    , quickCheckResult' $ withMaxSuccess m (prop_lookupKey @(Order Key) @String) -- 0.075
    , quickCheckResult' $ withMaxSuccess c (prop_lookupPair @(Order Key) @String) -- 0.830
    , quickCheckResult' $ withMaxSuccess m (prop_next @(Order Key) @String) -- 0.057
    , quickCheckResult' $ withMaxSuccess m (prop_null @(Order Key) @String) -- 0.053
    , quickCheckResult' $ withMaxSuccess d (prop_singleton @(Order Key) @String) -- 0.008
    , quickCheckResult' $ withMaxSuccess m (prop_delete @(Order Key) @String) -- 0.062
    , quickCheckResult' $ withMaxSuccess m (prop_insertAt @(Order Key) @String) -- 0.060
    , quickCheckResult' $ withMaxSuccess m (prop_pos_insertAt @(Order Key) @()) -- 0.062
    , quickCheckResult' $ withMaxSuccess m (prop_repair_valid @K @V) -- 0.147
    , quickCheckResult' $ withMaxSuccess m (prop_valid_norepair @K @V) -- 0.174
    ] >>= throwResult'

quickCheckResult' :: Testable prop => prop -> IO ([Seconds], Result)
quickCheckResult' prop = over _1 (: []) <$> duration (quickCheckResult prop)

throwResult' :: ([Seconds], Result) -> IO ([Seconds], Result)
throwResult' (secs, result@(Success {})) = return (secs, result)
throwResult' (_secs, result) = throw result
