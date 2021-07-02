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
  ( module Data.Order.Order
  , module Data.Order.One
  , module Data.Order.AssocList
  , Order
  , tests
  ) where

import qualified Data.Foldable as Foldable
import Data.Int
import Data.Monoid
import Data.Order.AssocList
import Data.Order.One
import Data.Order.Order
import Extra.QuickCheck
import Prelude hiding (break, drop, filter, foldMap, length, lookup, map, take, zip)
import Test.QuickCheck

type Order k = MapAndVec k

type Key = Integer

data InsertPosition k v = InsertPosition (Order k v) Int deriving Show

data ElementPosition o k v = ElementPosition (o v) (Maybe Int) deriving Show

-- Quickcheck

-- Build an arbitrary order and a valid insert position for that order
instance (Ord k, Arbitrary k, Arbitrary v) => Arbitrary (InsertPosition k v) where
  arbitrary = do
      o <- arbitrary :: Gen (Order k v)
      InsertPosition o <$> choose (0, Foldable.length o)

instance (Ordered o k v, Arbitrary (o v), Arbitrary k, Arbitrary v) => Arbitrary (ElementPosition o k v) where
  arbitrary = do
      o <- arbitrary :: Gen (o v)
      case Foldable.length o of
        0 -> return $ ElementPosition o Nothing
        n -> ElementPosition o <$> (Just <$> choose (0, pred n))

tests :: IO Result
tests = do
  mconcat <$> sequence
    [ quickCheckResult $ withMaxSuccess 100 (prop_keys @(Order Key) @String)
    , quickCheckResult $ withMaxSuccess 100 (prop_keys @(AssocList Key) @String)
    , quickCheckResult $ withMaxSuccess 100 (prop_lookup @(Order Key) @String)
    , quickCheckResult $ withMaxSuccess 100 (prop_lookupKey @(Order Key) @String)
    , quickCheckResult $ withMaxSuccess 100 (prop_lookupPair @(Order Key) @String)
    , quickCheckResult $ withMaxSuccess 100 (prop_splitAt @(Order Key) @String)
    , quickCheckResult $ withMaxSuccess 100 (prop_next @(Order Key) @String)
    , quickCheckResult $ withMaxSuccess 100 (prop_uncons @(Order Key) @String)
    , quickCheckResult $ withMaxSuccess 100 (prop_null @(Order Key) @String)
    , quickCheckResult $ withMaxSuccess 100 (prop_singleton @(Order Key) @String)
    , quickCheckResult $ withMaxSuccess 100 (prop_toPairs_fromPairs @(Order Key) @String)
    , quickCheckResult $ withMaxSuccess 100 (prop_delete @(Order Key) @String)
    , quickCheckResult $ withMaxSuccess 100 (prop_insertAt @(Order Key) @String)
    , quickCheckResult $ withMaxSuccess 100 (prop_insert_delete @(Order Key) @String)
    , quickCheckResult $ withMaxSuccess 100 (prop_insert_delete_pos @(Order Key) @String)
    , quickCheckResult $ withMaxSuccess 100 (prop_fromPairs @(Order Key) @String)
    , quickCheckResult $ withMaxSuccess 100 (prop_fromPairs @(AssocList Key) @String)
    , quickCheckResult $ withMaxSuccess 100 (prop_pos_insertAt @(Order Key) @())
    ] >>= throwResult
