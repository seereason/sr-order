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
  ( module Data.Order.AssocList
  , module Data.Order.One
  , module Data.Order.Ordered
  -- , module Data.Order.MapAndList
  , module Data.Order.Order
  , tests
  ) where

import Control.Exception (throw)
import Control.Lens (over, _1)
import qualified Data.Foldable as Foldable
import Data.Int
import Data.Monoid
import Data.Order.AssocList
import Data.Order.One
import Data.Order.Ordered
import Data.Order.Order
-- import Data.Order.MapAndList
import Extra.QuickCheck ({-instance Monoid Result-})
import Prelude hiding (break, drop, filter, foldMap, length, lookup, map, take, zip)
import System.Time.Extra
import Test.QuickCheck

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

tests :: IO ([Seconds], Result)
tests = do
  mconcat <$> sequence
    [ quickCheckResult' $ withMaxSuccess 100 (prop_toPairs_fromPairs @(Order Key) @String)
    , quickCheckResult' $ withMaxSuccess 100 (prop_uncons @(Order Key) @String)
    , quickCheckResult' $ withMaxSuccess 100 (prop_splitAt @(Order Key) @String)
    , quickCheckResult' $ withMaxSuccess 100 (prop_fromPairs @(Order Key) @String)
    , quickCheckResult' $ withMaxSuccess 100 (prop_insert_delete @(Order Key) @String)
    , quickCheckResult' $ withMaxSuccess 100 (prop_insert_delete_pos @(Order Key) @String)
    , quickCheckResult' $ withMaxSuccess 100 (prop_keys @(Order Key) @String)
    , quickCheckResult' $ withMaxSuccess 100 (prop_lookup @(Order Key) @String)
    , quickCheckResult' $ withMaxSuccess 100 (prop_lookupKey @(Order Key) @String)
    , quickCheckResult' $ withMaxSuccess 1000 (prop_lookupPair @(Order Key) @String)
    , quickCheckResult' $ withMaxSuccess 100 (prop_next @(Order Key) @String)
    , quickCheckResult' $ withMaxSuccess 100 (prop_null @(Order Key) @String)
    , quickCheckResult' $ withMaxSuccess 100 (prop_singleton @(Order Key) @String)
    , quickCheckResult' $ withMaxSuccess 100 (prop_delete @(Order Key) @String)
    , quickCheckResult' $ withMaxSuccess 100 (prop_insertAt @(Order Key) @String)
    , quickCheckResult' $ withMaxSuccess 100 (prop_pos_insertAt @(Order Key) @())
    ] >>= throwResult'

quickCheckResult' :: Testable prop => prop -> IO ([Seconds], Result)
quickCheckResult' prop = over _1 (: []) <$> duration (quickCheckResult prop)

throwResult' :: ([Seconds], Result) -> IO ([Seconds], Result)
throwResult' (secs, result@(Success {})) = return (secs, result)
throwResult' (_secs, result) = throw result
