{-# LANGUAGE CPP, FlexibleContexts, TypeFamilies #-}
{-# OPTIONS -Wall -Wredundant-constraints #-}

module Data.Order.HasKey
  ( HasKey(KeyType, getKey)
  ) where

import Instances.TH.Lift ()
import Prelude hiding (foldMap, length, lookup, map, splitAt, zip)

class Enum (KeyType v) => HasKey v where
  type KeyType v
  getKey :: v -> KeyType v

instance Enum k => HasKey (k, v) where
  type KeyType (k, v) = k
  getKey (k, _) = k
