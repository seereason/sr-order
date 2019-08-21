{-# LANGUAGE CPP, FlexibleContexts, TypeFamilies #-}
{-# OPTIONS -Wall -Wredundant-constraints #-}

module Data.Order.IsMap
  ( IsMap(fromMap)
  ) where

import Control.Lens hiding (view)
import Data.Map as Map (Map)
import Instances.TH.Lift ()
import Prelude hiding (foldMap, length, lookup, map, splitAt, zip)

-- | Can be fully constructed from a Map
class (Ixed o, Eq (Index o)) => IsMap o where
    fromMap :: Map (Index o) (IxValue o) -> o

instance Ord k =>           IsMap   (Map k v) where fromMap = id
