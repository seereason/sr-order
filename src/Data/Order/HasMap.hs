{-# LANGUAGE CPP, FlexibleContexts, TypeFamilies #-}
{-# OPTIONS -Wall -Wredundant-constraints #-}

module Data.Order.HasMap
  ( HasMap(toMap)
  ) where

import Control.Lens hiding (view)
import Data.Map as Map (Map)
import Instances.TH.Lift ()
import Prelude hiding (foldMap, length, lookup, map, splitAt, zip)

-- | Contains at least as much information as a Map
class HasMap o where
    toMap :: o -> Map (Index o) (IxValue o)

-- Map k v instances
instance                    HasMap  (Map k v) where toMap = id
