module Data.Order.One
  ( One(OneItem, one)
  ) where

import Data.Set as Set (Set, singleton)

-- | Copied from relude
class One x where
  -- | Type of single element of the structure.
  type OneItem x

  -- | Create a list, map, 'T.Text', etc from a single element.
  one :: OneItem x -> x

instance One [a] where
  type OneItem [a] = a
  one = (:[])

instance One (Set a) where
  type OneItem (Set a) = a
  one = Set.singleton
