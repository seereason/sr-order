module Data.Order.Classes.One
  ( One(OneItem, one)
  , tellone
  ) where

import Control.Monad.Writer (MonadWriter, tell)
import Data.Map as Map (Map, singleton)
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

instance One (Map k v) where
  type OneItem (Map k v) = (k, v)
  one = uncurry Map.singleton

tellone :: (MonadWriter w m, One w) => OneItem w -> m ()
tellone = tell . one
