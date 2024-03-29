{-# LANGUAGE DeriveGeneric, InstanceSigs, UndecidableInstances #-}

module Data.Order.Type
  ( unsafeOrder
  , toPairList
  , toPairs
  , fromPairsUnsafe
  , fromPairsSafe
  , overPairs
  , ioverPairs
  , next
  ) where

import Control.Lens (_1, _2, over)
import Data.Data (Data)
import Data.Foldable as Foldable (Foldable(foldl, foldr))
import Data.ListLike as ListLike (ListLike(..))
import qualified Data.ListLike as LL
import Data.Order.Instances.MapAndVector
import Data.SafeCopy (base, contain, SafeCopy(..), safeGet, safePut)
import qualified Data.Semigroup as Sem
import qualified Data.Sequence as Seq
import Data.Typeable (Typeable)
import Data.UList
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import GHC.Exts as GHC (IsList(..))
import GHC.Generics (Generic)

unsafeOrder :: Map k v -> Vector k -> Order k v
unsafeOrder = Order

-- Conversions

toPairs :: forall k a. Order k a -> Vector (k, a)
toPairs o = fmap (\k -> (k, _map o ! k)) (_vec o :: Vector k)

fromPairsSafe :: forall t k a. (Ord k, Foldable t) => t (k, a) -> Order k a
fromPairsSafe pairs =
  foldr (\(k, a) (Order m v) -> if Map.member k m then Order m v else Order (Map.insert k a m) (Vector.cons k v)) mempty pairs

fromPairs :: forall t k a. (Ord k, Foldable t) => t (k, a) -> Order k a
fromPairs = fromPairsSafe

fromPairsUnsafe :: forall t k a. (Ord k, Foldable t) => t (k, a) -> Order k a
fromPairsUnsafe pairs =
  foldr (\(k, a) (Order m v) -> Order (Map.insert k a m) (Vector.cons k v)) mempty pairs

overPairs :: ((k, v) -> (k', v')) -> Order k v -> Order k' v'
overPairs f = fromPairsUnsafe . fmap f . toPairs

ioverPairs :: (Int -> (k, v) -> (k', v')) -> Order k v -> Order k' v'
ioverPairs f = fromPairsUnsafe . fmap (uncurry f) . Vector.zipWith (,) (Vector.fromList [0..] :: Vector Int) . toPairs
