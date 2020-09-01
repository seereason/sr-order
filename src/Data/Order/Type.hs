{-# LANGUAGE DeriveGeneric, InstanceSigs, UndecidableInstances #-}

module Data.Order.Type
  ( Order(_map, _vec)
  , unsafeOrder
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
import Data.EnumMap as EnumMap ((!), EnumMap)
import qualified Data.EnumMap as EnumMap
import Data.Foldable as Foldable (Foldable(foldl, foldr))
import Data.ListLike as ListLike (ListLike(..))
import qualified Data.ListLike as LL
import Data.SafeCopy (base, contain, extension, Migrate(..), SafeCopy(..), safeGet, safePut)
import qualified Data.Semigroup as Sem
import qualified Data.Sequence as Seq
import Data.Typeable (Typeable)
import Data.UList
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import GHC.Exts as GHC (IsList(..))
import GHC.Generics (Generic)

data Order k v =
  Order
    { _map :: EnumMap k v
    , _vec :: Vector k
    } deriving (Generic, Data, Typeable, Functor, Read)

unsafeOrder :: EnumMap k v -> Vector k -> Order k v
unsafeOrder = Order

instance (Enum k, Ord k) => Sem.Semigroup (Order k v) where
    (<>) a b =
      let m = EnumMap.union (_map a) (_map b)
          v = Vector.filter (`EnumMap.member` m) (_vec a <> _vec b) in
      Order m v
    -- ^ If there are any common @k@ values in the shared
    -- map the elements from the second is omitted.  For
    -- this reason it is suggested that, when in doubt,
    -- the @k@ type be mapped to @Either k k@:
    -- @@
    --   mapKeys Left a <> mapKeys Right b
    -- @@

instance (Enum k, Ord k) => Monoid (Order k v) where
    mempty = unsafeOrder mempty mempty
#if !(MIN_VERSION_base(4,11,0))
    mappend = (<>)
#endif

-- Added 29 Aug 2020
instance (Enum k, Ord k, SafeCopy k, Typeable k, SafeCopy v, Typeable v) => Migrate (Order k v) where
  type MigrateFrom (Order k v) = Order_2 k v
  migrate (Order_2 m (UList v)) = Order m (Vector.fromList v)

instance (Ord k, Enum k, SafeCopy k, Typeable k, SafeCopy v, Typeable v) => SafeCopy (Order k v) where version = 3; kind = extension

data Order_2 k v =
  Order_2
    { _map_2 :: EnumMap k v
    , _vec_2 :: UList k
    } deriving (Generic, Data, Typeable, Functor, Read)

instance (Ord k, Enum k, SafeCopy k, Typeable k, SafeCopy v, Typeable v) => SafeCopy (Order_2 k v) where version = 2; kind = extension

instance (Ord k, SafeCopy k, Typeable k, SafeCopy v, Typeable v) => Migrate (Order_2 k v) where
  type MigrateFrom (Order_2 k v) = Order_1 k v
  migrate (Order_1 mp ks) = Order_2 mp (LL.fromListLike ks)

data Order_1 k v =
  Order_1
    { _map_1 :: EnumMap k v
    , _vec_1 :: Seq.Seq k
    } deriving (Generic, Data, Typeable, Functor, Read)

-- Don't change this, its not compatible with the generic instance.
instance (SafeCopy k, Typeable k, SafeCopy a, Typeable a) => SafeCopy (Order_1 k a) where
    putCopy (Order_1 m v) =
        contain $ do safePut m
                     safePut v
    getCopy =
        contain $ do m <- safeGet
                     v <- safeGet
                     return $ Order_1 m v
    version = 1
    kind = base
    errorTypeName _ = "Order"

-- Conversions

toPairList :: Enum k => Order k v -> [(k, v)]
toPairList = Vector.toList . toPairs

toPairs :: forall k a. Enum k => Order k a -> Vector (k, a)
toPairs o = fmap (\k -> (k, _map o ! k)) (_vec o :: Vector k)

fromPairsSafe :: forall t k a. (Ord k, Enum k, Foldable t) => t (k, a) -> Order k a
fromPairsSafe pairs =
  foldr (\(k, a) (Order m v) -> if EnumMap.member k m then Order m v else Order (EnumMap.insert k a m) (Vector.cons k v)) mempty pairs

fromPairs :: forall t k a. (Ord k, Enum k, Foldable t) => t (k, a) -> Order k a
fromPairs = fromPairsSafe

fromPairsUnsafe :: forall t k a. (Ord k, Enum k, Foldable t) => t (k, a) -> Order k a
fromPairsUnsafe pairs =
  foldr (\(k, a) (Order m v) -> Order (EnumMap.insert k a m) (Vector.cons k v)) mempty pairs

overPairs :: (Enum k, Ord k', Enum k') => ((k, v) -> (k', v')) -> Order k v -> Order k' v'
overPairs f = fromPairsUnsafe . fmap f . toPairs

ioverPairs :: (Enum k, Ord k', Enum k') => (Int -> (k, v) -> (k', v')) -> Order k v -> Order k' v'
ioverPairs f = fromPairsUnsafe . fmap (uncurry f) . Vector.zipWith (,) (Vector.fromList [0..] :: Vector Int) . toPairs

instance (Enum k, Ord k, Monoid (Order k v)) => LL.FoldableLL (Order k v) (k, v) where
    foldl f r0 xs = Foldable.foldl (\r k -> f r (k, _map xs ! k)) r0 (_vec xs)
    foldr f r0 xs = Foldable.foldr (\k r -> f (k, _map xs ! k) r) r0 (_vec xs)

-- | Return the next available key
-- @@
-- λ> next (fromPairs (Vector.fromList [(2, "a"), (5, "b")]))
-- 6
-- λ> next (fromPairs (Data.Vector.fromList []) :: Order Int String)
-- 0
-- @@
next :: Enum k => Order k a -> k
next o = LL.head (LL.dropWhile (`EnumMap.member` (_map o)) [toEnum 0 ..])
-- next (Order m _) = maybe (toEnum 0) (succ . toEnum . fst) (Set.maxView (EnumMap.keysSet m))
