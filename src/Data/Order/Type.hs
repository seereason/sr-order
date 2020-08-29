{-# LANGUAGE DeriveGeneric #-}

module Data.Order.Type
  ( Order(_map, _vec)
  , unsafeOrder
  , toPairList
  , toPairs
  , fromPairs
  , overPairs
  , ioverPairs
  ) where

import Data.Data (Data)
import Data.EnumMap as EnumMap ((!), EnumMap)
import qualified Data.EnumMap as EnumMap
import qualified Data.ListLike as LL
import Data.SafeCopy (base, contain, extension, Migrate(..), SafeCopy(..), safeGet, safePut)
import qualified Data.Semigroup as Sem
import qualified Data.Sequence as Seq
import Data.Typeable (Typeable)
import Data.UList
import Data.Vector (Vector)
import qualified Data.Vector as Vector
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

fromPairs :: forall t k a. (Ord k, Enum k, Foldable t) => t (k, a) -> Order k a
fromPairs pairs =
  foldr (\(k, a) (Order m v) -> Order (EnumMap.insert k a m) (Vector.cons k v)) mempty pairs

overPairs :: (Enum k, Ord k', Enum k') => ((k, v) -> (k', v')) -> Order k v -> Order k' v'
overPairs f = fromPairs . fmap f . toPairs

ioverPairs :: (Enum k, Ord k', Enum k') => (Int -> (k, v) -> (k', v')) -> Order k v -> Order k' v'
ioverPairs f = fromPairs . fmap (uncurry f) . Vector.zipWith (,) (Vector.fromList [0..] :: Vector Int) . toPairs

#if 0
instance (Ord k, Enum k) => GHC.IsList (Order k v) where
  type Item (Order k v) = (k, v)
  fromList = fromPairs
  toList = toPairList

instance (Ord k, Enum k, FoldableLL (Order k v) (k, v)) => ListLike (Order k v) (k, v) where
  singleton :: (k, v) -> Order k v
  singleton (k, v) = unsafeOrder (EnumMap.singleton k v) [k]
  null :: Order k v -> Bool
  null o = ListLike.null (_vec o)
  uncons :: Order k v -> Maybe ((k, v), Order k v)
  uncons o =
    case ListLike.uncons (_vec o) of
      Just (k0, ks') ->
        let (mk, mks) = EnumMap.partitionWithKey (\k _ -> k == k0) (_map o) in
          Just (ListLike.head (EnumMap.toList @k mk), unsafeOrder mks ks')
      Nothing -> Nothing

  drop :: Int -> Order k a -> Order k a
  drop n o =
    let (a, b) = ListLike.splitAt n (_vec o) in
    unsafeOrder (Foldable.foldr EnumMap.delete (_map o) a) b

  take :: Int -> Order k a -> Order k a
  take n o =
    let (a, b) = ListLike.splitAt n (_vec o) in
    unsafeOrder (Foldable.foldr EnumMap.delete (_map o) b) a

  splitAt :: Int -> Order k a -> (Order k a, Order k a)
  splitAt n = over _1 fromPairs . over _2 fromPairs . ListLike.splitAt n . toPairs
#endif
