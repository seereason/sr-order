{-# LANGUAGE DeriveGeneric #-}

module Data.Order.Type
  ( Order(_map, _vec)
  , unsafeOrder
  ) where

import Data.Data (Data)
import Data.EnumMap as EnumMap (EnumMap)
import qualified Data.ListLike as LL
import Data.SafeCopy (base, contain, extension, Migrate(..), SafeCopy(..), safeGet, safePut)
import qualified Data.Sequence as Seq
import Data.Typeable (Typeable)
import Data.UList (UList)
import GHC.Generics (Generic)

data Order k v =
  Order
    { _map :: EnumMap k v
    , _vec :: UList k
    } deriving (Generic, Data, Typeable, Functor, Read)

instance (Eq k, Ord k, Enum k, SafeCopy k, Typeable k, SafeCopy v, Typeable v) => SafeCopy (Order k v) where version = 2; kind = extension

unsafeOrder :: EnumMap k v -> UList k -> Order k v
unsafeOrder = Order

instance (Eq k, SafeCopy k, Typeable k, SafeCopy v, Typeable v) => Migrate (Order k v) where
  type MigrateFrom (Order k v) = Order_1 k v
  migrate (Order_1 mp ks) = unsafeOrder mp (LL.fromListLike ks)

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
