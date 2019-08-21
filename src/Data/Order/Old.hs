{-# LANGUAGE CPP, DeriveGeneric, TypeFamilies #-}
{-# OPTIONS -Wall -Wredundant-constraints #-}

module Data.Order.Old
  ( Order_1(Order_1)
  ) where

import Control.Lens hiding (view)
import Data.EnumMap as EnumMap (EnumMap)
import Data.SafeCopy (base, contain, SafeCopy(..), safeGet, safePut)
import Data.Sequence (Seq)
import Data.Typeable (Typeable)
import GHC.Exts as GHC
import GHC.Generics (Generic)
import Instances.TH.Lift ()
import Prelude hiding (foldMap, length, lookup, map, splitAt, zip)

data Order_1 k v =
  Order_1
    { _map_1 :: EnumMap k v
    , _vec_1 :: Seq k
    } deriving Generic

instance (Ord k, Enum k) => IsList (Order_1 k v) where
  type Item (Order_1 k v) = (k, v)
  fromList _pairs = error "instance IsList (Order_1 k v)"
  toList = error "instance IsList (Order_1 k v)"

type instance Index (Order_1 k v) = k
type instance IxValue (Order_1 k v) = v

#if 0
-- Could not deduce (Ord k) arising from a use of ‘extension’
$(deriveSafeCopy 1 'Base ''Order_1)
#else
instance (Ord k, Enum k, SafeCopy k, Typeable k, SafeCopy a, Typeable a) => SafeCopy (Order_1 k a) where
    putCopy (Order_1 m v) =
        contain $ do safePut m
                     safePut v
    getCopy =
        contain $ do m <- safeGet
                     v <- safeGet
                     return $ Order_1 m v
    version = 1
    kind = base
    errorTypeName _ = "Order_1"
#endif
