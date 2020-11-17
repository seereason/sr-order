{-# LANGUAGE DeriveGeneric, InstanceSigs, UndecidableInstances #-}

module Data.Order.Order
  ( Order(Order, _map, _vec)
  , toPairs
  , fromPairsUnsafe
  ) where

-- import Control.Lens (_1, _2, over)
import Control.Lens hiding (uncons, view)
import Data.Data (Data)
import Data.EnumMap as EnumMap ((!), EnumMap)
import qualified Data.EnumMap as EnumMap
import Data.Foldable as Foldable (Foldable(foldl, foldr))
import qualified Data.Foldable as Foldable
import qualified Data.ListLike as LL
import Data.Order.One
import Data.SafeCopy (base, contain, extension, Migrate(..), SafeCopy(..), safeGet, safePut)
import qualified Data.Semigroup as Sem
import qualified Data.Sequence as Seq
import Data.Serialize (Serialize(..))
import Data.Typeable (Proxy(Proxy), Typeable, typeRep)
import Data.UList
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import GHC.Exts as GHC (IsList(..))
import GHC.Generics (Generic)
import Text.PrettyPrint.HughesPJClass (Pretty(pPrint), text)

data Order k v =
  Order
    { _map :: EnumMap k v
    , _vec :: Vector k
    } deriving (Generic, Data, Typeable, Functor, Read)

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
    mempty = Order mempty mempty
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

instance (Enum k, Ord k, Monoid (Order k v)) => LL.FoldableLL (Order k v) (k, v) where
    foldl f r0 xs = Foldable.foldl (\r k -> f r (k, _map xs ! k)) r0 (_vec xs)
    foldr f r0 xs = Foldable.foldr (\k r -> f (k, _map xs ! k) r) r0 (_vec xs)

instance SafeCopy (Order k v) => Serialize (Order k v) where
    put = safePut
    get = safeGet

instance forall k v. (Ord k, Enum k, Show k, Show v, Typeable k, Typeable v) => Show (Order k v) where
  show o = "fromPairs " <> show (toPairs o) <> " :: Order (" <> show (typeRep (Proxy :: Proxy k)) <> ") (" <> show (typeRep (Proxy :: Proxy v)) <> ")"

instance forall k v. (Ord k, Enum k, Pretty k, Pretty v, Typeable k, Typeable v) => Pretty (Order k v) where
  pPrint o = text "Order " <> pPrint (Vector.toList (toPairs o))

-- Fold over the values only
-- @@
-- λ> foldMap id (fromList [(1, "a"), (2, "b")])
-- "ab"
-- @@
instance (Enum k, Ord k) => Foldable (Order k) where
    foldMap f o = Foldable.foldMap (\k -> f (_map o ! k)) (_vec o)
    null o = null (_vec o)
    length o = length (_vec o)

-- Fold over keys and values
-- @@
-- λ> ifoldMap (\k v-> show k ++ v) (fromList [(2, "a"), (5, "b")])
-- "2a5b"
-- @@
instance (Enum k, Ord k) => FoldableWithIndex k (Order k) where
    ifoldMap f o = Foldable.foldMap (\k -> f k (_map o ! k)) (_vec o)

-- @@
-- λ> ifoldMap (\k v-> ListLike.concat (ListLike.replicate k v :: [String])) (fromPairsSafe (ListLike.fromList [(2, "a"), (5, "b")]))
-- "aabbbbb"
-- @@
instance Enum k => FunctorWithIndex k (Order k) where
    imap f o = Order (EnumMap.mapWithKey f (_map o)) (_vec o)

toPairs :: forall k a. Enum k => Order k a -> Vector (k, a)
toPairs o = fmap (\k -> (k, _map o ! k)) (_vec o :: Vector k)
{-# DEPRECATED toPairs "Use pairs" #-}

fromPairsUnsafe :: forall t k a. (Ord k, Enum k, Foldable t) => t (k, a) -> Order k a
fromPairsUnsafe prs =
  foldr (\(k, a) (Order m v) -> Order (EnumMap.insert k a m) (Vector.cons k v)) mempty prs
{-# DEPRECATED fromPairsUnsafe "Use fromPairs" #-}

instance (Ord k, Eq v) => Eq (Order k v) where
  a == b = _map a == _map b && _vec a == _vec b

instance (Enum k, Ord k, Eq v, Ord v) => Ord (Order k v) where
    compare a b = compare (_vec a) (_vec b) <> compare (_map a) (_map b)

-- @@
-- λ> traverse (++ "!") (Data.Order.fromPairsUnsafe [('a', "1"),('b',"2")] :: Order Char String)
-- [fromList [('a','1'),('b','2')], fromList [('a','1'),('b','!')],fromList [('a','!'),('b','2')],fromList [('a','!'),('b','!')]] :: [Order Char Char]
-- @@
instance (Enum k, Ord k) => Traversable (Order k) where
    traverse f o = Order <$> traverse f (_map o) <*> pure (_vec o)

instance (Enum k, Ord k) => TraversableWithIndex k (Order k) where
    itraverse f o = Order <$> itraverse (\k a -> f (toEnum k) a) (_map o) <*> pure (_vec o)

-- | An Order has two notions of an 'Index', the 'Int' index of the
-- list and the @k@ index of the map.  Here is the 'Ixed' instance
-- for the latter, which only needs to address the _map field.
-- @@
-- λ> set (ix 'a') 30 (fromPairsUnsafe [('a',10),('b',20)])
-- fromPairsUnsafe [('a',30),('b',20)] :: Order (Char) (Integer)
-- @@

type instance Index (Order k a) = k
type instance IxValue (Order k a) = a
instance Enum k => Ixed (Order k a) where
    ix k f o =
        case EnumMap.lookup k (_map o) of
          Just a -> fmap (\a' -> Order (EnumMap.insert k a' (_map o)) (_vec o)) (f a)
          Nothing -> pure o

-- | 'At' instance.
-- @@
-- λ> set (at 'a') Nothing (fromPairs [('a',10),('b',20)])
-- fromPairs [('b',20)] :: Order (Char) (Integer)
-- λ> set (at 'b') (Just 40) (fromPairs [('a',10),('b',20)])
-- fromPairs [('a',10),('b',40)] :: Order (Char) (Integer)
-- @@
-- New elements appear at the end (positions of existing elements do
-- not change):
-- @@
-- λ> set (at 'x') (Just 30) (fromPairs [('a',10),('b',20)])
-- fromPairs [('a',10),('b',20),('x',30)] :: Order (Char) (Integer)
-- @@
instance (Enum k, Eq k) => At (Order k a) where
    at k f o =
        case EnumMap.lookup k (_map o) of
          Just a ->
              fmap (maybe (Order (EnumMap.delete k (_map o)) (Vector.filter (/= k) (_vec o)))
                          (\a' -> Order (EnumMap.insert k a' (_map o)) (_vec o)))
                   (f (Just a))
          Nothing ->
              fmap (maybe o
                          (\a' -> Order (EnumMap.insert k a' (_map o)) (_vec o <> Vector.singleton k)))
                   (f Nothing)

instance (Ord k, Enum k) => One (Order k v) where
  type OneItem (Order k v) = (k, v)
  one (k, v) = fromPairsUnsafe @[] [(k, v)]

instance (Eq k, Ord k, Enum k) => Ordered (Order k) k v where
  -- Override methods that could benefit from the At instance
  delete k o = set (at k) Nothing o
  -- we should also do good uncons, splitAt, and break implementations here
  fromPairs prs =
    Order (EnumMap.fromList (fmap (over _1 fromEnum) prs)) (GHC.fromList (fmap fst prs))
