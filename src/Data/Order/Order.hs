{-# LANGUAGE CPP, DeriveGeneric, InstanceSigs, UndecidableInstances #-}

module Data.Order.Order
  ( MapAndVec
  ) where

-- import Control.Lens (_1, _2, over)
import Control.Lens hiding (uncons, view)
import Data.Data (Data)
import Data.EnumMap as EnumMap (EnumMap, fromList)
import qualified Data.EnumMap as EnumMap
import Data.Foldable as Foldable (Foldable(foldl, foldr))
import qualified Data.Foldable as Foldable
import qualified Data.ListLike as LL
import Data.Map.Strict as Map ((!), Map, fromList)
import qualified Data.Map.Strict as Map
import Data.Order.One hiding ((!))
import Data.SafeCopy (base, extension, Migrate(..), SafeCopy(..), safeGet, safePut)
import qualified Data.Semigroup as Sem
import Data.Serialize (Serialize(..))
import Data.Typeable (Proxy(Proxy), Typeable, typeRep)
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import GHC.Exts as GHC (IsList(..))
import GHC.Generics (Generic)
import Text.PrettyPrint.HughesPJClass (Pretty(pPrint), text)
import Test.QuickCheck

-- | The primary instance of 'Ordered', the 'MapAndVec' type is
-- similar to an association list, a combination of a 'Vector' and a
-- 'Map'.
--
-- 'MapAndVec' has two notions of an 'Index', the 'Int' index of the
-- list and the @k@ index of the map.  Here is the 'Ixed' instance
-- for the latter, which only needs to address the '_theMap' field.
--
-- @
-- λ> set (ix 'a') 30 (fromPairsUnsafe [('a',10),('b',20)])
-- fromPairsUnsafe [('a',30),('b',20)] :: MapAndVec (Char) (Integer)
-- @
--
-- 'MapAndVec' has a fairly large number of other handy instances, for
-- example:
--
-- 'Foldable', which folds over the values only
--
-- @
-- λ> foldMap id (fromList [(1, "a"), (2, "b")])
-- "ab"
-- @
--
-- 'FoldableWithIndex', which folds over keys and values
--
-- @
-- λ> ifoldMap (\\k v-> show k ++ v) (fromList [(2, "a"), (5, "b")])
-- "2a5b"
-- λ> ifoldMap (\\k v-> ListLike.concat (ListLike.replicate k v :: [String])) (fromPairsSafe (ListLike.fromList [(2, "a"), (5, "b")]))
-- "aabbbbb"
-- @
--
-- 'Traversable':
--
-- @
-- λ> traverse (++ "!") (Data.Order.fromPairsUnsafe [('a', "1"),('b',"2")] :: Order Char String)
-- [fromList [('a','1'),('b','2')], fromList [('a','1'),('b','!')],fromList [('a','!'),('b','2')],fromList [('a','!'),('b','!')]] :: [Order Char Char]
-- @
--
-- 'At':
--
-- @
-- λ> set (at 'a') Nothing (fromPairs [('a',10),('b',20)])
-- fromPairs [('b',20)] :: MapAndVec (Char) (Integer)
-- λ> set (at 'b') (Just 40) (fromPairs [('a',10),('b',20)])
-- fromPairs [('a',10),('b',40)] :: MapAndVec (Char) (Integer)
-- @
--
-- New elements appear at the end (positions of existing elements do
-- not change):
--
-- @
-- λ> set (at 'x') (Just 30) (fromPairs [('a',10),('b',20)])
-- fromPairs [('a',10),('b',20),('x',30)] :: MapAndVec (Char) (Integer)
-- @
data MapAndVec k v =
  MapAndVec
    { _theMap :: Map k v
    , _theVec :: Vector k
    } deriving (Generic, Data, Typeable, Functor, Read)

instance (Ord k, Enum k, SafeCopy k, SafeCopy v) => SafeCopy (MapAndVec k v) where version = 4; kind = extension

instance Ord k => Sem.Semigroup (MapAndVec k v) where
    (<>) a b =
      -- If b contains keys already in a they must be removed.
      -- Arguably, the values of matching keys are supposed to match.
      -- This Semigroup is not really that great, I should probably
      -- remove it, or create some newtypes for different
      -- interpretations.
      let v = _theVec a <> Vector.filter (\x -> Map.notMember x (_theMap a)) (_theVec b)
          m = Map.union (_theMap b) (_theMap a) in -- prefer the values in b
      MapAndVec m v
    -- ^ If there are any common @k@ values in the shared
    -- map the elements from the second is omitted.  For
    -- this reason it is suggested that, when in doubt,
    -- the @k@ type be mapped to @Either k k@:
    -- @@
    --   mapKeys Left a <> mapKeys Right b
    -- @@

instance (Ord k) => Monoid (MapAndVec k v) where
    mempty = MapAndVec mempty mempty
#if !(MIN_VERSION_base(4,11,0))
    mappend = (<>)
#endif

data MapAndVec_3 k v = MapAndVec_3 (EnumMap k v) (Vector k) deriving (Generic)
instance (Ord k, SafeCopy k, SafeCopy v) => SafeCopy (MapAndVec_3 k v) where version = 3; kind = base
instance (Ord k, Enum k, SafeCopy k, SafeCopy v) => Migrate (MapAndVec k v) where
  type MigrateFrom (MapAndVec k v) = MapAndVec_3 k v
  migrate (MapAndVec_3 mp vec) = MapAndVec (Map.fromList (EnumMap.toList mp)) vec

instance (Ord k, Monoid (MapAndVec k v)) => LL.FoldableLL (MapAndVec k v) (k, v) where
    foldl f r0 xs = Foldable.foldl (\r k -> f r (k, _theMap xs ! k)) r0 (_theVec xs)
    foldr f r0 xs = Foldable.foldr (\k r -> f (k, _theMap xs ! k) r) r0 (_theVec xs)

instance SafeCopy (MapAndVec k v) => Serialize (MapAndVec k v) where
    put = safePut
    get = safeGet

instance forall k v. (Ord k, Show k, Show v, Typeable k, Typeable v) => Show (MapAndVec k v) where
  show o = "fromPairs " <> show (pairs o) <> " :: Order (" <> show (typeRep (Proxy :: Proxy k)) <> ") (" <> show (typeRep (Proxy :: Proxy v)) <> ")"

instance forall k v. (Ord k, Pretty k, Pretty v, Typeable k, Typeable v) => Pretty (MapAndVec k v) where
  pPrint o = text "Order " <> pPrint (pairs o)

-- Fold over the values only
-- @@
-- λ> foldMap id (fromList [(1, "a"), (2, "b")])
-- "ab"
-- @@
instance Ord k => Foldable (MapAndVec k) where
    foldMap f o = Foldable.foldMap (\k -> f (_theMap o ! k)) (_theVec o)
    null o = null (_theVec o)
    length o = length (_theVec o)

instance Ord k => FoldableWithIndex k (MapAndVec k) where
    ifoldMap f o = Foldable.foldMap (\k -> f k (_theMap o ! k)) (_theVec o)

instance FunctorWithIndex k (MapAndVec k) where
    imap f o = MapAndVec (Map.mapWithKey f (_theMap o)) (_theVec o)

fromPairsUnsafe :: forall t k a. (Ord k, Foldable t) => t (k, a) -> MapAndVec k a
fromPairsUnsafe prs =
  foldr (\(k, a) (MapAndVec m v) -> MapAndVec (Map.insert k a m) (Vector.cons k v)) mempty prs
{-# DEPRECATED fromPairsUnsafe "Use fromPairs" #-}

instance (Ord k, Eq v) => Eq (MapAndVec k v) where
  a == b = _theMap a == _theMap b && _theVec a == _theVec b

instance (Ord k, Eq v, Ord v) => Ord (MapAndVec k v) where
    compare a b = compare (_theVec a) (_theVec b) <> compare (_theMap a) (_theMap b)

instance (Ord k) => Traversable (MapAndVec k) where
    traverse f o = MapAndVec <$> traverse f (_theMap o) <*> pure (_theVec o)

instance (Ord k) => TraversableWithIndex k (MapAndVec k) where
    itraverse f o = MapAndVec <$> itraverse (\k a -> f k a) (_theMap o) <*> pure (_theVec o)

type instance Index (MapAndVec k a) = k
type instance IxValue (MapAndVec k a) = a
instance Ord k => Ixed (MapAndVec k a) where
    ix k f o =
        case Map.lookup k (_theMap o) of
          Just a -> fmap (\a' -> MapAndVec (Map.insert k a' (_theMap o)) (_theVec o)) (f a)
          Nothing -> pure o

instance Ord k => At (MapAndVec k a) where
    at k f o =
        case Map.lookup k (_theMap o) of
          Just a ->
              fmap (maybe (MapAndVec (Map.delete k (_theMap o)) (Vector.filter (/= k) (_theVec o)))
                          (\a' -> MapAndVec (Map.insert k a' (_theMap o)) (_theVec o)))
                   (f (Just a))
          Nothing ->
              fmap (maybe o
                          (\a' -> MapAndVec (Map.insert k a' (_theMap o)) (Vector.singleton k <> _theVec o)))
                   (f Nothing)

instance Ord k => One (MapAndVec k v) where
  type OneItem (MapAndVec k v) = (k, v)
  one (k, v) = fromPairsUnsafe @[] [(k, v)]

instance (Eq k, Ord k) => Ordered (MapAndVec k) k v where
  -- Override methods that could benefit from the At instance
  delete k o = set (at k) Nothing o
  -- we should also do good uncons, splitAt, and break implementations here
  fromPairs prs =
    MapAndVec (Map.fromList prs) (GHC.fromList (fmap fst prs))

instance (Ord k, {-Show k,-} Arbitrary k, Arbitrary v) => Arbitrary (MapAndVec k v) where
  arbitrary = do
      (ks :: [k]) <- (sized pure >>= \n -> vectorOf n arbitrary) >>= shuffle
      let ks' = LL.nub ks
      (vs :: [v]) <- vector (LL.length ks')
      return (fromPairs (LL.zip ks' vs :: [(k, v)]) :: MapAndVec k v)

newtype LL a = LL {unLL :: a} deriving (Eq, Ord)

instance Monoid a => Monoid (LL a) where
  mempty = LL mempty
#if !(MIN_VERSION_base(4,11,0))
  mappend = (<>)
#endif

instance Sem.Semigroup a => Sem.Semigroup (LL a) where
  (<>) (LL a) (LL b) = LL (a <> b)

-- | A ListLike instance hidden inside the newtype LL.
instance (Ord k, LL.FoldableLL (LL (MapAndVec k v)) (k, v)) => LL.ListLike (LL (MapAndVec k v)) (k, v) where
  singleton :: (k, v) -> LL (MapAndVec k v)
  singleton = LL . one
  null :: LL (MapAndVec k v) -> Bool
  null = Foldable.null . unLL
  uncons :: LL (MapAndVec k v) -> Maybe ((k, v), LL (MapAndVec k v))
  uncons (LL o) = over (_Just . _2) LL (uncons o)

  drop :: Int -> LL (MapAndVec k a) -> LL (MapAndVec k a)
  drop n = LL . Data.Order.One.drop n . unLL

  take :: Int -> LL (MapAndVec k a) -> LL (MapAndVec k a)
  take n = LL . Data.Order.One.take n . unLL

  splitAt :: Int -> LL (MapAndVec k a) -> (LL (MapAndVec k a), LL (MapAndVec k a))
  splitAt n (LL o) = over _1 LL $ over _2 LL $ Data.Order.One.splitAt n o

instance (Ord k) => GHC.IsList (LL (MapAndVec k v)) where
  type Item (LL (MapAndVec k v)) = (k, v)
  fromList = LL . fromPairsUnsafe
  toList = toPairList . unLL

instance (Ord k, Monoid (MapAndVec k v)) => LL.FoldableLL (LL (MapAndVec k v)) (k, v) where
    foldl f r0 (LL xs) = Foldable.foldl (\r k -> f r (k, _theMap xs ! k)) r0 (_theVec xs)
    foldr f r0 (LL xs) = Foldable.foldr (\k r -> f (k, _theMap xs ! k) r) r0 (_theVec xs)
