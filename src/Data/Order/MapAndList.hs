{-# LANGUAGE CPP, DeriveGeneric, InstanceSigs, UndecidableInstances #-}

module Data.Order.MapAndList
  ( MapAndList(MapAndList)
  ) where

-- import Control.Lens (_1, _2, over)
import Control.Lens hiding (uncons, view)
import Data.Data (Data)
import Data.Foldable as Foldable (Foldable(foldl, foldr))
import qualified Data.Foldable as Foldable
import qualified Data.ListLike as LL
import Data.Map.Strict as Map ((!), Map)
import qualified Data.Map.Strict as Map
import Data.Order.One (One(OneItem, one))
import Data.Order.Ordered hiding ((!))
import Data.SafeCopy (base, SafeCopy(..), safeGet, safePut)
import qualified Data.Semigroup as Sem
import Data.Serialize (Serialize(..))
import qualified Data.Set as Set (member, singleton)
import Data.Typeable (Proxy(Proxy), Typeable, typeRep)
import GHC.Generics (Generic)
import Text.PrettyPrint.HughesPJClass (Pretty(pPrint), text)
import Test.QuickCheck

-- | The primary instance of 'Ordered', the 'Order' type is
-- similar to an association list, a combination of a 'Vector' and a
-- 'Map'.
--
-- 'Order' has two notions of an 'Index', the 'Int' index of the
-- list and the @k@ index of the map.  Here is the 'Ixed' instance
-- for the latter, which only needs to address the '_theMap' field.
--
-- @
-- λ> set (ix 'a') 30 (fromPairsUnsafe [('a',10),('b',20)])
-- fromPairsUnsafe [('a',30),('b',20)] :: Order (Char) (Integer)
-- @
--
-- 'Order' has a fairly large number of other handy instances, for
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
-- fromPairs [('b',20)] :: Order (Char) (Integer)
-- λ> set (at 'b') (Just 40) (fromPairs [('a',10),('b',20)])
-- fromPairs [('a',10),('b',40)] :: Order (Char) (Integer)
-- @
--
-- New elements appear at the end (positions of existing elements do
-- not change):
--
-- @
-- λ> set (at 'x') (Just 30) (fromPairs [('a',10),('b',20)])
-- fromPairs [('a',10),('b',20),('x',30)] :: Order (Char) (Integer)
-- @
data MapAndList k v =
  MapAndList
    { _theMap :: Map k v
    , _theVec :: [k]
    } deriving (Generic, Data, Typeable, Functor, Read)

instance (Ord k, SafeCopy k, SafeCopy v) => SafeCopy (MapAndList k v) where version = 4; kind = base

instance Ord k => Sem.Semigroup (MapAndList k v) where
    (<>) a b =
      -- If b contains keys already in a they must be removed.
      -- Arguably, the values of matching keys are supposed to match.
      -- This Semigroup is not really that great, I should probably
      -- remove it, or create some newtypes for different
      -- interpretations.
      let v = _theVec a <> LL.filter (\x -> Map.notMember x (_theMap a)) (_theVec b)
          m = Map.union (_theMap b) (_theMap a) in -- prefer the values in b
      MapAndList m v
    -- ^ If there are any common @k@ values in the shared
    -- map the elements from the second is omitted.  For
    -- this reason it is suggested that, when in doubt,
    -- the @k@ type be mapped to @Either k k@:
    -- @@
    --   mapKeys Left a <> mapKeys Right b
    -- @@

instance (Ord k) => Monoid (MapAndList k v) where
    mempty = MapAndList mempty mempty
#if !(MIN_VERSION_base(4,11,0))
    mappend = (<>)
#endif

instance (Ord k, Monoid (MapAndList k v)) => LL.FoldableLL (MapAndList k v) (k, v) where
    foldl f r0 xs = Foldable.foldl (\r k -> f r (k, _theMap xs ! k)) r0 (_theVec xs)
    foldr f r0 xs = Foldable.foldr (\k r -> f (k, _theMap xs ! k) r) r0 (_theVec xs)

instance SafeCopy (MapAndList k v) => Serialize (MapAndList k v) where
    put = safePut
    get = safeGet

instance forall k v. (Ord k, Show k, Show v, Typeable k, Typeable v) => Show (MapAndList k v) where
  show o = "fromPairs " <> show (pairs o) <> " :: Order (" <> show (typeRep (Proxy :: Proxy k)) <> ") (" <> show (typeRep (Proxy :: Proxy v)) <> ")"

instance forall k v. (Ord k, Pretty k, Pretty v, Typeable k, Typeable v) => Pretty (MapAndList k v) where
  pPrint o = text "Order " <> pPrint (pairs o)

-- Fold over the values only
-- @@
-- λ> foldMap id (fromList [(1, "a"), (2, "b")])
-- "ab"
-- @@
instance Ord k => Foldable (MapAndList k) where
    foldMap f o = Foldable.foldMap (\k -> f (_theMap o ! k)) (_theVec o)
    null o = null (_theVec o)
    length o = length (_theVec o)

instance Ord k => FoldableWithIndex k (MapAndList k) where
    ifoldMap f o = Foldable.foldMap (\k -> f k (_theMap o ! k)) (_theVec o)

instance FunctorWithIndex k (MapAndList k) where
    imap f o = MapAndList (Map.mapWithKey f (_theMap o)) (_theVec o)
    {-# INLINE imap #-}

-- Not seeing what's unsafe about this
fromPairsUnsafe :: forall t k a. (Ord k, Foldable t) => t (k, a) -> MapAndList k a
fromPairsUnsafe prs =
  foldr (\(k, a) (MapAndList m v) -> MapAndList (Map.insert k a m) (LL.cons k v)) mempty prs
{-# DEPRECATED fromPairsUnsafe "Use fromPairs" #-}

instance (Ord k, Eq v) => Eq (MapAndList k v) where
  a == b = _theMap a == _theMap b && _theVec a == _theVec b

instance (Ord k, Eq v, Ord v) => Ord (MapAndList k v) where
    compare a b = compare (_theVec a) (_theVec b) <> compare (_theMap a) (_theMap b)

instance (Ord k) => Traversable (MapAndList k) where
    traverse f o = MapAndList <$> traverse f (_theMap o) <*> pure (_theVec o)

instance (Ord k) => TraversableWithIndex k (MapAndList k) where
    itraverse f o = MapAndList <$> itraverse (\k a -> f k a) (_theMap o) <*> pure (_theVec o)

type instance Index (MapAndList k a) = k
type instance IxValue (MapAndList k a) = a
instance Ord k => Ixed (MapAndList k a) where
    ix k f o =
        case Map.lookup k (_theMap o) of
          Just a -> fmap (\a' -> MapAndList (Map.insert k a' (_theMap o)) (_theVec o)) (f a)
          Nothing -> pure o

instance Ord k => At (MapAndList k a) where
    at k f o =
        case Map.lookup k (_theMap o) of
          Just a ->
              fmap (maybe (MapAndList (Map.delete k (_theMap o)) (LL.filter (/= k) (_theVec o)))
                          (\a' -> MapAndList (Map.insert k a' (_theMap o)) (_theVec o)))
                   (f (Just a))
          Nothing ->
              fmap (maybe o
                          (\a' -> MapAndList (Map.insert k a' (_theMap o)) (LL.singleton k <> _theVec o)))
                   (f Nothing)

instance Ord k => One (MapAndList k v) where
  type OneItem (MapAndList k v) = (k, v)
  one (k, v) = fromPairsUnsafe @[] [(k, v)]

instance (Eq k, Ord k) => Ordered (MapAndList k) k v where
  -- Override methods that could benefit from the At instance
  delete k o = set (at k) Nothing o
  -- we should also do good uncons, splitAt, and break implementations here
  {-# INLINE delete #-}
  uncons (MapAndList mp ks) =
    case LL.uncons ks of
      Nothing -> Nothing
      Just (k, ks') ->
        case Map.lookup k mp of
          Nothing -> Nothing -- error
          Just v -> Just ((k, v), MapAndList (Map.delete k mp) ks')
  {-# INLINE uncons #-}
  splitAt i (MapAndList mp ks) =
    (MapAndList mp1 ks1, MapAndList mp2 ks2)
    where
      (ks1, ks2) = LL.splitAt i ks
      kset = foldMap Set.singleton ks1
      (mp1, mp2) = Map.partitionWithKey (\k _ -> Set.member k kset) mp
  {-# INLINE splitAt #-}
  fromPairs prs =
    -- MapAndList (Map.fromList prs) (GHC.fromList (fmap fst prs))
    foldr (\(k, a) (MapAndList m v) -> MapAndList (Map.insert k a m) (LL.cons k v)) mempty prs
  {-# INLINE fromPairs #-}
  pos k (MapAndList _ v) = LL.findIndex (== k) v
  {-# INLINE pos #-}

instance (Ord k, {-Show k,-} Arbitrary k, Arbitrary v) => Arbitrary (MapAndList k v) where
  arbitrary = do
      (ks :: [k]) <- (sized pure >>= \n -> vectorOf n arbitrary) >>= shuffle
      let ks' = LL.nub ks
      (vs :: [v]) <- vector (LL.length ks')
      return (fromPairs (LL.zip ks' vs :: [(k, v)]) :: MapAndList k v)
