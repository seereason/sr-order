-- | The type 'Order' is an instance of 'Ordered' consisting of a
-- 'Map' and a 'Vector'.

{-# LANGUAGE CPP, DeriveGeneric, InstanceSigs, OverloadedLabels, OverloadedLists, UndecidableInstances #-}

module Data.Order.Instances.MapAndVector where

-- import Control.Lens (_1, _2, over)
import Control.Lens hiding (uncons)
import Data.Data (Data)
import Data.Foldable as Foldable (Foldable(foldl, foldr))
import qualified Data.Foldable as Foldable
import Data.Generics.Labels ()
import qualified Data.ListLike as LL
import Data.Map.Strict as Map (Map, {-filterWithKey,-} fromSet, mapKeys, member)
import qualified Data.Map.Strict as Map
import Data.Maybe (isJust)
import Data.Order.Classes.One (One(OneItem, one))
import Data.Order.Classes.Ordered
import Data.SafeCopy (base, extension, Migrate(..), SafeCopy(..), safeGet, safePut)
import qualified Data.Semigroup as Sem
import Data.Serialize (Serialize(..))
import Data.Set as Set (difference, member, notMember, Set, singleton)
import Data.Typeable (Proxy(Proxy), Typeable, typeOf, typeRep)
import Data.Vector as Vector (Vector, splitAt {-uncons appears after 0.12.0-}, (!?))
import qualified Data.Vector as Vector
import Debug.Trace (trace)
import GHC.Exts (fromList, IsList, Item, toList)
import GHC.Generics (Generic)
import Test.QuickCheck
import Text.PrettyPrint.HughesPJClass (Pretty(pPrint), text)
-- import Test.QuickCheck

-- | The primary instance of 'Ordered', the 'Order' type is
-- similar to an association list, a combination of a 'Vector' and a
-- 'Map'.
--
-- 'Order' has two notions of an 'Index', the 'Int' index of the
-- list and the @k@ index of the map.  Here is the 'Ixed' instance
-- for the latter, which only needs to address the '_theMap' field.
--
-- @
-- λ> set (ix 'a') 30 (fromPairs [('a',10),('b',20)])
-- fromPairs [('a',30),('b',20)] :: Order (Char) (Integer)
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
-- λ> traverse (++ "!") (Data.Order.fromPairs [('a', "1"),('b',"2")] :: Order Char String)
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
data Order_4 k v =
  Order_4
    { _theMap_4 :: Map k v
    , _theVec_4 :: Vector k
    } deriving (Generic, Data, Typeable, Functor, Read)

instance (Ord k, SafeCopy k, SafeCopy v) => SafeCopy (Order_4 k v) where version = 4; kind = base
instance (Ord k, Eq k, SafeCopy k, SafeCopy v) => Migrate (Order_5 k v) where
  type MigrateFrom (Order_5 k v) = Order_4 k v
  migrate (Order_4 m v) = Order_5 m (LL.nub v)

data Order_5 k v =
  Order_5
    { _theMap_5 :: Map k v
    , _theVec_5 :: Vector k
    } deriving (Generic, Data, Typeable, Functor, Read)

instance (Ord k, SafeCopy k, SafeCopy v) => SafeCopy (Order_5 k v) where version = 5; kind = extension
instance (Ord k, Eq k, SafeCopy k, SafeCopy v) => Migrate (Order k v) where
  type MigrateFrom (Order k v) = Order_5 k v
  migrate (Order_5 m v) =
    let (m', v') = repair' (m, v) in
      if (Map.keysSet m', v') /= (Map.keysSet m, v)
      then trace ("Order repaired: " <> show (orderSchema (Order m v)) <> " -> " <> show (orderSchema (Order m' v')) <> " :: " <> show (typeOf (Order m' v'))) (Order m' v')
      else Order m' v'

instance (Ord k, SafeCopy k, SafeCopy v) => SafeCopy (Order k v) where version = 6; kind = extension

data Order k v =
  Order
    { _theMap :: Map k v
    , _theVec :: Vector k
    } deriving (Generic, Data, Typeable, Functor, Read)

instance Ord k => Sem.Semigroup (Order k v) where
    (<>) a b =
      -- If b contains keys already in a they must be removed.
      -- Arguably, the values of matching keys are supposed to match.
      -- This Semigroup is not really that great, I should probably
      -- remove it, or create some newtypes for different
      -- interpretations.
      let v = _theVec a <> Vector.filter (\x -> Map.notMember x (_theMap a)) (_theVec b)
          m = Map.union (_theMap a) (_theMap b) in -- prefer the values in a
      Order m v
    -- ^ If there are any common @k@ values in the shared
    -- map the elements from the second is omitted.  For
    -- this reason it is suggested that, when in doubt,
    -- the @k@ type be mapped to @Either k k@:
    -- @@
    --   mapKeys Left a <> mapKeys Right b
    -- @@

instance (Ord k) => Monoid (Order k v) where
    mempty = Order mempty mempty
#if !(MIN_VERSION_base(4,11,0))
    mappend = (<>)
#endif

repair' :: forall k v. Ord k => (Map k v, Vector k) -> (Map k v, Vector k)
repair' = fixDuplicatesInVec . fixMissingFromVec . fixMissingFromMap

-- Any keys present in m but missing from v are added to the end of v.
fixMissingFromVec :: forall k v. Ord k => (Map k v, Vector k) -> (Map k v, Vector k)
fixMissingFromVec (m, v) =
  let missing = convertList (Set.difference (Map.keysSet m) (convertList v)) in
    (m, v <> missing)
-- Any keys present in v but missing from m are removed.
fixMissingFromMap :: forall k v. Ord k => (Map k v, Vector k) -> (Map k v, Vector k)
fixMissingFromMap (m, v) =
  let missing = Set.difference (convertList v) (Map.keysSet m) in
    (m, Vector.filter (`Set.notMember` missing) v)
-- Last step - remove any duplicate keys in v.
fixDuplicatesInVec :: forall k v. Ord k => (Map k v, Vector k) -> (Map k v, Vector k)
fixDuplicatesInVec (m, v) =
  let (goodkeys, _) = foldl collect (mempty, mempty) v
      collect :: (Vector k, [k]) -> k -> (Vector k, [k])
      collect (s, d) k =
        if Vector.elem k s
        then (s, d <> [k])
        else (s <> [k], d) in
    (m, goodkeys)

partitionDuplicates :: Eq k => Vector k -> (Vector k, Vector k)
partitionDuplicates v =
  foldr (\k (a, b) ->
          if Vector.elem k a
          then (a, Vector.snoc b k)
          else (Vector.snoc a k, b)) (mempty, mempty) v

orderSchema :: forall k v. Ord k => Order k v -> Order Int Bool
orderSchema (Order m v) =
  Order m''' v'
  where
    m' :: Map k (Maybe v)
    m' = fmap Just m <> fromSet (const Nothing) (convertList v)
    ks :: [k]
    ks = Map.keys m'
    vs :: [Maybe v]
    vs = fmap (\k -> Map.findWithDefault Nothing k m') ks
    km :: Map k Int
    km = Map.fromList (zip ks [1..])
    v' :: Vector Int
    v' = fmap (\k -> case Map.lookup k km of
                       Nothing -> error "orderSchema"
                       Just n -> n) v
    m'' :: Map Int (Maybe v)
    m'' = mapKeys (\k -> case Map.lookup k km of
                           Nothing -> error "orderSchema"
                           Just n -> n) m'
    m''' :: Map Int Bool
    m''' = fmap isJust m''

instance (Ord k, Monoid (Order k v)) => LL.FoldableLL (Order k v) (k, v) where
  foldl f r0 o = Foldable.foldl g r0 (_theVec o)
    where g r k = maybe {-(error msg)-} r (\v -> f r (k, v)) (Map.lookup k (_theMap o))
          -- msg = "Internal Order error in FoldableLL.foldl, orderSchema=" <> show (orderSchema o)
  foldr f r0 o = Foldable.foldr g r0 (_theVec o)
    where g k r = maybe {-(error msg)-} r (\v -> f (k, v) r) (Map.lookup k (_theMap o))
          -- msg = "Internal Order error in FoldableLL.foldr, orderSchema=" <> show (orderSchema o)

instance SafeCopy (Order k v) => Serialize (Order k v) where
    put = safePut
    get = safeGet

instance forall k v. (Ord k, Show k, Show v, Typeable k, Typeable v) => Show (Order k v) where
  showsPrec d o  = showParen (d > 10) $
    showString "fromPairs " .
    shows (pairs o) .
    showString " :: Order (" .
    shows (typeRep (Proxy :: Proxy k)) .
    showString ") (" .
    shows (typeRep (Proxy :: Proxy v)) .
    showString ")"

instance forall k v. (Ord k, Pretty k, Pretty v, Typeable k, Typeable v) => Pretty (Order k v) where
  pPrint o = text "Order " <> pPrint (pairs o)

-- Fold over the values only
-- @@
-- λ> foldMap id (fromList [(1, "a"), (2, "b")])
-- "ab"
-- @@
instance (Ord k{-, Typeable k-}) => Foldable (Order k) where
  foldMap f o = Foldable.foldMap g (_theVec o)
    where g k = maybe {-(error msg)-} mempty f (Map.lookup k (_theMap o))
          -- msg = "Internal Order error in Foldable.foldMap, orderSchema=" <> show (orderSchema o)
  null o = null (_theVec o)
  length o = length (_theVec o)

-- Used by itraverse, pairs, toMap, etc.
instance (Ord k{-, Typeable k-}) => FoldableWithIndex k (Order k) where
  ifoldMap f o = Foldable.foldMap g (_theVec o)
    where g k = maybe {-(error msg)-}mempty (f k) (Map.lookup k (_theMap o))
          -- msg = "Internal Order error in FoldableWithIndex.ifoldMap, orderSchema=" <> show (orderSchema o)

instance FunctorWithIndex k (Order k) where
    imap f o = Order (Map.mapWithKey f (_theMap o)) (_theVec o)
    {-# INLINABLE imap #-}

instance (Ord k, Eq v) => Eq (Order k v) where
  a == b = _theMap a == _theMap b && _theVec a == _theVec b

instance (Ord k, Eq v, Ord v) => Ord (Order k v) where
    compare a b = compare (_theVec a) (_theVec b) <> compare (_theMap a) (_theMap b)

instance (Ord k{-, Typeable k-}) => Traversable (Order k) where
    traverse f o = Order <$> traverse f (_theMap o) <*> pure (_theVec o)

instance (Ord k{-, Typeable k-}) => TraversableWithIndex k (Order k) where
    itraverse f o = Order <$> itraverse (\k a -> f k a) (_theMap o) <*> pure (_theVec o)

type instance Index (Order k a) = k
type instance IxValue (Order k a) = a
instance Ord k => Ixed (Order k a) where
    ix k f o =
        case Map.lookup k (_theMap o) of
          Just a -> fmap (\a' -> Order (Map.insert k a' (_theMap o)) (_theVec o)) (f a)
          Nothing -> pure o

newtype Appending a = Appending {_unAppending :: a} deriving (Generic, Typeable, Show)
newtype Prepending a = Prepending {_unPrepending :: a} deriving (Generic, Typeable, Show)

type instance Index (Appending o) = Index o
type instance IxValue (Appending o) = IxValue o
instance Ixed (Order k v) => Ixed (Appending (Order k v)) where
  ix k = #_unAppending . ix k

type instance Index (Prepending o) = Index o
type instance IxValue (Prepending o) = IxValue o
instance Ixed (Order k v) => Ixed (Prepending (Order k v)) where
  ix k = #_unPrepending . ix k

atOrderPrepend :: (Ord k, Functor f) => k -> (Maybe v -> f (Maybe v)) -> Order k v -> f (Order k v)
atOrderPrepend k f o =
        case Map.lookup k (_theMap o) of
          Just a ->
              fmap (maybe (Order (Map.delete k (_theMap o)) (Vector.filter (/= k) (_theVec o)))
                          (\a' -> Order (Map.insert k a' (_theMap o)) (_theVec o)))
                   (f (Just a))
          Nothing ->
              fmap (maybe o
                          (\a' -> Order (Map.insert k a' (_theMap o)) (Vector.singleton k <> _theVec o)))
                   (f Nothing)

atOrderAppend :: (Ord k, Functor f) => k -> (Maybe v -> f (Maybe v)) -> Order k v -> f (Order k v)
atOrderAppend k f o =
        case Map.lookup k (_theMap o) of
          Just a ->
              fmap (maybe (Order (Map.delete k (_theMap o)) (Vector.filter (/= k) (_theVec o)))
                          (\a' -> Order (Map.insert k a' (_theMap o)) (_theVec o)))
                   (f (Just a))
          Nothing ->
              fmap (maybe o
                          (\a' -> Order (Map.insert k a' (_theMap o)) (_theVec o <> Vector.singleton k)))
                   (f Nothing)

-- Safer not to have this, but it would require a lot of changes
instance Ord k => At (Order k a) where
  at k = \f o -> atOrderAppend k f o

instance Ord k => At (Prepending (Order k a)) where
  at k = \f (Prepending o) -> Prepending <$> atOrderPrepend k f o

instance Ord k => At (Appending (Order k a)) where
  at k = \f (Appending o) -> Appending <$> atOrderAppend k f o

instance (Ord k, Typeable k, Typeable v) => One (Order k v) where
  type OneItem (Order k v) = (k, v)
  one (k, v) = fromPairs [(k, v)]

instance (Ord k, Typeable k, Typeable v) => IsList (Order k v) where
  type Item (Order k v) = (k, v)
  -- fromList :: [(k, v)] -> Order k v
  fromList = fromPairs
  -- toList :: Order k v -> [(k, v)]
  toList o = pairs o

vectorUncons :: Vector a -> Maybe (a, Vector a)
vectorUncons ks =
  case ks !? 0 of
    Nothing -> Nothing
    Just k -> Just (k, Vector.tail ks)
{-# INLINABLE vectorUncons #-}

instance (Eq k, Ord k, Typeable k, Typeable v) => Ordered (Order k) k v where
  -- Override methods that could benefit from the At instance.
  delete :: k -> Order k v -> Order k v
  delete k o =
    -- set (at k) Nothing o
    case view ((#_theMap :: Lens' (Order k v) (Map k v)) . at k) o of
      Nothing -> o
      Just (_ :: v) -> over #_theMap (Map.delete k) $ over #_theVec (deleteFirst (/= k)) o
  -- we should also do good uncons, splitAt, and break implementations here
  {-# INLINABLE delete #-}
  uncons (Order mp ks) =
    case vectorUncons ks of
      Nothing -> Nothing
      Just (k, ks') ->
        case Map.lookup k mp of
          Nothing -> Nothing -- error
          Just v -> Just ((k, v), Order (Map.delete k mp) ks')
  {-# INLINABLE uncons #-}
  splitAt i (Order mp ks) =
    (Order mp1 ks1, Order mp2 ks2)
    where
      (ks1, ks2) = Vector.splitAt i ks
      kset = foldMap Set.singleton ks1
      (mp1, mp2) = Map.partitionWithKey (\k _ -> Set.member k kset) mp
  {-# INLINABLE splitAt #-}
  fromPairs prs =
    -- Make sure that no duplicate keys make it into theVec.
    foldr (\(k, a) (Order m v) ->
                      case Map.member k m of
                        True -> Order m v
                        False -> Order (Map.insert k a m) (Vector.cons k v)) mempty prs
  {-# INLINABLE fromPairs #-}
  pos k (Order _ v) = Vector.findIndex (== k) v
  {-# INLINABLE pos #-}

  -- | Three inconsistencies may arise in this type: (1) duplicate
  -- keys in v, (2) keys in v missing from m, (3) keys in m missing
  -- from v.
  valid (Order m v) =
    Map.keysSet m == (convertList v :: Set k) &&
    Vector.null (snd (partitionDuplicates v))
  repair (Order m v) =
    uncurry Order $ repair' (m, v)

instance (Ord k, Typeable k, Typeable v, Arbitrary k, Arbitrary v) => Arbitrary (Order k v) where
  arbitrary = do
      (ks :: [k]) <- (sized pure >>= \n -> vectorOf n arbitrary) >>= shuffle
      let ks' = LL.nub ks
      (vs :: [v]) <- vector (LL.length ks')
      return (fromPairs (LL.zip ks' vs :: [(k, v)]) :: Order k v)

-- | Make sure repairing an arbitrary Map and Vector results in a valid Order.
prop_repair_valid :: forall (k :: *) (v :: *). (Ord k, Typeable k, Typeable v) => (Map k v, Vector k) -> Bool
prop_repair_valid (m, v) = valid (uncurry Order (repair' (m, v)))

-- | Valid orders must be unchanged by repair, invalid orders must be changed.
prop_valid_norepair :: forall (k :: *) (v :: *). (Ord k, Eq v, Typeable k, Typeable v) => (Map k v, Vector k) -> Bool
prop_valid_norepair (m, v) =
  case valid (Order m v) of
    True -> repair' (m, v) == (m, v)
    False -> repair' (m, v) /= (m, v)

-- | Drop the first element that satisfies the predicate
deleteFirst :: (a -> Bool) -> Vector a -> Vector a
deleteFirst p v =
  let (a, b) = Vector.break (not . p) v in
    a <> Vector.drop 1 b

-- | Convert between 'IsList' instances.  (Move into sr-utils.)
convertList :: (IsList a, IsList b, Item a ~ Item b) => a -> b
convertList = fromList . toList
