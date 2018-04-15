-- | The prototypical instance of OrderedMap, a data structure that
-- combines a 'Map' @k@ @v@ with a list of @k@, representing the
-- element order.  This means the @[k]@ can be reordered without
-- invalidating any @k@ values that might be in use.  This is useful
-- for collaborative systems where one person might reorder a list
-- while another person is modifying one of its elements.

{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Order_0
    ( Order_0(..)
    ) where

import Control.Lens (At(..), FoldableWithIndex(ifoldMap), FunctorWithIndex(imap), Index, Ixed(..), IxValue, (<&>),
                     TraversableWithIndex(..), Traversal', _Just, lens, Lens', makeLensesFor, view)
import Data.Data (Data)
import Data.Default (Default(def))
import Data.List as List (elem, foldl, foldl', foldr, filter, partition)
import qualified Data.ListLike as LL
import Data.Map as Map ((!), Map)
import qualified Data.Map as Map
import Data.OrderedMap as OrderedMap
import Data.Proxy (Proxy(Proxy))
import Data.SafeCopy (SafeCopy(..), base, contain, deriveSafeCopy, safeGet, safePut)
import Data.Serialize (Serialize(get, put))
import Data.Set as Set (fromList)
import Data.Tree (Tree(Node))
import Data.Typeable (Typeable)
import GHC.Generics (Generic)
import Instances.TH.Lift ()
import Language.Haskell.TH
-- import Language.Haskell.TH.Path.Core (Describe(..), IsPath(..), makeCol, makeRow, makeTrees, PathStart(..), Peek(..), U(u, unU'))
-- import Language.Haskell.TH.Path.GHCJS (SafeCopy(..), base, contain, deriveSafeCopy, safeGet, safePut)
import Language.Haskell.TH.Lift (deriveLiftMany)
--import Language.Haskell.TH.TypeGraph.Prelude ({-some Lift instances?-})
import Prelude hiding (init)
import Test.QuickCheck (Arbitrary(arbitrary), choose, forAll, Gen, infiniteListOf,
                        listOf, Property, property, quickCheckAll, shuffle, sublistOf)
import Web.Routes.TH (derivePathInfo)

{-
bang :: (Eq k, Ord k, Show k, Show v) => String -> Map k v -> k -> v
bang s mp k = case Map.lookup k mp of
             Nothing -> error $ s ++ " " ++ show mp ++ " ! " ++ show k
             Just v -> v
-}

data Order_0 k v =
    Order_0
          { elems :: Map k v
          -- ^ Return the key-value map
          , order :: [k]
          -- ^ Return the list of keys in order.
          , next :: k
          -- ^ Next available key
          }
    deriving (Data, Typeable, Generic, Functor)

instance (Ixed (Order_0 k v), Enum k, Ord k) => OrderedMap (Order_0 k v) where
    empty = Order_0 mempty mempty (toEnum 0)
    nextKey = next
    toMap = elems
    toKeys = order
    newKey o = (next o, o {next = succ (next o)})
    fromMapVecKey mp ks k = Order_0 { elems = mp, order = ks, next = k}

instance (Enum k, Ord k) => Default (Order_0 k v) where
    def = empty

instance Ord k => Traversable (Order_0 k) where
    traverse f (Order_0 es ks n) = Order_0 <$> traverse f es <*> pure ks <*> pure n

instance Ord k => TraversableWithIndex k (Order_0 k) where
    itraverse f (Order_0 es ks n) = Order_0 <$> itraverse f es <*> pure ks <*> pure n

-- Make sure that Foldable.toList gives us the key order,
-- rather than the order returned by Map.toList.
instance Ord k => Foldable (Order_0 k) where
    foldMap f (Order_0 es ks n) = foldMap (\k -> f (es ! k)) ks
instance Ord k => FoldableWithIndex k (Order_0 k) where
    ifoldMap f (Order_0 es ks n) = foldMap (\k -> f k (es ! k)) ks
instance FunctorWithIndex k (Order_0 k) where
    imap f (Order_0 es ks n) = Order_0 (Map.mapWithKey f es) ks n

instance (Ord k, Enum k, Show k, Show v) => Show (Order_0 k v) where
    show o = "fromMapVecKey (" ++ show (toMap o) ++ ") (" ++ show (toKeys o) ++ ") (" ++ show (nextKey o) ++ ")"
    -- show o = "(fromPairs (" ++ show (toPairs o) ++ "))"

instance (Ord k, Enum k) => Monoid (Order_0 k v) where
    mempty = empty
    mappend a b = foldr (\ x m -> fst (append x m)) a (values b)

-- Not sure how correct these three instances are in the presence of
-- randomly allocated keys and the like.
instance (Ord k, Enum k, Eq v) => Eq (Order_0 k v) where
    a == b = values a == values b

instance (Ord k, Enum k, Eq v, Ord v) => Ord (Order_0 k v) where
    compare a b = compare (values a) (values b)

instance (Ord k, Enum k, Read v) => Read (Order_0 k v) where
    -- readsPrec :: Int -> String -> [(OrderMap k a, String)]
    readsPrec _ s = let l = (read s :: [v]) in [(OrderedMap.fromElements l, "")]

instance (Ord k, Enum k, Monoid (Order_0 k v)) => LL.ListLike (Order_0 k v) v where
    uncons m =
        case toKeys m of
          [] -> Nothing
          (hd : tl) -> Just (elems m ! hd, m {order = tl, elems = Map.delete hd (elems m), next = next m})
    null = null . order
    singleton x = fst $ append x empty
    head m = case order m of
               (hd : _) -> elems m ! hd
               _ -> error "OrderMap.head"
    tail m = case order m of
               (hd : tl) -> m {order = tl, elems = Map.delete hd (elems m), next = next m}
               _ -> error "OrderMap.tail"

instance (Ord k, Enum k, Monoid (Order_0 k v)) => LL.FoldableLL (Order_0 k v) v where
    foldl f r0 xs = List.foldl f r0 (values xs)
    foldr f r0 xs = List.foldr f r0 (values xs)

instance (Ord k, Enum k, Serialize k, Serialize e) => Serialize (Order_0 k e) where
    put o = put (toMap o, toKeys o, nextKey o)
    get = do (mp, ks, n) <- get; return $ fromMapVecKey mp ks n

instance (Enum k, Ord k, Arbitrary v, Arbitrary k) => Arbitrary (Order_0 k v) where
    arbitrary = (fromPairs . LL.fromList) <$> listOf arbitrary

$(makeLensesFor [("elems", "elemsL"), ("order", "orderL"), ("next", "nextL")] ''Order_0)

-- | Given the name of a type such as AbbrevPair, generate declarations
-- @@
--     newtype AbbrevPairID = AbbrevPairID {unAbbrevPairID :: IntJS} deriving (Eq, Ord, Read, Show, Data, Typeable)
--     type AbbrevPairs = Order_0 AbbrevPairID AbbrevPair
--     instance Enum AbbrevPairID where
--       toEnum = AbbrevPairID . toEnum
--       fromEnum = fromEnum . unAbbrevPairID
-- @@
deriveOrder :: TypeQ -> Name -> [Name] -> Q [Dec]
deriveOrder ityp t supers = do
  let idname = mkName (nameBase t ++ "ID")
      unname = mkName ("un" ++ nameBase t ++ "ID")
      mpname = mkName (nameBase t ++ "s")
#if MIN_VERSION_template_haskell(2,12,0)
  idtype <- newtypeD (cxt []) idname [] Nothing (recC idname [varStrictType unname (strictType notStrict ityp) ]) [derivClause Nothing (map conT $ [''Eq, ''Ord, ''Read, ''Show, ''Data, ''Typeable] ++ supers)]
#elif MIN_VERSION_template_haskell(2,11,0)
  idtype <- newtypeD (cxt []) idname [] Nothing (recC idname [varStrictType unname (strictType notStrict ityp) ]) (sequence $ map conT $ [''Eq, ''Ord, ''Read, ''Show, ''Data, ''Typeable] ++ supers)
#else
  idtype <- newtypeD (cxt []) idname [] (recC idname [varStrictType unname (strictType notStrict ityp) ]) ([''Eq, ''Ord, ''Read, ''Show, ''Data, ''Typeable] ++ supers)
#endif
  insts <- [d| instance Enum $(conT idname) where
                 toEnum = $(conE idname) . toEnum
                 fromEnum = fromEnum . $(varE unname) |]
  -- It would be nice to build the PathInfo instance for idname, but a
  -- call to derivePathInfo would try to reify it, and its too soon
  -- for that.
  omtype <- tySynD mpname [] [t|Order_0 $(conT idname) $(conT t)|]
  return $ [idtype, omtype] ++ insts

#if 0
$(deriveSafeCopy 0 'base ''Order_0)
#else
instance (Ord k, Enum k, SafeCopy k, SafeCopy a) => SafeCopy (Order_0 k a) where
    putCopy m = contain $ do safePut (elems m)
                             safePut (order m)
                             safePut (next m)
    getCopy = contain $ do elems_ <- safeGet
                           order_ <- safeGet
                           next_ <- safeGet
                           return $ Order_0 {elems = elems_, order = order_, next = next_}
    version = 0
    kind = base
    errorTypeName _ = "Order_0"
#endif

$(deriveLiftMany [''Order_0])

type instance IxValue (Order_0 k v) = v
type instance Index (Order_0 k v) = k

instance (Ord k, Enum k) => Ixed (Order_0 k v) where
  ix k f o =
      case Map.lookup k (toMap o) of
        Just v -> f v <&> \v' -> alter (const (Just v')) k o
        Nothing -> pure o
  {-# INLINE ix #-}

instance (Ord k, Enum k) => At (Order_0 k a) where
  at k f o = f mv <&> \r -> case r of
    Nothing -> maybe o (const (alter (const Nothing) k o)) mv
    Just  v' -> alter (const (Just v')) k o
    where mv = lookByKey k o
  {-# INLINE at #-}
