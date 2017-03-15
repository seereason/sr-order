-- | The prototypical instance of OrderedMap, a data structure that
-- combines a 'Map' @k@ @v@ with a list of @k@, representing the
-- element order.  This means the @[k]@ can be reordered without
-- invalidating any @k@ values that might be in use.  This is useful
-- for collaborative systems where one person might reorder a list
-- while another person is modifying one of its elements.

{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveDataTypeable #-}
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

module Data.Order
    ( Order(..), elemsL, orderL, nextL
    , omapPaths
    , Path_OMap(Path_OMap, Path_At)
#if !__GHCJS__
    , deriveOrder
#endif
    , module OrderedMap
    ) where

import Control.Lens (Traversal', _Just, lens, Lens', makeLensesFor, view)
import Data.Aeson (ToJSON(toJSON), FromJSON(parseJSON))
import Data.Data (Data)
import Data.List as List (elem, foldl, foldl', foldr, filter, partition)
import qualified Data.ListLike as LL
import Data.Map as Map ((!), Map)
import qualified Data.Map as Map
import Data.OrderedMap as OrderedMap
import Data.Proxy (Proxy(Proxy))
import Data.SafeCopy (SafeCopy(..), base, contain, deriveSafeCopy, safeGet, safePut)
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
                        Property, property, quickCheckAll, shuffle, sublistOf)
import Web.Routes.TH (derivePathInfo)

{-
bang :: (Eq k, Ord k, Show k, Show v) => String -> Map k v -> k -> v
bang s mp k = case Map.lookup k mp of
             Nothing -> error $ s ++ " " ++ show mp ++ " ! " ++ show k
             Just v -> v
-}

data Order k v =
    Order { elems :: Map k v
          -- ^ Return the key-value map
          , order :: [k]
          -- ^ Return the list of keys in order.
          , next :: k
          -- ^ Next available key
          }
    deriving (Data, Typeable, Generic)

instance (Enum k, Ord k) => OrderedMap (Order k v) where
    type OKey (Order k v) = k
    type OValue (Order k v) = v
    empty = Order mempty mempty (toEnum 0)
    nextKey = next
    toMap = elems
    toKeys = order
    newKey o = (next o, o {next = succ (next o)})
    fromMapAndList mp ks = Order { elems = mp, order = ks, next = succ (maximum ks)}
    fromMapListKey mp ks k = Order { elems = mp, order = ks, next = k}
    fromPairs = foldr (\(k, v) o -> o {elems = Map.insert k v (elems o),
                                       order = k : order o,
                                       next = max (succ k) (next o)}) empty
    toPairs o = let mp = toMap o in map (\k -> (k, mp ! k)) (toKeys o)

instance (Ord k, Enum k, ToJSON k, Data v, ToJSON v) => ToJSON (Order k v) where
  toJSON = toJSON . toPairs

instance (Ord k, Enum k, FromJSON k, Data v, FromJSON v) => FromJSON (Order k v) where
  parseJSON = fmap fromPairs . parseJSON

instance (Ord k, Enum k, Show k, Show v) => Show (Order k v) where
    show o = "fromMapKeyList (" ++ show (toMap o) ++ ") (" ++ show (toKeys o) ++ ") (" ++ show (nextKey o) ++ ")"
    -- show o = "(fromPairs (" ++ show (toPairs o) ++ "))"

instance (Ord k, Enum k) => Monoid (Order k v) where
    mempty = empty
    mappend a b = foldr (\ x m -> fst (append x m)) a (toList b)

-- Not sure how correct these three instances are in the presence of
-- randomly allocated keys and the like.
instance (Ord k, Enum k, Eq v) => Eq (Order k v) where
    a == b = toList a == toList b

instance (Ord k, Enum k, Eq v, Ord v) => Ord (Order k v) where
    compare a b = compare (toList a) (toList b)

instance (Ord k, Enum k, Read v) => Read (Order k v) where
    -- readsPrec :: Int -> String -> [(OrderMap k a, String)]
    readsPrec _ s = let l = (read s :: [v]) in [(OrderedMap.fromList l, "")]

instance (Ord k, Enum k, Monoid (Order k v)) => LL.ListLike (Order k v) v where
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

instance (Ord k, Enum k, Monoid (Order k v)) => LL.FoldableLL (Order k v) v where
    foldl f r0 xs = List.foldl f r0 (toList xs)
    foldr f r0 xs = List.foldr f r0 (toList xs)

instance forall k v. (Enum k, Ord k, Arbitrary k, Arbitrary v) => Arbitrary (Order k v) where
    arbitrary =
        fromPairs <$>
          (zip
             <$> ((sublistOf (fmap toEnum [0..20]) :: Gen [k]) >>= shuffle)
             <*> infiniteListOf (arbitrary :: Gen v))

$(makeLensesFor [("elems", "elemsL"), ("order", "orderL"), ("next", "nextL")] ''Order)

data Path_OMap k a = Path_OMap | Path_At k a deriving (Eq, Ord, Read, Show, Typeable, Data, Generic, FromJSON, ToJSON)

omapPaths :: (Ord k, Enum k) => Lens' a (Order k v) -> a -> [(k, c -> Path_OMap k c)]
omapPaths lns a = map (\ (k, _) -> (k, Path_At k)) (toPairs (Control.Lens.view lns a))

#if !__GHCJS__
-- | Given the name of a type such as AbbrevPair, generate declarations
-- @@
--     newtype AbbrevPairID = AbbrevPairID {unAbbrevPairID :: IntJS} deriving (Eq, Ord, Read, Show, Data, Typeable)
--     type AbbrevPairs = Order AbbrevPairID AbbrevPair
--     instance Enum AbbrevPairID where
--       toEnum = AbbrevPairID . toEnum
--       fromEnum = fromEnum . unAbbrevPairID
-- @@
deriveOrder :: TypeQ -> Name -> [Name] -> Q [Dec]
deriveOrder ityp t supers = do
  let idname = mkName (nameBase t ++ "ID")
      unname = mkName ("un" ++ nameBase t ++ "ID")
      mpname = mkName (nameBase t ++ "s")
#if MIN_VERSION_template_haskell(2,11,0)
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
  omtype <- tySynD mpname [] [t|Order $(conT idname) $(conT t)|]
  return $ [idtype, omtype] ++ insts

instance (Ord k, Enum k, SafeCopy k, SafeCopy a) => SafeCopy (Order k a) where
    putCopy m = contain $ do safePut (elems m)
                             safePut (order m)
                             safePut (next m)
    getCopy = contain $ do elems_ <- safeGet
                           order_ <- safeGet
                           next_ <- safeGet
                           return $ Order {elems = elems_, order = order_, next = next_}

$(derivePathInfo ''Path_OMap)
$(deriveSafeCopy 0 'base ''Path_OMap)
$(deriveLiftMany [''Order])
#endif
