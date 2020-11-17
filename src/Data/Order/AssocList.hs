-- | A simple type that satisfies 'IndexedOrder' and 'AList'

{-# LANGUAGE DeriveAnyClass, TemplateHaskell, UndecidableInstances #-}

module Data.Order.AssocList where

import Control.Lens hiding (cons, uncons)
import Data.Order.One
import Data.Set as Set (fromList)
import Data.List (sortBy)
import GHC.Generics (Generic)
import qualified Data.ListLike as LL
import Test.QuickCheck

newtype AssocList k a = AssocList {_pairs :: [(k, a)]} deriving (Generic, Show)

instance Monoid (AssocList k a) where
  mappend = (<>)
  mempty = AssocList mempty

instance Semigroup (AssocList k a) where
  AssocList a <> AssocList b = AssocList (a <> b)

type instance Index (AssocList k a) = k
type instance IxValue (AssocList k a) = a

instance Eq k => At (AssocList k a) where
  at k = iso _pairs AssocList . (lens getter setter)
    where
      getter :: [(k, a)] -> Maybe a
      getter ((k', a) : _) | k == k' = Just a
      getter (_ : more) = getter more
      getter [] = Nothing
      setter :: [(k, a)] -> Maybe a -> [(k, a)]
      setter prs ma =
        setter' [] prs
        where
          setter' :: [(k, a)] -> [(k, a)] -> [(k, a)]
          setter' r ((k', a) : more)
            | k == k' = r <> maybe [] (\a' -> [(k, a')]) ma <> more
            | otherwise = r <> [(k', a)] <> setter' r more
          setter' r [] = r <> maybe [] (\a -> [(k, a)]) ma

instance At (AssocList k a) => Ixed (AssocList k a) where

-- Note that this doesn't have access to the k values
instance Foldable (AssocList k) where
  foldMap :: Monoid m => (a -> m) -> AssocList k a -> m
  foldMap _ (AssocList []) = mempty
  foldMap f (AssocList ((_k, a) : prs)) = f a <> foldMap f (AssocList prs)

instance FoldableWithIndex k (AssocList k) where
  ifoldMap :: forall a r. Monoid r => (k -> a -> r) -> AssocList k a -> r
  ifoldMap f (AssocList o) = foldMap (uncurry f) o

deriving instance (Eq k, Eq v) => Eq (AssocList k v)
deriving instance (Ord k, Ord v) => Ord (AssocList k v)

-- | This newtype can satisfy 'At (o k)' because its 'Index' is not
-- forced to be @Index [(k, a)] ~ Int@ as a list's is.
instance One (AssocList k a) where
  type OneItem (AssocList k a) = (k, a)
  one (k, a) = AssocList [(k, a)]

instance (Eq k, Ord k) => Ordered (AssocList k) k v where
  pairs = _pairs
  fromPairs = AssocList
  -- keys - Use default implementation for correct order
  keysSet = Set.fromList . keys
  -- values - Use default implementation for correct order
  -- member - default
  -- pos - default
  -- singleton - default
  -- cons - default (assuming (a : v) is same as [a] <> v
  -- lookup - default
  -- delete - default
  -- This implementation works with infinite lists
  uncons (AssocList ((k, v) : more)) = Just ((k, v), AssocList more)
  uncons (AssocList []) = Nothing
  break f (AssocList prs) =
    let (before, after) = Prelude.break f prs in
      (AssocList before, AssocList after)
  -- takeWhile - default
  -- dropWhile - default
  -- partition - default
  -- filter -- default
  -- permute -- could benefit from a reimplementation
  splitAt n (AssocList prs) =
    let (before, after) = Prelude.splitAt n prs in
      (AssocList before, AssocList after)
  -- deleteAt - default
  -- insertPairAt - default
  -- insertPairAtWith - default
  -- take - default
  -- drop - default
  -- lookupPair - default
  sortBy cmp (AssocList prs) = AssocList (Data.List.sortBy cmp prs)
  -- next - default
  -- insertAt - default
  -- append -- default

instance (Enum k, Ord k, {-Show k,-} Arbitrary k, Arbitrary v) => Arbitrary (AssocList k v) where
  arbitrary = do
      (ks :: [k]) <- (sized pure >>= \n -> vectorOf n arbitrary) >>= shuffle
      let ks' = LL.nub ks
      (vs :: [v]) <- vector (LL.length ks')
      return (AssocList (LL.zip ks' vs))
