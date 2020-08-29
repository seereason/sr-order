-- | A list whose elements are always unique.  

{-# LANGUAGE DeriveAnyClass, DeriveDataTypeable, DeriveGeneric, FlexibleInstances, MultiParamTypeClasses, TypeFamilies #-}

module Data.UList
  ( UList(..)
  , umap
  ) where

import Data.Data (Data)
import Data.ListLike as LL (FoldableLL(..), ListLike(..), nub, singleton, uncons, null)
import Data.Semigroup (Semigroup(..))
import Data.Serialize (Serialize)
import Data.SafeCopy (SafeCopy)
import Data.Typeable (Typeable)
import GHC.Exts (IsList(..))
import GHC.Generics (Generic)

newtype UList a = UList {unUList :: [a]} deriving (Eq, Ord, Data, Typeable, Generic, Read, Show, Serialize)

instance (SafeCopy a, Typeable a) => SafeCopy (UList a)

instance Eq a => IsList (UList a) where
  type Item (UList a) = a
  fromList = UList {-. nub-}
  toList = unUList

instance Eq a => Semigroup (UList a) where
  UList a <> UList b = fromList (a <> b)

instance Eq a => Monoid (UList a) where
  mempty = UList mempty
  mappend = (<>)

instance Foldable UList where
    foldMap f = foldMap f . unUList

instance FoldableLL (UList a) a where
  foldl f x (UList xs) = LL.foldl f x xs
  foldr f x (UList xs) = LL.foldr f x xs

instance Eq a => ListLike (UList a) a where
  singleton = UList . singleton
  uncons (UList []) = Nothing
  uncons (UList xs) = fmap (\(x, xs') -> (x, UList xs')) (uncons xs)
  null (UList []) = True
  null (UList _) = False

umap :: Eq b => (a -> b) -> UList a -> UList b
umap f (UList xs) = fromList (fmap f xs)
