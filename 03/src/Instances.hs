{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -fplugin=HLint #-} -- run hlint on build via the hlint source plugin

module Instances where

import Prelude hiding (reverse)

import Data.Char (isSpace)
import Data.Function (on)

import Control.Applicative (liftA2)

newtype Pointwise a b = Pointwise {getPointwise :: (a, b)}
  deriving (Show, Eq)

instance (Ord a, Ord b) => Ord (Pointwise a b) where
  (<=) :: Pointwise a b -> Pointwise a b -> Bool
  Pointwise (a, b) <= Pointwise (a', b') = a <= a' &&  b <= b'

newtype Lexicographic a b = Lexicographic {getLexicographic :: (a, b)}
  deriving (Show, Eq)

-- The default instance for tuples and lists
instance (Ord a, Ord b) => Ord (Lexicographic a b) where
  (<=) :: Lexicographic a b -> Lexicographic a b -> Bool
  (Lexicographic (a, b)) <= (Lexicographic (a', b')) = a < a' || (a == a' && b <= b')

newtype Fun a b = Fun {getFun :: a -> b}

instance (Semigroup b) => Semigroup (Fun a b) where
  (<>) :: Fun a b -> Fun a b -> Fun a b
  (<>) = (Fun .) . (liftA2 (<>) `on` getFun)

instance (Monoid b) => Monoid (Fun a b) where
  mempty :: Fun a b
  mempty = Fun mempty

newtype First a = First {getFirst :: Maybe a}
  deriving (Eq, Show)

instance Semigroup (First a) where
  (<>) :: First a -> First a -> First a
  (First Nothing) <> x = x
  x <> _ = x

instance Monoid (First a) where
  mempty :: First a
  mempty = First Nothing

newtype Last a = Last {getLast :: Maybe a}
  deriving (Eq, Show)

instance Semigroup (Last a) where
  (<>) :: Last a -> Last a -> Last a
  x <> (Last Nothing) = x
  _ <> x = x

instance Monoid (Last a) where
  mempty :: Last a
  mempty = Last Nothing

newtype Pair a b = Pair {getPair :: (a, b)}
  deriving (Show, Eq)

-- The default instance for tuples
instance (Semigroup a, Semigroup b) => Semigroup (Pair a b) where
  (<>) :: Pair a b -> Pair a b -> Pair a b
  (Pair (a, b)) <> (Pair (a', b')) = Pair (a <> a', b <> b')

instance (Monoid a, Monoid b) => Monoid (Pair a b) where
  mempty :: Pair a b
  mempty = Pair (mempty, mempty)

newtype Dual a = Dual {getDual :: a}
  deriving (Show, Eq)

instance Semigroup a => Semigroup (Dual a) where
  (<>) :: Dual a -> Dual a -> Dual a
  (Dual a) <> (Dual b) = Dual (b <> a)

instance Monoid a => Monoid (Dual a) where
  mempty :: Dual a
  mempty = Dual mempty

reverse :: [a] -> [a]
--reverse' = getDual . foldr (<>) (Dual []) . map (\x -> Dual [x])
--reverse' = getDual . foldr ((<>) . (\ x -> Dual [x])) (Dual [])
reverse = getDual . foldMap (Dual . (:[]))

data Flux a = Flux
  { sides :: Maybe (a, a)
  , changes :: Int
  }
  deriving (Show, Eq)

flux :: a -> Flux a
flux x = Flux (Just (x, x)) 0

instance (Eq a) => Semigroup (Flux a) where
  (<>) :: Flux a -> Flux a -> Flux a
  f <> Flux Nothing _ = f
  (Flux Nothing _) <> f = f
  (Flux (Just (a, _)) c) <> (Flux (Just (a', b')) c') = Flux (Just (a, b')) count
    where count = if a' /= a then c' + c + 1 else c'

instance (Eq a) => Monoid (Flux a) where
  mempty :: Flux a
  mempty = Flux Nothing 0
