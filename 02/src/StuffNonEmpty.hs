module StuffNonEmpty
  ( NonEmpty(..)
  , mapNonEmpty
  , groupNonEmpty
  , groupByNonEmpty
  , groupOnNonEmpty
  , classifyOnNonEmpty
  ) where

import Stuff (sortOn, sortBy, on, (&&&))

data NonEmpty a = a :| [a]
  deriving (Show, Eq, Ord)
infixr 4 :|

groupNonEmpty :: Eq a => [a] -> [NonEmpty a]
groupNonEmpty [] = []
groupNonEmpty (x:xs) = (x :| takeWhile (==x) xs) : groupNonEmpty (dropWhile (==x) xs)

mapNonEmpty :: (a -> b) -> NonEmpty a -> NonEmpty b
mapNonEmpty f (x :| xs) = f x :| map f xs

groupByNonEmpty :: (a -> a -> Bool) -> [a] -> [NonEmpty a]
groupByNonEmpty _ [] = []
groupByNonEmpty f (x:xs) = (x :| takeWhile (f x) xs) : groupByNonEmpty f (dropWhile (f x) xs)

groupOnNonEmpty :: Eq b => (a -> b) -> [a] -> [NonEmpty a]
groupOnNonEmpty f =  map (mapNonEmpty fst) . groupByNonEmpty (on (==) snd) . map (id &&& f)

classifyOnNonEmpty :: Ord b => (a -> b) -> [a] -> [NonEmpty a]
classifyOnNonEmpty f = groupOnNonEmpty f . sortOn f
