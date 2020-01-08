module Stuff
  ( group
  , sortBy
  , groupBy
  , sortOn
  , groupOn
  , classifyOn
  , (&&&)
  , on
  ) where

group :: Eq a => [a] -> [[a]]
group [] = []
group l@(x:_) = takeWhile (==x) l : group (dropWhile (==x) l)

-- Not mandatory, delete if you don't want this.
insertBy :: (a -> a -> Ordering) -> a -> [a] -> [a]
insertBy _ a [] = [a]
insertBy f a l@(x:xs)
  | f a x == GT = x : insertBy f a xs
  | otherwise = a : l

sortBy :: (a -> a -> Ordering) -> [a] -> [a]
sortBy f = foldr (insertBy f) []

groupBy :: (a -> a -> Bool) -> [a] -> [[a]]
groupBy _ [] = []
groupBy f l@(x:_) = takeWhile (f x) l : groupBy f (dropWhile (f x) l)

on :: (b -> b -> c) -> (a -> b) -> a -> a -> c
on f g a b = f (g a) (g b)

(&&&) :: (a -> b) -> (a -> c) -> a -> (b, c)
(&&&) f g a = (f a, g a)

sortOn :: Ord b => (a -> b) -> [a] -> [a]
sortOn f
  = map fst
  . sortBy (compare `on` snd)
  . map (id &&& f)

groupOn :: Eq b => (a -> b) -> [a] -> [[a]]
groupOn f
  = map (map fst)
  . groupBy ((==) `on` snd)
  . map (id &&& f)

classifyOn :: Ord b => (a -> b) -> [a] -> [[a]]
classifyOn f = groupOn f . sortOn f
