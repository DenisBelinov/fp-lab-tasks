{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -fplugin=HLint #-} -- run hlint on build via the hlint source plugin

module Trees where

import Prelude
import Data.Monoid (Sum(..), All(..), Any(..), First(..))
import Data.Maybe (isJust)
import Data.Bifunctor

import Instances (Dual(..))

data Tree a
  = Empty
  | Node a (Tree a) (Tree a)
  deriving Show

instance Eq a => Eq (Tree a) where
  (==) :: Tree a -> Tree a -> Bool
  Empty == Empty = True
  (Node x t1 t2) == (Node x' t1' t2') = (x == x') && (t1 == t1') && (t2 == t2')
  _ == _ = False

insertOrdered :: Ord a => a -> Tree a -> Tree a
insertOrdered x Empty = Node x Empty Empty
insertOrdered x (Node a lt rt)
  | x > a = Node a lt (insertOrdered x rt)
  | otherwise = Node a (insertOrdered x lt) rt

listToBST :: Ord a => [a] -> Tree a
listToBST = foldr insertOrdered Empty

isBST :: Ord a => Tree a -> Bool
isBST = between Bot Top

-- idea for implementing isBST - delete if you don't want it
data BotTop a = Bot | Val a | Top
  deriving (Show, Eq, Ord)

between :: Ord a => BotTop a -> BotTop a -> Tree a -> Bool
between a b Empty = a <= b
between a b (Node x lt rt) = between a (Val x) lt && between (Val x) b rt

findBST :: Ord a => a -> Tree a -> Bool
findBST _ Empty = False
findBST a (Node x lt rt) = x == a || findBST a lt || findBST a rt

mapTree :: (a -> b) -> Tree a -> Tree b
mapTree _ Empty = Empty
mapTree f (Node x lt rt) = Node (f x) (mapTree f lt) (mapTree f rt)

foldTree :: Monoid a => Tree a -> a
foldTree Empty = mempty
foldTree (Node x lt rt) = foldTree lt <> x <> foldTree rt

foldMapTree :: Monoid b => (a -> b) -> Tree a -> b
foldMapTree f = foldTree . mapTree f

sumTree :: Num a => Tree a -> a
sumTree = getSum . foldMapTree Sum

allTree :: (a -> Bool) -> Tree a -> Bool
allTree p = getAll . foldMapTree (All . p)

treeToList :: Tree a -> [a]
treeToList = foldMapTree (:[])

elemTree :: Eq a => a -> Tree a -> Bool
elemTree x = getAny . foldMapTree (Any . (==x))

onMaybe :: (a -> Bool) -> a -> Maybe a
onMaybe p x = if p x then Just x else Nothing

findPred :: (a -> Bool) -> Tree a -> Maybe a
findPred p = getFirst . foldMapTree (First . onMaybe p)

findAll :: (a -> Bool) -> Tree a -> [a]
findAll p = foldMapTree (\x -> [x | p x])

ifJust :: Maybe a -> (a -> Maybe b) -> Maybe b
ifJust Nothing _ = Nothing
ifJust (Just x) f = f x

validateTree :: (a -> Maybe b) -> Tree a -> Maybe (Tree b)
validateTree _ Empty = Just Empty
validateTree f (Node v lt rt) =
  validateTree f lt `ifJust` (\lt' ->
  f v               `ifJust` (\v' ->
  validateTree f rt `ifJust` (Just . Node v' lt')))

data Direction
  = L -- go left
  | R -- go right
  deriving (Show, Eq)

fetch :: [Direction] -> Tree a -> Maybe a
fetch _ Empty = Nothing
fetch [] (Node x _ _) = Just x
fetch (L:ds) (Node _ lt _) = fetch ds lt
fetch (R:ds) (Node _ _ rt) = fetch ds rt

mapDirections :: Tree a -> Tree (a, [Direction])
mapDirections Empty = Empty
mapDirections (Node x lt rt) = Node (x, []) newlt newrt
  where newlt = mapTree (second ((:) L)) (mapDirections lt)
        newrt = mapTree (second ((:) R)) (mapDirections rt)

paths :: Tree a -> [(a, [Direction])]
paths = treeToList . mapDirections
