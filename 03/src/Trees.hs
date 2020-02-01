{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -fplugin=HLint #-} -- run hlint on build via the hlint source plugin

module Trees where

import Prelude
import Data.Monoid (Sum(..), All(..), Any(..), First(..))
import Data.Maybe (isJust)

data Tree a
  = Empty
  | Node a (Tree a) (Tree a)
  deriving Show

instance Eq a => Eq (Tree a) where
  (==) :: Tree a -> Tree a -> Bool
  Empty == Empty = True
  Node a left1 right1 == Node b left2 right2 = a == b && left1 == left2 && right1 == right2
  _ == _ = False

insertOrdered :: Ord a => a -> Tree a -> Tree a
insertOrdered x Empty = Node x Empty Empty
insertOrdered x (Node root left right)
  | x <= root = Node root (insertOrdered x left) right
  | otherwise = Node root left (insertOrdered x right) 

listToBST :: Ord a => [a] -> Tree a
listToBST = foldr insertOrdered Empty 

isBST :: Ord a => Tree a -> Bool
isBST Empty = True
isBST (Node _ Empty Empty) = True
isBST (Node root (Node leftroot leftleft leftright) Empty) = (leftroot <= root) && isBST (Node leftroot leftleft leftright)
isBST (Node root Empty (Node rightroot rightleft rightright)) = (root < rightroot) && isBST (Node rightroot rightleft rightright)
isBST (Node root (Node leftroot leftleft leftright) (Node rightroot rightleft rightright)) = (leftroot <= root) && (root < rightroot) && isBST (Node leftroot leftleft leftright) && isBST (Node rightroot rightleft rightright)

findBST :: Ord a => a -> Tree a -> Bool
findBST _ Empty = False
findBST x (Node root left right)
  | x == root = True
  | x < root = findBST x left
  | otherwise = findBST x right 

mapTree :: (a -> b) -> Tree a -> Tree b
mapTree _ Empty = Empty
mapTree f (Node root left right) = Node (f root) (mapTree f left) (mapTree f right)

foldTree :: Monoid a => Tree a -> a
foldTree Empty = mempty
foldTree (Node root left right) = foldTree left <> root <> foldTree right

foldMapTree :: Monoid b => (a -> b) -> Tree a -> b
foldMapTree _ Empty = mempty
foldMapTree f (Node root left right) = foldMapTree f left <> f root <> foldMapTree f right

sumTree :: Num a => Tree a -> a
sumTree = getSum . foldMapTree Sum

allTree :: (a -> Bool) -> Tree a -> Bool
allTree f = getAll . foldMapTree (All . f)

treeToList :: Tree a -> [a]
treeToList = foldMapTree (: [])

elemTree :: Eq a => a -> Tree a -> Bool
elemTree x = getAny . foldMapTree (Any . (x ==)) 

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
-- validateTree f = foldMapTree (`ifJust` f)
validateTree = undefined

data Direction
  = L -- go left
  | R -- go right
  deriving (Show, Eq)

fetch :: [Direction] -> Tree a -> Maybe a
fetch _ Empty = Nothing 
fetch [] (Node root _ _) = Just root
fetch (x:xs) (Node _ left right)
  | x == L = fetch xs left
  | otherwise = fetch xs right

paths :: Tree a -> [(a, [Direction])]
paths t = treeToList (go [] t)
  where go _ Empty = Empty
        go xs (Node root left right) = Node (root, xs) (go (xs ++ [L]) left) (go (xs ++ [R]) right)
