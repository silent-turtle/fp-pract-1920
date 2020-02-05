{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE NamedFieldPuns #-}
-- ^ allows us to write shorter record matches:
-- instead of writing
-- ```
-- toAssocList Trie {val = val, children = children} ...
-- ```
--
-- we can just write
--
-- ```
-- toAssocList Trie {val, children} = ...
-- ```
--
-- with the same effect

module Trie where

import Data.Bifunctor

data Trie a = Node
  { val :: Maybe a
  , children :: [(Char, Trie a)]
  }
  deriving (Show, Eq)

instance Functor Trie where
  fmap :: (a -> b) -> Trie a -> Trie b
  fmap _ (Node Nothing []) = Node Nothing []
  fmap f (Node (Just x) []) = Node (Just (f x)) []
  fmap f (Node Nothing xs) = Node Nothing (map (second (fmap f)) xs)
  fmap f (Node (Just x) xs) = Node (Just (f x)) (map (second (fmap f)) xs) 

instance Foldable Trie where
  foldMap :: Monoid b => (a -> b) -> Trie a -> b
  foldMap _ (Node Nothing []) = mempty
  foldMap f (Node (Just x) []) = f x
  foldMap f (Node Nothing xs) = foldr ((<>) . foldMap f . snd) mempty xs
  foldMap f (Node (Just x) xs) = f x <> foldr ((<>) . foldMap f . snd) mempty xs

modify :: Eq k => k -> v -> [(k, v)] -> [(k, v)]
modify _ _ [] = []
modify key value ((k, v):xs)
  | key == k = (key, value) : modify key value xs
  | otherwise = (k, v) : modify key value xs 

insert :: String -> a -> Trie a -> Trie a
insert "" value (Node _ xs) = Node (Just value) xs
insert (x:xs) value (Node y ys) 
  | isNothing (lookupTrie (x:xs) (Node y ys)) = Node y ((x, insert xs value (Node Nothing [])):ys)
  | otherwise = Node y ys

lookupTrie :: String -> Trie a -> Maybe a
lookupTrie "" (Node x _) = x
lookupTrie (x:xs) (Node _ ys) = case filteredTrie of [] -> Nothing
                                                     (y:_) -> lookupTrie xs (snd y)
  where filteredTrie = filter (\z -> fst z == x) ys

first :: (a -> c) -> (a, b) -> (c, b)
first f (x, y) = (f x, y)

maybeToList :: Maybe a -> [a]
maybeToList Nothing = []
maybeToList (Just x) = [x]

toAssocList :: Trie a -> [(String, a)]
toAssocList = helper []
  where helper :: String -> Trie a -> [(String, a)]
        helper prefix (Node Nothing xs) = foldMap (\(k, v) -> helper (prefix ++ [k]) v) xs
        helper prefix (Node (Just x) xs) = (prefix, x) : foldMap (\(k, v) -> helper (prefix ++ [k]) v) xs

fromAssocList :: [(String, a)] -> Trie a
fromAssocList []  = Node Nothing []
fromAssocList [(k, v)] = insert k v (Node Nothing [])
fromAssocList ((k, v) : xs) = insert k v (fromAssocList xs)

subTrie :: String -> Trie a -> Maybe (Trie a)
subTrie "" x= Just x
subTrie (x:xs) (Node _ ys) = case filteredTrie of [] -> Nothing
                                                  (y:_) -> subTrie xs (snd y)
  where filteredTrie = filter (\z -> fst z == x) ys
