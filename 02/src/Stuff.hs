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
group (x:xs) = ((x : takeWhile (==x) xs) : group (dropWhile (==x) xs))

sortBy :: (a -> a -> Ordering) -> [a] -> [a]
sortBy _ [] = []
sortBy comp list = mergeSort list
  where merge [] xs = xs
        merge xs [] = xs
        merge (x:xs) (y:ys)
          | ((comp x y) == LT) || ((comp x y ) == EQ) = (x : merge xs (y:ys))
          | otherwise = (y : merge (x:xs) ys)
        split xs n = splitHelper xs n []
          where splitHelper ys m buffer
                  | m == 0 || (null ys) = (reverse buffer, ys)
                  | otherwise = splitHelper (tail ys) (pred m) ((head ys):buffer)
        mergeSort [] = []
        mergeSort [x] = [x]
        mergeSort xs = merge (mergeSort (fst splitxs)) (mergeSort (snd splitxs))
          where splitxs = split xs ((length xs) `quot`  2)

groupBy :: (a -> a -> Bool) -> [a] -> [[a]]
groupBy _ [] = []
groupBy p (x:xs) = ((x:takeWhile (p x) xs) : groupBy p (dropWhile (p x) xs))

on :: (b -> b -> c) -> (a -> b) -> a -> a -> c
on f g x y = (f (g x) (g y))

(&&&) :: (a -> b) -> (a -> c) -> a -> (b, c)
(&&&) f g x = (,) (f x) (g x)

sortOn :: Ord b => (a -> b) -> [a] -> [a]
sortOn _ [] = []
sortOn f xs = map fst (sortBy (compare `on` snd) (map ((&&&) id f) xs)) 

groupOn :: Eq b => (a -> b) -> [a] -> [[a]]
groupOn _ [] = []
groupOn f xs = map (map fst) (groupBy ((==) `on` snd) (map ((&&&) id f) xs))

classifyOn :: Ord b => (a -> b) -> [a] -> [[a]]
classifyOn _ [] = []
classifyOn f xs = map (map fst) (groupBy ((==) `on` snd) (sortBy (compare `on` snd) (map ((&&&) id f) xs))) 
