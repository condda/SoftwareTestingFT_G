module Bonus1 where

--import Lab1Bonus

--1
length' :: [a] -> Int
length' = foldr (const succ) 0

--2
elem' :: Eq a => a -> [a] -> Bool
elem' x = foldr (\n m -> (x==n) || m) False

--3
or' :: [Bool] -> Bool
or' = foldr (||) False

--4
map' :: (a->b) -> [a] -> [b]
map' f = foldr (\x ys -> f x : ys) []

--5
filter' :: (a -> Bool) -> [a] -> [a]
filter' p = foldr (\x ys -> if p x then x : ys else ys) []

--6
(+++) :: [a] -> [a] -> [a]
(+++) xs ys= foldr (\a bs -> a : bs ) ys xs

--7
reverse' :: [a] -> [a]
reverse' = foldr (\x ys -> ys ++ [x]) []

