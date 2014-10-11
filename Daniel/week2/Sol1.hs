module Lab2 where

import Data.List
import System.Random

data Shape = NoTriangle | Degenerate | Equilateral | Isosceles | Rectangular | Other deriving (Eq,Show)

triangle :: Integer -> Integer -> Integer -> Shape
triangle x y z | a < 0 = NoTriangle
               | a + b == c = Degenerate
			   | (a == b) && (b == c) =  Equilateral
               | (a == b) || (b == c) = Isosceles
			   | a*a + b*b == c*c = Rectangular
			   | otherwise = Other
			   where [a,b,c] = sort [x,y,z]
			   
-- 40 min

--2
contains :: Eq a => a -> [a] -> Bool
contains _ [] = False
contains x (y:ys) | x == y = True
                  | otherwise = contains x ys

removeElem :: Eq a => a -> [a] -> [a]
removeElem _ [] = []
removeElem x (y:ys) | x == y = ys
                    | otherwise = y : (removeElem x ys)

isPermutation :: Eq a => [a] -> [a] -> Bool
isPermutation [] [] = True
isPermutation _ [] = False
isPermutation [] _ = False
isPermutation (x:xs) ys | contains x ys = (isPermutation (removeElem x ys) xs)
                        | otherwise = False


isPermutation1 :: Ord a => [a] -> [a] -> Bool
isPermutation1 x y = sort x == sort y
--- 1 hour

--4
removeElement n [] = [] 
removeElement n (x:xs)
  | n == x = xs
  | otherwise = x:(removeElement n xs)
  
permutation :: Eq a => [a] ->  [[a]]
permutation [] = [[]]
permutation xs = [x:ys | x <- xs, ys <- permutation (removeElement x xs)]

--5

isDerangement :: Eq a => [a] -> [a] -> Bool
isDerangement [] [] = True
isDerangement (x:xs) (y:ys) = x /= y && isDerangement xs ys

--6
deran :: [a] -> [a]
deran xs = filter (isDerangement xs) $ permutation xs


-- isDerangement :: [a] -> [a] -> Bool
-- isDerangement x y = filter (deran x y) (permutation x)







  
  
fst1 :: (a,b) -> a
fst1 (x,y) = x 

zip1 :: [a]->[b]->[(a,b)]
zip1 [] _ = []
zip1 _ [] = []
zip1 (x:xs) (y:ys) = (x,y) : zip1 xs ys

elem1 :: Eq a => a -> [a] -> Bool
elem1 _ [] = False
elem1 a (x:xs) | a == x = True
               | otherwise = elem1 a xs
			   
compare1 :: Ord a => a -> a -> Ordering
compare1 a b | a == b = EQ
             | a > b = GT
			 | otherwise = LT