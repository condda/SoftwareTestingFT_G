module Lab2 where

import Data.List
import System.Random

-- Ex 1
data Shape = NoTriangle | Equilateral 
		| Isosceles | Rectangular 
		| Other deriving (Eq,Show)

triangle :: Integer -> Integer -> Integer -> Shape
triangle x y z 	
		| x < 0 || y < 0 || z < 0 = NoTriangle
		| x + y < z || x + z < y || y + z < x = NoTriangle 
		| x == y && y == z = Equilateral
		| x == y || y == z || x == z = Isosceles
		| (x^2 + y^2 == z^2) 
			|| (x^2 + z^2 == y^2) 
			|| (y^2 + z^2 == x^2) = Rectangular
		| otherwise = Other


testEquilateral = all (==True) [triangle x y z == Equilateral
			| x <- [1..10], y <- [1..10], z <- [1..10], 
			 (x==y && y==z)]

testRectangular = all (==True) [triangle x y z == Rectangular
			| x <- [1..10], y <- [1..10], z <- [1..10],
			(x < y && y < z) && (x^2 + y ^2 == z ^2)]

-- Ex 2

contains :: Eq a => a -> [a] -> Bool
contains _ [] = False
contains x [y] = x == y
contains x (y:ys) | x == y = True
		  | otherwise = contains x ys

removeFst :: Eq a => a -> [a] -> [a]
removeFst _ [] = []
removeFst x (y:ys) | x == y = ys
		   | otherwise = y : (removeFst x ys)

isPermutation :: Eq a => [a] -> [a] -> Bool
isPermutation [] [] = True
isPermutation [] _ = False
isPermutation _ [] = False
isPermutation (x:xs) ys | contains x ys = ( isPermutation xs (removeFst x ys) )
			| otherwise = False


--isPermutation :: Ord a => [a] -> [a] -> Bool
--isPermutation x y = (sort x) == (sort y)

-- Ex 3
testPermutation = all (==True) [isPermutation [1,2,3,4] [2,3,1,4], 
				isPermutation [1,3,5] [3,5,1],
				(not $ isPermutation [1,2,3] [1,3,4]),
				(not $ isPermutation [1,2] [1])]

-- Ex 4
perms :: Eq a => [a] -> [[a]]
perms [] = [[]]
perms xs = [x:ys | x <- xs, ys <- perms (delete x xs)]

--http://shivindap.wordpress.com/2009/01/12/permutations-of-a-list-in-haskell/
--Number of permutations of a list with n elements is n!

-- Ex 5

isSamePos :: Eq a => [a] -> [a] -> Bool
isSamePos [] [] = True
isSamePos _ [] = True
isSamePos [] _ = True
isSamePos (x:xs) (y:ys) | x==y = False
			| otherwise = isSamePos xs ys

isDerangement :: Eq a => [a] -> [a] -> Bool
isDerangement [] [] = True
isDerangement _ [] = False
isDerangement [] _ = False
isDerangement xs ys = (isPermutation xs ys) && (isSamePos xs ys)

-- Ex 6

deran :: Eq a => [a] -> [[a]]
deran [] = [[]]
deran xs = filter (isDerangement xs) (perms xs)


-- Ex 7

testDerangement = all (==True) [(not $ isDerangement [] [1,2]),
				(not $ isDerangement [1] []),
				(not $ isDerangement [1,2,3] [1,3,2]),
				isDerangement [1,2,3] [3,1,2]]
