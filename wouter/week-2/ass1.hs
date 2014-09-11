-- Assignment 1.

module Lab2 where

import Data.List
--import System.Random

data Shape = NoTriangle | Equilateral
           | Isosceles | Rectangular | Other deriving (Eq,Show)

triangle :: Integer -> Integer -> Integer -> Shape
triangle a b c	| a <= 0 || b <= 0 || c <= 0	= NoTriangle
		| a == b && b == c		= Equilateral
		| a ^ 2 == b ^ 2 + c ^ 2 ||
		  b ^ 2 == a ^ 2 + c ^ 2 ||
		  c ^ 2 == a ^ 2 + b ^ 2	= Rectangular
		| a==b || b==c || a==c		= Isosceles
		| otherwise			= Other

validateAll :: [Bool] -> Bool
validateAll [] = True
validateAll (x:xs)
		| x = validateAll xs
		| otherwise = False

testTriangle1a :: Bool
testTriangle1a = validateAll
		[ triangle a b c == NoTriangle
		| a <- [-10..10], b <- [-10..10], c <- [-10..10],
		  a <= 0 || b <= 0 || c <= 0 ]

testTriangle1b :: Bool
testTriangle1b = validateAll
		[ triangle a b c /= NoTriangle
		| a <- [-10..10], b <- [-10..10], c <- [-10..10],
		  not (a <= 0 || b <= 0 || c <= 0) ]

testTriangle2a :: Bool
testTriangle2a = validateAll
		[ triangle a a a == Equilateral
		| a <- [1..10] ]

testTriangle2b :: Bool
testTriangle2b = validateAll
		[ triangle a b c /= Equilateral
		| a <- [-10..10], b <- [-10..10], c <- [-10..10],
		  not (a == b && b == c) ]

testTriangle3a :: Bool
testTriangle3a = validateAll
		[ triangle a b c == Rectangular |
		  a <- [1..50], b <- [1..50], c <- [1..50],
		  a ^ 2 == b ^ 2 + c ^ 2 ||
		  b ^ 2 == a ^ 2 + c ^ 2 ||
		  c ^ 2 == a ^ 2 + b ^ 2 ]

testTriangle3b :: Bool
testTriangle3b = validateAll
		[ triangle a b c /= Rectangular |
		  a <- [1..50], b <- [1..50], c <- [1..50],
		  not (a ^ 2 == b ^ 2 + c ^ 2 ||
		  b ^ 2 == a ^ 2 + c ^ 2 ||
		  c ^ 2 == a ^ 2 + b ^ 2) ]

testTriangle4a :: Bool
testTriangle4a = validateAll
		[ triangle a b c == Isosceles |
		  a <- [1..10], b <- [1..10], c <- [1..10],
		  (a == b || b == c || a == c) &&
		  (not (a == b && b == c && a == c))]

testTriangle4b :: Bool
testTriangle4b = validateAll
		[ triangle a b c /= Isosceles |
		  a <- [1..10], b <- [1..10], c <- [1..10],
		  not ((a == b || b == c || a == c) &&
		  (not (a == b && b == c && a == c)))]

testTriangle5a :: Bool
testTriangle5a = validateAll
		[ triangle a b c == Other |
		  a <- [1..50], b <- [1..50], c <- [1..50], -- NoTriangle
		  a /= b && a /= c && b /= c && -- Equilateral, Isosceles
		  a ^ 2 /= b ^ 2 + c ^ 2 && -- Rectangular
		  b ^ 2 /= a ^ 2 + c ^ 2 &&
		  c ^ 2 /= a ^ 2 + b ^ 2]

testTriangle5b :: Bool
testTriangle5b = validateAll
		[ triangle a b c /= Other |
		  a <- [1..50], b <- [1..50], c <- [1..50], -- NoTriangle
		  not (a /= b && a /= c && b /= c && -- Equilateral, Isosceles
		  a ^ 2 /= b ^ 2 + c ^ 2 && -- Rectangular
		  b ^ 2 /= a ^ 2 + c ^ 2 &&
		  c ^ 2 /= a ^ 2 + b ^ 2)]

testTriangle :: Bool
testTriangle = validateAll
		[ testTriangle1a, testTriangle1b,
		  testTriangle2a, testTriangle2b,
		  testTriangle3a, testTriangle3b,
		  testTriangle4a, testTriangle4b,
		  testTriangle5a, testTriangle5b ]
-- Approx. 30 mins

-- +Time spent: 1:30-1:45 for the original implementation.

-- Assignment 2

-- I designed two implementations, one simple one using sort:
isPermutation :: Ord a => [a] -> [a] -> Bool
isPermutation as bs = length(as) == length(bs)
		   && sort(as) == sort(bs)

-- And one without the need of orderable elements:
remFst :: Eq a => a -> [a] -> [a]
remFst el (a:as)
	| el == a 	= as
	| otherwise 	= a:remFst el as

contains :: Eq a => [a] -> a -> Bool
contains [] _ = False
contains (a:as) el
	| a == el	= True
	| otherwise	= contains as el

isPermutation2 :: Eq a => [a] -> [a] -> Bool
isPermutation2 [] [] = True
isPermutation2 _ [] = False
isPermutation2 [] _ = False
isPermutation2 (a:as) bs
	| contains bs a = isPermutation2 as (remFst a bs)
	| otherwise	= False

-- 30 mins so far.

-- Assignment 3 - TODO
-- Shift the list to the right.
shiftLeft :: [a] -> [a]
shiftLeft (x:xs) = xs ++ [x]

dropOne :: [a] -> [a]
dropOne (x:xs) = xs

-- Test for permutation when equal, different order,
-- dropped element from the left-hand side,
-- dropped element from the right-hand side,
-- same length, one different element.
testPermutation :: Bool
testPermutation = validateAll
	[ isPermutation (shiftLeft x) x
	&& isPermutation x x
	&& not (isPermutation (dropOne x) x)
	&& not (isPermutation x (dropOne x))
	&& not (isPermutation [n+1 | n <- x] x)
	| x <- [[5..10], [-20..20]] ]

-- Assignment 4
remElById :: [a] -> Int -> [a]
remElById (a:as) 0 = as
remElById (a:as) i = a:remElById as (i - 1)

flatten :: [[a]] -> [a]
flatten [] = []
flatten (a:as) = a ++ flatten as

perms :: Eq a => [a] -> [[a]]
perms [] = []
perms (a:[]) = [[a]]
perms as = flatten[ [ a:bs | bs <- perms (remFst a as) ] | a <- as ]

-- Assignment 5
isSubset :: Eq a => [a] -> [a] -> Bool
isSubset [] [] = True
isSubset _ [] = False
isSubset [] _ = True
isSubset (a:as) bs | contains bs a	= isSubset as (remFst a bs)
		   | otherwise		= False

isDerangement' :: Eq a => [a] -> [a] -> Bool
isDerangement' [] [] = True
isDerangement' _ [] = False
isDerangement' [] _ = False
isDerangement' (x:xs) (y:ys)
	| x == y	= False
	| otherwise	= isDerangement' xs ys

isDerangement as bs = (isSubset as bs)
	&& (isSubset bs as)
	&& (isDerangement' as bs)
-- Time spent: 20 mins

-- MISTAKE: { 1,2,3 } -> { 1, 3, 2 } : not a derangement, because 1 is on the same pos.

-- Assignment 6
deran :: Int -> [[Int]]
-- We use isDerangement' here instead of isDerangement, because
-- it is certain that the items in x and [0..n-1] are the same.
deran n = [ x | x <- perms [0..n-1], isDerangement' x [0..n-1] ]
-- Time spent: 30 mins

-- Assignment 7: TODO
-- Basically testDerangement is almost the same as testPermutation,
-- with the exception that for sets where elements are on the same place,
-- they are not derangements from each other.
testDerangement :: Bool
testDerangement = validateAll
	[ isDerangement (shiftLeft x) x
	&& not (isDerangement x x)
	&& not (isDerangement (dropOne x) x)
	&& not (isDerangement x (dropOne x))
	&& not (isDerangement [n+1 | n <- x] x)
	| x <- [[5..10], [-20..20]] ]
