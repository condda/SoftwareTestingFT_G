-- NOTE: This is NOT the final version!!

module Test

where

import Data.List
import Data.Char
import System.Random
import Control.Exception

-- Taken from slides.:
getRandomInt :: Int -> IO Int
getRandomInt n = getStdRandom (randomR (0,n))

-- Create an infinite list of indexes
randomIndexList :: Int -> IO [Int]
randomIndexList indexes = do
  g <- newStdGen
  return (randomRs (0, (indexes-1)) g)

remElById :: [a] -> Int -> [a]
remElById (l:lst) id | id == 0	 = lst
		     | otherwise = l : (remElById lst (id - 1))



-- Based on the list of unused indexes, we can create a shuffle of unused elements for which the element is
-- unequal to the index.
-- In some cases (chance is 1/n), the LAST element is equal to the index. In that case, createIndexMapFor reruns this function
-- with a shifted random set.
createIndexMapFor' :: [Int] -> [Int] -> Int -> [Int]
createIndexMapFor' [] _ _ = []
createIndexMapFor' unused (r:randomIndexes) index | r >= (length unused) = createIndexMapFor' unused randomIndexes index
						  | r == index = createIndexMapFor' unused randomIndexes index
						  | otherwise = (unused !! r) : (createIndexMapFor' (remElById unused r) randomIndexes (index + 1))


isLastElementEqualToId x = (x !! ((length x) - 1)) == (length x) - 1

-- Here we create a derangement index map of length n, using an infinite list of random indexes.
-- If the random element is not contained in the list, check for the next element in the list.
createIndexMapFor :: Int -> [Int] -> [Int]
createIndexMapFor n (r:randomIndexes) | not (isLastElementEqualToId indices) = indices
				      | otherwise = createIndexMapFor n randomIndexes
	where indices = createIndexMapFor' [0..(n-1)] randomIndexes 0


-- Here we create a list of random indexes, which is passed into createIndexMap, which creates the derangement indexes
-- from which we can create the list of derangements.
-- MEMO TO SELF: I HATE MONADS! AVOID THEM WHENEVER POSSIBLE!
rand n = do
  x <- randomIndexList n
  return $ createIndexMapFor n x


elementsByIndexes [] _ = []
elementsByIndexes (id:ids) lst = lst !! id : (elementsByIndexes ids lst)

-- This is the function where this entire bonus assignment is all about:
derLst lst = do
  ids <- rand (length lst)
  return $ elementsByIndexes ids lst

-- From the assignment:
contains :: Eq a => [a] -> a -> Bool
contains [] _ = False
contains (a:as) el
	| a == el	= True
	| otherwise	= contains as el

remFst :: Eq a => a -> [a] -> [a]
remFst el (a:as)
	| el == a 	= as
	| otherwise 	= a:remFst el as


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

-- Tests
-- No, it does not follow from the previous tests that this also works.
testDerLst n = do
  x <- (derLst [0..(n-1)])
  return $ (isDerangement x [0..(n-1)], x)
