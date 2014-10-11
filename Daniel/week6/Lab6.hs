module Lab6

where
import Data.List
import System.Random
import Test.QuickCheck
import Week6

carmichael :: [Integer]
carmichael = [ (6*k+1)*(12*k+1)*(18*k+1) | 
      k <- [2..], 
      isPrime (6*k+1), 
      isPrime (12*k+1), 
      isPrime (18*k+1) ]
	  
	  
--1

exM1 :: Integer -> Integer -> Integer -> Integer
exM1 x 0 n = 1
exM1 0 p n = 0
exM1 x p 0 = error "cannot divide by 0"
exM1 x p n = product [mod (x^y) n | y <- getListP p]

getListP :: Integer -> [Integer]
getListP 0 = []
getListP 1 = [1]
getListP y
  | isSqrt y = y : getListP x
  | otherwise = 1 : getListP (y - 1)
  where x = (div y 2)
  
isSqrt :: Integer -> Bool
isSqrt 1 = True
isSqrt y
  | even y = isSqrt x
  | otherwise = False
  where x = (div y 2)

test1 :: Integer -> Integer -> Integer -> Bool
test1 x y z = (y < 0) || (z <= 0) || (exM1 x y z) == (expM x y z)