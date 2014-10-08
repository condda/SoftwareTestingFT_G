module Lab6

where
import Data.List
import System.Random
import Week6
import Test.QuickCheck

carmichael :: [Integer]
carmichael = [ (6*k+1)*(12*k+1)*(18*k+1) | 
      k <- [2..], 
      isPrime (6*k+1), 
      isPrime (12*k+1), 
      isPrime (18*k+1) ]

-- Assignment 1
-- a ^ b mod c
exM2 :: Integer -> Integer -> Integer -> Integer
exM2 a 0 c = 1 `rem` c
exM2 a b c | b `rem` 2 == 0   = ((exM2 a (b `quot` 2) c) ^ 2) `rem` c
           | otherwise        = ((exM2 a (b - 1) c) * a) `rem` c

exTest :: Integer -> Integer -> Integer -> Bool
exTest a b c | b <= 0 = True
             | c <= 0 = True
             | otherwise = exM2 a b c == exM a b c

exTest2 :: Integer -> Integer -> Integer -> Bool
exTest2 x y z = True

-- Assignment 2

-- exM 100 (2^32) 10
-- using :set +s for timing, I got the following results:

-- exM 100 (2^24) 10 took 1.36 secs,       159623272 bytes

-- exM 100 ((2^24)-1) 10 took 2.53 secs,   261426480 bytes

-- exM2 100 (2^24) 10 took 0.00 secs,        2059024 bytes

-- exM2 100 ((2^24)-1) 10 took 0.00 secs,    2605976 bytes




-- exM 100 (2^26) 10 took 11.94 secs,      628208792 bytes

-- exM 100 ((2^26)-1) 10 took 11.94 secs, 1034963032 bytes

-- exM2 100 (2^26) 10 took 0.00 secs,        2059040 bytes

-- exM2 100 ((2^26)-1) 10 took 0.01 secs,    2575176 bytes

-- Assignment 3
composites :: [Integer]
composites = composites' primes
  where composites' (p1:p2:ps) = [p1+1..p2-1] ++ composites' (p2:ps)

-- Very simple test using the first 105 composites from http://en.wikipedia.org/wiki/Composite_number
testComposites = take 105 composites == [ 4, 6, 8, 9, 10, 12, 14, 15, 16, 18, 20, 21, 22, 24, 25, 26, 27, 28, 30, 32, 33, 34, 35, 36, 38, 39, 40, 42, 44, 45, 46, 48, 49, 50, 51, 52, 54, 55, 56, 57, 58, 60, 62, 63, 64, 65, 66, 68, 69, 70, 72, 74, 75, 76, 77, 78, 80, 81, 82, 84, 85, 86, 87, 88, 90, 91, 92, 93, 94, 95, 96, 98, 99, 100, 102, 104, 105, 106, 108, 110, 111, 112, 114, 115, 116, 117, 118, 119, 120, 121, 122, 123, 124, 125, 126, 128, 129, 130, 132, 133, 134, 135, 136, 138, 140 ]

-- Assignment 4

-- As the composites are the numbers that are not prime, we can
-- test Fermat's Primality Check by checking if k is "very" high.

-- Test if "probable prime" can be found in the list of numbers
testP k (n:ns) = do
  f <- primeF k n
  if f
    then return n
    else testP k ns

testF k = testP k composites

-- With testF 1, we got numbers between 4 and 63 after 20 runs.
testCompF _ 0 k nos = testP k nos
testCompF fn times k nos = do
  f <- testP k nos
  g <- testCompF fn (times - 1) k nos
  return $ if f `fn` g then f else g

-- testCompF (<) 100 1 composites resulted in 4    (0.00 secs, 1577184 bytes)
-- testCompF (>) 100 1 composites resulted in 63   (0.01 secs, 3150840 bytes)

-- testCompF (<) 100 2 composites resulted in 4    (0.36 secs, 176821472 bytes)
-- testCompF (>) 100 2 composites resulted in 1417 (0.25 secs, 144077600 bytes)

-- testCompF (<) 100 3 composites resulted in 4    (4.20 secs, 962954240 bytes)
-- testCompF (>) 100 3 composites resulted in 8911 (4.96 secs, 1197929216 bytes)

-- As we can see from the results above, the chance of finding a prime number is higher for a higher k.
-- However, also memory and time increases dramatically. This is logically explainable:
-- the memory used for the function primeF is O(3k), because k is the amount of times
-- primeF is called internally, with 0 just returning True. Also, the more precisely prime numbers
-- can be found, the more calls are made to primeF, because testMinF exits when the first
-- composite is found.



-- Assignment 5
-- Unfortunately testCompF (>) 100 1 carmichael freezes my PC.
-- However, testCompF (>) 1 1 carmichael did give me a very high composite number, 294409 for k = 1.
