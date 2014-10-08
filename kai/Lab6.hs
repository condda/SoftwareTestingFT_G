module Lab6

where
import Data.List
import System.Random
import Week6
import Test.QuickCheck
import Test.Hspec


-- 1
-- You can find the exM in the Week6 file

--exM :: Integer -> Integer -> Integer -> Integer
--exM x 1 m = rem x m
--exM x y m
--  | (rem y 2) == 0 = (exM x2 (quot y 2) m)
--  | otherwise = rem (x * (exM x2 (quot (y - 1) 2) m)) m
--  where x2 = (rem (x*x) m)
 

-- its often slow because expM is slow for big numbers
testExM :: Integer -> Integer -> Integer -> Bool
testExM x y z = (y < 0) || (z <= 0) || (y <= 15) || (exM x y z) == (expM x y z)


--2 

-- expM 10 1000000 7 vs exM 10 1000000 7
-- exM is fast
-- expM doesn't give an answer is reasonable time

--3
composed = composed_sieve [2..]
composed_sieve (n:ns) = [(n+1)..((head$next_prime) - 1)] ++ (composed_sieve next_prime)
  where next_prime = (filter (\ m -> rem m n /= 0) ns)
      
-- 4
numbers_that_pass_the_fermat_test :: [Integer] -> IO ()
numbers_that_pass_the_fermat_test (x:xs) = do
	p <- (primeF 3 x)
	if p
		then do
			print x
			numbers_that_pass_the_fermat_test xs
		else numbers_that_pass_the_fermat_test xs

composed_pass_fermat = numbers_that_pass_the_fermat_test composed
carmichaels_pass_fermat = numbers_that_pass_the_fermat_test carmichael

-- 6
numbers_that_pass_the_miller_rabin_test :: [Integer] -> IO ()
numbers_that_pass_the_miller_rabin_test (x:xs) = do
	p <- (primeMR 1 x)
	if p
		then do
			print x
			numbers_that_pass_the_miller_rabin_test xs
		else numbers_that_pass_the_miller_rabin_test xs

composed_pass_miller = numbers_that_pass_the_miller_rabin_test composed
carmichaels_pass_miller = numbers_that_pass_the_miller_rabin_test carmichael

-- 7
fake_mersenne :: [Integer] -> IO ()
fake_mersenne [] = print "end"
fake_mersenne (x:xs) = do
	p <- (primeMR 1 ((2^x) - 1))
	if p
		then do
			print x
			fake_mersenne xs
		else fake_mersenne xs



-- doesn't work because the list is endless
-- mersenne2 :: IO [Integer]
-- mersenne2 = do sequence $ map (primeMR 1) primes

-- showMersenne2 = do
--	x <- mersenne2
--	print $ head  x

-- 8

prime_with_bitlength :: Integer -> IO Integer
prime_with_bitlength n = do
	p <- randomRIO(2^(n-1), 2^n)
	q <- primeMR 4 p
	if q
		then return p
		else prime_with_bitlength n

--TODO write a function that generates 2 keys and use them for encoding and decoding



