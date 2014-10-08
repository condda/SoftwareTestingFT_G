module Lab6

where
import Data.List
import System.Random
import Week6
import Test.QuickCheck
import Test.Hspec

carmichael :: [Integer]
carmichael = [ (6*k+1)*(12*k+1)*(18*k+1) | 
      k <- [2..], 
      isPrime (6*k+1), 
      isPrime (12*k+1), 
      isPrime (18*k+1) ]

--exM' :: Integer -> Integer -> Integer -> Integer
--exM' x 1 m = rem x m
--exM' x y m
--  | (mod y 2) == 0 = (exM' (rem (x*x) m) (quot y 2) m)
--  | otherwise = rem (x * (exM' (rem (x*x) m) (quot (y - 1) 2) m)) m
  
test1 :: Integer -> Integer -> Integer -> Bool
test1 x y z = (y < 0) || (z <= 0) || (exM x y z) == (expM x y z)


--2 

-- exM' 10 1000000 7 vs exM 10 1000000 7
-- exM' is very vast
-- exM cant come up with an answer


--3
composed = ref_sieve [2..]
ref_sieve (n:ns) = [(n+1)..((head$new) -1 )] ++ (ref_sieve new)
  where new = (filter (\ m -> rem m n /= 0) ns)
      
-- 4

test_fails :: [Integer] -> IO ()
test_fails (x:xs) = do
	p <- (primeF 3 x)
	if p
		then do
			print x
			test_fails xs
		else test_fails xs

composed_pass_fermat = test_fails composed
carmichaels_pass_fermat = test_fails carmichael
-- 6
test_failsMR :: [Integer] -> IO ()
test_failsMR (x:xs) = do
	p <- (primeMR 1 x)
	if p
		then do
			print x
			test_failsMR xs
		else test_failsMR xs


composed_pass_miller = test_failsMR composed
carmichaels_pass_miller = test_failsMR carmichael


-- 7

--mersenne = do
--	filter (\p -> primeMR 1 ((2^p) - 1)) primes


mersenne :: [Integer] -> IO ()
mersenne [] = print "end"
mersenne (x:xs) = do
	p <- (primeMR 1 ((2^x) - 1))
	if p
		then do
			print x
			mersenne xs
		else mersenne xs



prime_with_bitlength :: Integer -> IO Integer
prime_with_bitlength n = do
	p <- randomRIO(2^(n-1),2^n)
	q <- primeMR 4 p
	if q
		then return p
		else prime_with_bitlength n

