module Sol1 where
-- import GS.hs

divides :: Integer -> Integer -> Bool
divides d n = rem n d == 0

ld :: Integer -> Integer
ld n = ldf 2 n

ldf :: Integer -> Integer -> Integer
ldf k n | divides k n = k
		| k^2 > n     = n
		| otherwise   = ldf (k+1) n
		

prime0 :: Integer -> Bool
prime0 n | n < 1	= error "not a positive integer"
		 | n == 1 = False
		 | otherwise = ld n == n

mnmInt :: [Int] -> Int
mnmInt [] = error "empty list"
mnmInt [x] = x
mnmInt (x:xs) = min x (mnmInt xs)

--1.09
mxmInt :: [Int] -> Int
mxmInt [] = error "empty list"
mxmInt [x] = x
mxmInt (x:xs) = max x (mxmInt xs)

--1.10
removeFst :: Int -> [Int] -> [Int]
removeFst m [] = []
removeFst m (x:xs) | m == x = xs
				   | otherwise = x : removeFst m xs
				   
--1.11		 
srtInts :: [Int] -> [Int]
srtInts [] = []
srtInts xs = m : (srtInts (removeFst m xs)) where m = mnmInt xs

srtInts1 :: [Int] -> [Int]
srtInts1 [] = []
srtInts1 xs = let
				m = mnmInt xs
				in m : (srtInts1 (removeFst m xs))

--1.12
average :: [Integer] -> Float
average [] = error "empty list"
--average xs = fromInteger (sum xs) / fromInteger (length xs)

sum1 :: [Int] -> Int
sum1 [] = 0
sum1 (x:xs) = x + sum1 xs

length1 :: [a] -> Int
length1 [] = 0
length1 (x:xs) = 1 + length1 xs

--1.13
count :: Char -> String -> Int
count c [] = 0
count c (x:xs) | c == x = 1 + (count c xs)
			   | otherwise = count c xs

--1.14
blowup :: String -> String
blowup [] = []
blowup xs = blowup1 1 xs

blowup1 :: Int -> String -> String
blowup1 0 _ = []
blowup1 _ [] = []
blowup1 n (x:xs) = (take n (repeat x)) ++ (blowup1(n+1) xs)

--1.15
srtString :: [String] -> [String]
srtString [] = []
srtString xs = m : (srtString (removeFstStr m xs)) where m = mnmStr xs

mnmStr :: [String] -> String
mnmStr [x] = x
mnmStr (x1:x2:xs) | x1 <= x2  = x1
				  | otherwise = x2

removeFstStr :: String -> [String] -> [String]
removeFstStr m [] = []
removeFstStr m (x:xs) | m == x = xs
					  | otherwise = x : removeFstStr m xs

--1.16
prefix :: String -> String -> Bool
prefix [] ys = True
prefix (x:xs) [] = False
prefix (x:xs) (y:ys) = (x == y) && prefix xs ys

--1.17
subString :: String -> String -> Bool
subString [] ys = True
subString (x:xs) [] = False
subString xs ys = (equalsStart xs ys) || subString xs (tail ys)

equalsStart [] _ = True
equalsStart _ [] = False 
equalsStart (x:xs) (y:ys) = (x == y) && (equalsStart xs ys)


factors :: Integer -> [Integer]
factors n | n < 1     = error "argument not positive"
          | n == 1    = []
		  | otherwise = p : factors (div n p) where p = ld n
