module Sol1 where

-- import GS

-- Exercise 1.9
maxInt :: [Int] -> Int
maxInt [x] = x
maxInt (x : xs) = max x (maxInt xs)


-- Exercise 1.10
-- Changed a bit to support all types.
removeFst :: Eq a => a -> [a] -> [a]
removeFst _ [] = []
removeFst m (x:xs) | m == x    = xs
                   | otherwise = x : removeFst m xs

-- Exercise 1.13
count :: Char -> String -> Int
count _ [] = 0
count a (x:xs) | a == x    = 1 + count a xs
               | otherwise = count a xs


-- Exercise 1.14
blowup' :: Int -> String -> String
blowup' 0 _ = []
blowup' _ [] = []
blowup' tim (x:xs) = (take tim (repeat x)) ++ (blowup' (tim+1) xs)

blowup :: String -> String
blowup [] = []
blowup s = blowup' 1 s

-- Exercise 1.15
minStr :: [String] -> String
minStr [x] = x
minStr (x1:x2:xs) | x1 > x2 = x2
		| otherwise = x1

srtStrings :: [String] -> [String]
srtStrings [] = []
srtStrings xs = m : (srtStrings (removeFst m xs)) where m = minStr xs

-- 1.17
substr :: String -> String -> Bool
substr [] _ = True
substr _ [] = False
substr (s1:str1) (s2:str2) | s1 == s2 && substr str1 str2 || restTest = True
			   | otherwise = restTest
	where restTest = substr (s1:str1) str2

-- 1.20
lengths :: Eq a => [[a]] -> [Int]
lengths lst = map length lst

-- 1.21
sumLengths :: Eq a => [[a]] -> Int
sumLengths lst = sum (lengths lst)
