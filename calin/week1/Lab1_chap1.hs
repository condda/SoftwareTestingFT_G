module Sol1 where

import GS

-- 1.9
maxInt :: [Int] -> Int
maxInt [] = error "empty list"
maxInt [x] = x
maxInt (x:xs) = max x (maxInt xs)

-- 1.10
removeFst :: Eq a => a -> [a] -> [a]
removeFst _ [] = []
removeFst x (y:ys) | x == y = ys
		   | otherwise = y : removeFst x ys


-- 1.13
count :: Char -> String -> Int
count x [] = 0
count x (y:ys) 
	| x == y = 1 + ( count x ys )
	| otherwise = ( count x ys ) 

-- 1.14
blowup' :: Int -> String -> String
blowup' 0 _ = []
blowup' _ [] = []
blowup' x (y:ys) = (take x (repeat y)) ++ (blowup' (x+1) ys) 

blowup :: String -> String
blowup [] = []
blowup xs = blowup' 1 xs

-- 1.15
mnmStr :: [String] -> String
mnmStr [] = error "empty list"
mnmStr [x] = x
mnmStr (x:xs) = min x (mnmStr xs)

srtString :: [String] -> [String]
srtString [] = []
srtString xs =
	let
		m = mnmStr xs
	in m : (srtString ( removeFst m xs )) 


-- 1.17

substring :: String -> String -> Bool
substring [] ys = True
substring (x:xs) [] = False
substring xs all@(y:ys) = (prefix xs all) || (substring xs ys)

-- 1.20

lengths :: [[a]] -> [Int]
lengths xs = map length xs

-- 1.21

sumLengths :: [[a]] -> Int
sumLengths xs = sum ( lengths xs ) 
