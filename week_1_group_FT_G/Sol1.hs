-- 1.9
list_max :: [Int] -> Int
list_max [] = error "No max from emptry list"
list_max [x] = x
list_max (x:y:xs) = max x (list_max xs)

-- 1.10
removeFst :: (Eq a) => a -> [a] -> [a]
removeFst _ [] = []
removeFst m (x:xs)
  | m == x = xs
  | otherwise = x: removeFst m xs

-- 1.13
count :: (Eq a) => a -> [a] -> Int
count x xs = length $ filter (== x) xs

-- count _ [] = 0
-- count m (x:xs)
--  | m == x = 1 + count m xs
--  | otherwise = count m xs


-- 1.14
blowup :: String -> String
blowup = concat . (zipWith replicate [1..])

-- 1.15
put_next :: (Ord a) =>  a -> [a] -> [a]
put_next x [] = [x]
put_next m (x:xs)
  | m < x = (m:x:xs)
  | otherwise = x : (put_next m xs)

srtString :: [String] -> [String]
srtString [] = []
srtString [x] = [x]
srtString (x:xs) = put_next x (srtString xs)
 

-- 1.17
substring :: String -> String -> Bool
substring _ [] = False
substring str1 str2 = (equalsStart str1 str2) || substring str1 (tail str2)
equalsStart [] _ = True
equalsStart _ [] = False 
equalsStart (x:xs) (y:ys) = (x == y) && (equalsStart xs ys)


-- 1.20
lengths :: [[a]] -> [Int]
lengths = map length

-- 1.21
sumLengths :: [[a]] -> Int
sumLengths = sum.lengths





