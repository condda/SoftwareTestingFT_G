module Sol2 where

--import TAMO

-- TODO 2.13

-- 2.51
unique :: Eq a => (a -> Bool) -> [a] -> Bool
unique _ [] = False
unique p (x:xs) = (p x) && (not (unique p xs))

-- 2.52
parity :: [Bool] -> Bool
parity [] = True
parity (x:xs) | x = not (parity xs)
	      | otherwise = parity xs

-- 2.53
evenNR :: Eq a => (a -> Bool) -> [a] -> Bool
evenNR p xs = parity (map p xs)
