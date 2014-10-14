module Sol2 where

import GS
import TAMO

-- 2.51
unique :: (a -> Bool) -> [a] -> Bool
unique p xs = (length $ filter p xs ) == 1

-- 2.52
parity :: [Bool] -> Bool
parity xs = even $ length $ filter (==True) xs

-- 2.53
evenNR :: (a -> Bool) -> [a] -> Bool
evenNR p xs = even $ length $ filter p xs 
