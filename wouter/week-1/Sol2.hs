module Sol2 where

--import TAMO

-- 2.13
-- -lequiv

-- 2.51
unique :: (a -> Bool) -> [a] -> Bool
--unique _ [] | false
unique p (x:xs) | p . x && (not (unique p xs))

-- 2.52


-- 2.53
