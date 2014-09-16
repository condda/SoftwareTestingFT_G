module Bonus2

where

-- We found the following function:
numberOfDerangements :: Int -> Int
numberOfDerangements 1 = 1
numberOfDerangements x | even x = (numberOfDerangements (x-1) ) * (x-1) - 1
                       | otherwise  = (numberOfDerangements (x-1) ) * (x-1) + 1
