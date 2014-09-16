number_of_derangements :: Int -> Int
number_of_derangements 1 = 1
number_of_derangements x
  | even x = (number_of_derangements (x-1) ) * (x-1) - 1
  | otherwise  = (number_of_derangements (x-1) ) * (x-1) + 1
