
import Lab2
import Test.QuickCheck


-- 1

test1 = [shape x y z | x <- [1..20], y <- [1..20], z <- [1..20]]


test_isPermutation xs ys = (isPermutation xs ys) == isPermutation2 xs ys
--test_unit_isPermutation = 
different :: Eq a => [a] -> Bool
different [] = True
different (x:xs) = (not (elem x xs)) && (different xs)

-- test_perms_no_perm_is_the_same xs = (length xs > 5) || different (perms xs)
test_perms_correct_length xs = (length xs > 8) || all (\x -> (length xs) == length x) (perms xs)
test_perms_total_length xs = (length xs > 8) || (length $ perms xs) == product [1..length xs]

--test_sinterklaas_is_permutation xs =  do
--    ys <- sinterklaas xs
--    return (isPermutation xs ys)

test_number_of_derangements n = (number_of_derangements n) ==