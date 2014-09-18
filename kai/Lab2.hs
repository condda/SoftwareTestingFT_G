module Lab2 where

import Data.List
import System.Random

import Control.Applicative



-- 1 (20min)
data Shape = NoTriangle | Equilateral | Isosceles | Rectangular | Other deriving (Eq,Show)

shape :: Integer -> Integer -> Integer -> Shape 
shape x y z
  | a < 0 = NoTriangle
  | a + b <= c = NoTriangle
  | a == b && b == c = Equilateral
  | a*a + b*b == c*c = Rectangular
  | a == b || b == c = Isosceles
  | otherwise = Other
  where [a,b,c] = sort [x,y,z]

-- 1 testing



-- 2 (30 min) 
removeElement n [] = [] 
removeElement n (x:xs)
  | n == x = xs
  | otherwise = x:(removeElement n xs)

contains _ [] = False
contains n (x:xs)
  | n == x = True
  | otherwise = contains x xs

isPermutation :: Eq a => [a] -> [a] -> Bool
isPermutation [] [] = True

isPermutation (x:xs) ys
  | contains x ys = isPermutation xs (removeElement x ys)
isPermutation _ _ = False

-- or if the input is off time Ord
isPermutation2 x y = (sort x) == (sort y)
-- 3 testing
-- isPermutation [1..1000000] [1000000..1]
-- #=> True
-- isPermutation [True, True, False, False, False] [False, True, False, True, False]
-- #=> True




shuffle :: [a] -> IO [a]
shuffle [] = return []
shuffle lst = do
    (e, rest) <- pickElem <$> getIx
    (e:) <$> shuffle rest
    where
    getIx = getStdRandom $ randomR (1, length lst)
    pickElem n = case splitAt n lst of
        ([], s) -> error $ "failed at index " ++ show n -- should never match
        (r, s)  -> (last r, init r ++ s)



-- 4
-- | The 'permutations' function returns the list of all permutations of the argument.
--
-- > permutations "abc" == ["abc","bac","cba","bca","cab","acb"]
perms            :: [a] -> [[a]]
perms xs0        =  xs0 : perms' xs0 []
  where
    perms' []     _  = []
    perms' (t:ts) is = foldr interleave (perms' ts (t:is)) (perms is)
      where interleave    xs     r = let (_,zs) = interleave' id xs r in zs
            interleave' _ []     r = (ts, r)
            interleave' f (y:ys) r = let (us,zs) = interleave' (f . (y:)) ys r
                                     in  (y:us, f (t:y:us) : zs)

-- 5
isDerangement :: Eq a => [a] -> [a] -> Bool
isDerangement [] [] = True
isDerangement (x:xs) (y:ys) = x /= y && isDerangement xs ys

-- 6
deran :: Int -> [[Int]]
deran x = filter (isDerangement list) $ perms list
  where list = [1..(x-1)]
-- maybe we can do this faster


sinterklaas :: Eq a => [a] -> IO [a]
sinterklaas x = do
              line <- shuffle x
              if isDerangement x line
                then return line
                else sinterklaas x

--sinterklaas [] = error "No elements to shuffle"
--sinterklaas (x:[]) = []
--sinterklaas (x:y:[]) = [2, 1]
--sitnerklaas xs = (y, pos, ys) = random (xs)
--sinterklaas n = x op plaats y
--  x = random_getal(n-1)
--  y = random_getal(n-2)

--number_of_derangements x
--  | even x = (number_of_derangements x)  * x + 1
--  | otherwise  = (number_of_derangements x) * x - 1

 --(f x) * x + 1
 --f x = + 1 + (x-1) * f (x-1)
 --f x = - 1 + (x-1) * f (x-1)

 --x=2n
 --y=2m+1

 --f 2n     = (f (2n-1) ) * (2n-1) - 1
 --f 2n + 1 = ((f (2n-1) ) * (2n-1) - 1 ) * (2n) + 1
 --f 2n + 1 = f (2n-1)  * (2n-1)* 2n + (-2n) + 1
 --f 2n + 1 = f (2n-1)  * (2n-1)* 2n + (-2n) + 1
 --f x = f (x-2) * (x-2) * (x-1) - x + 2


number_of_derangements :: Int -> Int
number_of_derangements 1 = 1
number_of_derangements x
  | even x = (number_of_derangements (x-1) ) * (x-1) - 1
  | otherwise  = (number_of_derangements (x-1) ) * (x-1) + 1

  




