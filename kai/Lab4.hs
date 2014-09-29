
module Lab4 where 
import Test.QuickCheck
import Test.Hspec
import System.Random
import Data.List
import SetOrd

import Data.Time
import Data.Char (chr)
import Test.QuickCheck
import Control.Applicative

import System.Environment (getArgs)




fp :: Eq a => (a -> a) -> a -> a
fp f x
  | x == f x = x 
  | otherwise = fp f (f x)

-- 1


-- 0 uur

-- 2




-- 0 uur

-- 3
getRandomSet :: IO(Set Int)
getRandomSet = do
  gen <- newStdGen
  return (list2set $ take 10 $ randoms gen)

--instance Arbitrary Color where

--instance Arbitrary a => Arbitrary (Set a) where
--  arbitrary = getRandomSet


--instance Arbitrary a => Arbitrary (Set a) where
--  arbitrary = do
--    x <- arbitrary
--    xs <- getRandomSet
--    return $ (Set xs)

--instance (Arbitrary a, Ord a) => Arbitrary (Set a) where
--  arbitrary = list2set <$> arbitrary

instance (Arbitrary a, Ord a) => Arbitrary (Set a) where
  arbitrary = sized $ \n ->
    do k <- choose (0,n)
       list2set <$> sequence [ arbitrary | _ <- [1..k] ]

-- 4

intersectionSet :: Ord a => Set a -> Set a -> Set a
intersectionSet (Set x) (Set y) = Set (intersection' x y)

intersection' :: Ord a => [a] -> [a] -> [a]
intersection' [] _ = []
intersection' _ [] = []
intersection' (x:xs) (y:ys)
  | x == y = x : (intersection' xs ys)
  | x > y = (intersection' (x:xs) ys)
  | x < y = (intersection' xs (y:ys))


unionSet :: Ord a => Set a -> Set a -> Set a
unionSet (Set x) (Set y) = list2set (x ++ y)


differenceSet :: Ord a => Set a -> Set a -> Set a
differenceSet (Set x) (Set y) = Set (difference' x y)
difference' [] _ = []
difference' xs [] = xs
difference' (x:xs) (y:ys)
  | x == y = (difference' xs ys)
  | x >  y = (difference' (x:xs) ys)
  | x <  y = x:(difference' xs (y:ys))

test_intersection_is_subSet_of_y :: Set Int -> Set Int -> Bool
test_intersection_is_subSet_of_y x y = subSet (intersectionSet x y) x

test4 :: IO ()
test4 = hspec $ do
  describe "Tests for exercise 4" $ do
    it "test something" $ 
      intersectionSet (Set [1,2,3,4]) (Set [-100..100]) == Set [1,2,3,4]
    it "x should be a subset of (the union of x and y)" $ property $
       \x y -> subSet x (unionSet (x :: Set Int) (y :: Set Int))

    it "y should be a subset of (the union of x and y)" $ property $
      \x y -> subSet y (unionSet (x :: Set Int) (y :: Set Int))

    it "(The difference of x and y) should be a subset of x" $ property $
      \x y -> subSet (differenceSet (x :: Set Int) (y :: Set Int)) x

    it "(The difference of x and y) have no elements in common with y" $ property $
      \x y -> isEmpty $ intersectionSet (differenceSet (x :: Set Int) (y :: Set Int)) y
   



--main :: IO ()
--main = hspec $ do
--  describe "Test intersectionSet" $ do
--    it "test if " $ property $ property $
--      \x y -> subSet x (intersectionSet x y)


--    it "returns the first element of an *arbitrary* list" $
--      property $ \x xs -> head (x:xs) == (x :: Int)
   
test_test :: Set Int -> Bool
test_test x = True

-- 5

type Rel a = [(a,a)]
infixr 5 @@
(@@) :: Ord a => Rel a -> Rel a -> Rel a
r @@ s = nub [ (x,z) | (x,y) <- r, (w,z) <- s, y == w ]


trClos xs = fp (\ys -> nub $ sort ((xs @@ ys) ++ xs)) xs
-- this is the sypelest implementation we could think of,
-- We add xs1 = (xs0 @@ xs0 ++ xs0)
-- We add xs2 = (xs0 @@ xs1 ++ xs0)
-- We add xs3 = (xs0 @@ xs2 ++ xs0)
-- We add xs4 = (xs0 @@ xs3 ++ xs0)
-- till xsn == xs(n+1) because than we mapt all the possible relations

-- 5


--all_points [] = []
--all_points ((x1,x2):xs) = [x1,x2] ++ (all_points xs)

--test_trClos xs x z = [ (y,z) | (a,b) <- xs, a == y]

--test5 :: IO ()
--test5 = hspec $ do
--  describe "Prelude.head" $ do
--    it "all the original relations are in there" $ do
--      quickCheck 

--    it "returns the first element of an *arbitrary* list" $
--      property $ \x xs -> head (x:xs) == (x :: Int)
   

-- 5
-- the best way to test if our trClos is working is making a random path, and look if its available or not
--xs
--ys = trClos
--points = nub $ all_points xs
--(elem rel ys) == any 



-- BONUS

-- This is Newton's Method
-- The idea behind Newton's Method is. Propose we need to calculate sqrt(5), this is the same question as
-- calculating f(x) = x^2 - 5 = 0

-- the method of lineair approximation says that f(x) ===== f(y) + f'(x)(x - y)
-- this is                                       f(x) ===== yy - 5 + 2xx - 2xy


