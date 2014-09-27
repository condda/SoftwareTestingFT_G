
module Lab4 where 
import Test.QuickCheck
import Test.Hspec
import System.Random
import Data.List
import SetOrd



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


instance Arbitrary a => Arbitrary (Set a) where
  arbitrary = do
    x <- arbitrary
    return $ (Set [x, x, x])


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


test_union_is_subSet_of_x x y = subSet (unionSet x y) x
test_union_is_subSet_of_y x y = subSet (unionSet x y) y

-- x ^ y = z => z < y && z < x


unionSet :: Ord a => Set a -> Set a -> Set a
unionSet (Set x) (Set y) = list2set (x ++ y)

test_difference_is_subset_of_x x y = subSet (differenceSet x y) x
test_difference_has_nothing_in_common_with_y x y = isEmpty $ intersectionSet (differenceSet x y) y


differenceSet :: Ord a => Set a -> Set a -> Set a
differenceSet (Set x) (Set y) = Set (difference' x y)
difference' [] _ = []
difference' xs [] = xs
difference' (x:xs) (y:ys)
  | x == y = (difference' xs ys)
  | x >  y = (difference' (x:xs) ys)
  | x <  y = x:(difference' xs (y:ys))


test_intersection_is_subSet_of_y x y = subSet y (intersectionSet x y)

--main :: IO ()
--main = hspec $ do
--  describe "Test intersectionSet" $ do
--    it "test if " $ property $ property $
--      \x y -> subSet x (intersectionSet x y)


--    it "returns the first element of an *arbitrary* list" $
--      property $ \x xs -> head (x:xs) == (x :: Int)
   


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

all_points [] = []
all_points ((x1,x2):xs) = [x1,x2] ++ (all_points xs)

-- 5
-- the best way to test if our trClos is working is making a random path, and look if its available or not
--xs
--ys = trClos
--points = nub $ all_points xs
--(elem rel ys) == any 

--main :: IO ()
--main = hspec $ do
--  describe "Prelude.head" $ do
--    it "all the original relations are in there" $ do
--      True

--    it "returns the first element of an *arbitrary* list" $
--      property $ \x xs -> head (x:xs) == (x :: Int)
   





-- BONUS

-- This is Newton's Method
-- The idea behind Newton's Method is. Propose we need to calculate sqrt(5), this is the same question as
-- calculating f(x) = x^2 - 5 = 0

-- the method of lineair approximation says that f(x) ===== f(y) + f'(x)(x - y)
-- this is                                       f(x) ===== yy - 5 + 2xx - 2xy


