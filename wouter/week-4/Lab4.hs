-- DONE: 1,2,3,5,7,8
-- TODO: 4,6

module Lab4 where 

import           Control.Applicative

import Test.QuickCheck
import Test.Hspec
import SetOrd
import Data.List
import System.Random

-- Assignment 1.
-- I still have to learn the relation rules, further more, we had this in our bachelor.
-- I was also confused with assignment 2.2 of the Workshop, but that is now no problem.

-- Assignment 2.
-- Time spent: 2 hours

-- Assignment 3.
-- Time spent: 2 hours

getRandomSet :: IO(Set Int)
getRandomSet = do
  gen <- newStdGen
  return $ list2set $ take (head $ randomRs (3, 20) gen) $ (randomRs (0, 20) gen)

testIsSet :: Set Int -> Bool
testIsSet set = (list2set (set2list set)) == set

-- Test using:
--
--   quickCheck testIsSet

instance (Arbitrary a, Ord a) => Arbitrary (Set a) where
  arbitrary = sized $ \n ->
    do k <- choose (0,n)
       list2set <$> sequence [ arbitrary | _ <- [1..k] ]

-- Assignment 4.
setIntersect :: Ord a => Set a -> Set a -> Set a
setIntersect _ (Set []) = Set []
setIntersect (Set []) _ = Set []
setIntersect setA (Set (b:bs))
  | inSet b setA = insertSet b (setIntersect setA (Set bs))
  | otherwise    = setIntersect setA (Set bs)

setUnion :: Ord a => Set a -> Set a -> Set a
setUnion as (Set []) = as
setUnion as (Set (b:bs)) = insertSet b $ setUnion as (Set bs)

setDiff :: Ord a => Set a -> Set a -> Set a
setDiff as (Set []) = as
setDiff as (Set (b:bs)) = deleteSet b $ setDiff as (Set bs)
-- Time spent initial implementation: < 20 mins.

-- TODO more tests
testIntersect :: Set Int -> Set Int -> Bool
testIntersect a b =
  (subSet c a) && (subSet c b)
  where c = setIntersect a b



-- Assignment 5. - Time spent: 30 mins.
type Rel a = [(a,a)]

infixr 5 @@

(@@) :: Eq a => Rel a -> Rel a -> Rel a

r @@ s = nub [ (x,z) | (x,y) <- r, (w,z) <- s, y == w ]

set2list (Set s) = s

allIn :: Eq a => Rel a -> Rel a -> Bool
allIn [] s = True
allIn (r:rs) s = r `elem` s && rs `allIn` s

relUnion :: Ord a => Eq a => Rel a -> Rel a -> Rel a
relUnion r s = set2list (setUnion (list2set r) (list2set s))

trClos' :: Ord a => Rel a -> Rel a -> Rel a -> Rel a
trClos' r nxt tot = if comp `allIn` tot
                    then tot
                    else trClos' r comp (relUnion comp tot)
  where comp = r @@ nxt

trClos :: Ord a => Rel a -> Rel a
trClos r = trClos' r r r


-- Yes, we can use QuickCheck:
-- quickCheck trClos

testTrClos :: [(Int, Int)] -> Bool
testTrClos rNotUnique = (r @@ r) `allIn` (trClos r)
  where r = nub rNotUnique

-- Assignment 8. Bonus
-- this function calls itself until x == f x, and returns the found x.
-- (\ x -> ((x + a/x) / 2)) performs the so-called Babylonian method,
-- which, for each recursive step, converges to the square root of x.
-- Thus, the more often it is called, the more precise the outcome becomes.
-- The highest precision on a computer is reached when f(x) = x (i.e.
-- the number does not change anymore), which is exactly what this
-- function call does.
