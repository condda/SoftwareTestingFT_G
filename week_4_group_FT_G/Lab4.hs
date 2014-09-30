-- DONE: 1,2,3,4,5,7,8
-- TODO: 6

module Lab4 where 

import Control.Applicative

import Test.QuickCheck
import Test.Hspec
import SetOrd
import Data.List
import System.Random

-- Assignment 1.
-- None.

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

instance (Arbitrary a, Ord a) => Arbitrary (Set a) where
  arbitrary = sized $ \n ->
    do k <- choose (0,n)
       list2set <$> sequence [ arbitrary | _ <- [1..k] ]

-- Test using:
--
--   quickCheck testIsSet

-- Assignment 4.
setIntersect :: Ord a => Set a -> Set a -> Set a
setIntersect _ (Set []) = Set []
setIntersect (Set []) _ = Set []
setIntersect setA (Set (b:bs))
  | inSet b setA = insertSet b (setIntersect setA (Set bs))
  | otherwise    = setIntersect setA (Set bs)

setUnion :: Ord a => Set a -> Set a -> Set a
setUnion (Set x) (Set y) = list2set (x ++ y)

setDiff :: Ord a => Set a -> Set a -> Set a
setDiff as (Set []) = as
setDiff as (Set (b:bs)) = deleteSet b $ setDiff as (Set bs)

-- Not only QuickCheck, but also HSpec!
testIntersect :: IO()
testIntersect = hspec $ do
  describe "Intersect (a, b)" $ do
    it "should should be a subset of a and b." $ property $
      \x y -> testSimpleIntersect (x :: Set Int) (y :: Set Int)

testUnion = hspec $ do
  describe "Union (a, b)" $ do
    it "should be the superset of a and of b." $ property $
       \x y -> testSubsetUnion (x:: Set Int) (y :: Set Int)

testDifference = hspec $ do
  describe "Difference (a, b)" $ do
    it "should be a subset of a, but not per sé of b" $ property $
      \x y -> testDifferenceSubset (x :: Set Int) (y :: Set Int)

testSimpleIntersect a b = subSet (setIntersect a b) a && subSet (setIntersect a b) b

testSubsetUnion a b = subSet a (setUnion a b) && subSet b (setUnion a b)

testDifferenceSubset a b = subSet (setDiff a b) a

-- Test using quickCheck:
--
-- quickCheck testSimpleIntersect
-- quickCheck testSubsetUnion
-- quickCheck testDifferenceSubset

-- Assignment 5. - Time spent: 30 mins.
type Rel a = [(a,a)]

infixr 5 @@

fp :: Eq a => (a -> a) -> a -> a
fp f x
  | x == f x = x 
  | otherwise = fp f (f x)

(@@) :: Eq a => Rel a -> Rel a -> Rel a

r @@ s = nub [ (x,z) | (x,y) <- r, (w,z) <- s, y == w ]

trClos xs = fp (\ys -> nub $ sort ((xs @@ ys) ++ xs)) xs

set2list (Set s) = s

allIn :: Eq a => Rel a -> Rel a -> Bool
allIn [] s = True
allIn (r:rs) s = r `elem` s && rs `allIn` s

-- Assignment 6. - Time spent: 30 mins.

testTrClos :: IO()
testTrClos = hspec $ do
  describe "Transitive closure" $ do
    it "contains R." $ property $
      \x -> testTrClosRinR (x :: [(Int, Int)])

    it "is transitive." $ property $
      \x -> testTrClosRoRinR (x :: [(Int, Int)])

    it "is minimal." $ property $
      \x -> testTrClosIsMinimal (x :: [(Int, Int)])


-- Assignment 7.
-- Yes, we can use QuickCheck:

testTrClosRinR :: [(Int, Int)] -> Bool
testTrClosRinR rNotUnique = r `allIn` (trClos r)
  where r = nub rNotUnique

testTrClosRoRinR :: [(Int, Int)] -> Bool
testTrClosRoRinR rNotUnique = (trC @@ trC) `allIn` trC
  where r = nub rNotUnique
        trC = trClos r

testTrClosIsMinimal :: [(Int, Int)] -> Bool
testTrClosIsMinimal rNotUnique = True -- TODO
  where r = nub rNotUnique
        trC = trClos r

-- quickCheck testTrClosRinR
-- quickCheck testTrClosRoRinR

-- Assignment 8. Bonus
-- this function calls itself until x == f x, and returns the found x.
-- (\ x -> ((x + a/x) / 2)) performs the so-called Babylonian method,
-- which, for each recursive step, converges to the square root of x.
-- Thus, the more often it is called, the more precise the outcome becomes.
-- The highest precision on a computer is reached when f(x) = x (i.e.
-- the number does not change anymore), which is exactly what this
-- function call does.