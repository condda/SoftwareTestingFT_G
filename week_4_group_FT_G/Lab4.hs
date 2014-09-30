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

-- Random Data Generator from scratch:
getRandomSet :: IO(Set Int)
getRandomSet = do
  gen <- newStdGen
  return $ list2set $ take (head $ randomRs (3, 20) gen) $ (randomRs (0, 20) gen)

set2list (Set s) = s

testIsSet :: Set Int -> Bool
testIsSet set = (list2set (set2list set)) == set

-- Random Data Generator for QuickCheck:
instance (Arbitrary a, Ord a) => Arbitrary (Set a) where
  arbitrary = sized $ \n ->
    do k <- choose (0,n)
       list2set <$> sequence [ arbitrary | _ <- [1..k] ]

-- Test using:
--
--   quickCheck testIsSet

-- Assignment 4. Time spent: 2 hours
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
  describe "setIntersect (a, b)" $ do
    it "should should be a subset of a and b." $ property $
      \x y -> testSimpleIntersect (x :: Set Int) (y :: Set Int)

    it "should hold that the intersection of a and b is equivalent to the negation of the union of diff a b and diff b a iff setIntersect, setUnion and setDifference are correctly implemented." $ property $
      \x y -> testEquivalenceRelations x y

testUnion = hspec $ do
  describe "setUnion (a, b)" $ do
    it "should be the superset of a and of b." $ property $
       \x y -> testSubsetUnion (x:: Set Int) (y :: Set Int)

    it "should hold that the intersection of a and b is equivalent to the negation of the union of diff a b and diff b a iff setIntersect, setUnion and setDifference are correctly implemented." $ property $
      \x y -> testEquivalenceRelations x y

testDifference = hspec $ do
  describe "setDifference (a, b)" $ do
    it "should be a subset of a, but not per sé of b" $ property $
      \x y -> testDifferenceSubset (x :: Set Int) (y :: Set Int)

    it "should hold that the intersection of a and b is equivalent to the negation of the union of diff a b and diff b a iff setIntersect, setUnion and setDifference are correctly implemented." $ property $
      \x y -> testEquivalenceRelations x y

testSimpleIntersect :: Set Int -> Set Int -> Bool
testSimpleIntersect a b = subSet (setIntersect a b) a && subSet (setIntersect a b) b

testSubsetUnion :: Set Int -> Set Int -> Bool
testSubsetUnion a b = subSet a (setUnion a b) && subSet b (setUnion a b)

testDifferenceSubset :: Set Int -> Set Int -> Bool
testDifferenceSubset a b = subSet (setDiff a b) a

testEquivalenceRelations :: Set Int -> Set Int -> Bool
testEquivalenceRelations a b = (setIntersect a b) == (setDiff (setUnion a b) (setUnion (setDiff a b) (setDiff b a)))

-- Test using quickCheck:
--
-- quickCheck testSimpleIntersect
-- quickCheck testSubsetUnion
-- quickCheck testDifferenceSubset
-- quickCheck testEquivalenceRelations

-- Assignment 5. - Time spent: 30 mins.

type Rel a = [(a,a)]

infixr 5 @@

fp :: Eq a => (a -> a) -> a -> a
fp f x
  | x == f x = x 
  | otherwise = fp f (f x)

(@@) :: Eq a => Rel a -> Rel a -> Rel a

r @@ s = nub [ (x,z) | (x,y) <- r, (w,z) <- s, y == w ]

-- Our implementation
trClos :: Ord a => Rel a -> Rel a
trClos xs = fp (\ys -> nub $ sort ((xs @@ ys) ++ xs)) xs


-- Assignment 6. - Time spent: 60 mins.
-- "In mathematics, the transitive closure of a binary relation R on a set X is the transitive relation R+ on set X such that R+ contains R and R+ is minimal (Lidl and Pilz 1998:337)."
-- Source: http://en.wikipedia.org/wiki/Transitive_closure
testTrClos :: IO()
testTrClos = hspec $ do
  describe "Transitive closure" $ do
    it "contains R." $ property $
      testTrClosRinR

    it "is transitive." $ property $
      testTrClosRoRinR

    it "is minimal." $ property $
      testTrClosIsMinimal


-- Assignment 7.
-- Yes, we can use QuickCheck:

testTrClosRinR :: [(Int, Int)] -> Bool
testTrClosRinR r = r `isInfixOf` (trClos r)

testTransitive :: [(Int, Int)] -> Bool
testTransitive r = (r @@ r) `isInfixOf` r

testTrClosRoRinR :: [(Int, Int)] -> Bool
testTrClosRoRinR rNotUnique = testTransitive $ trClos (nub rNotUnique)

testTrClosIsMinimal :: [(Int, Int)] -> Bool
testTrClosIsMinimal rNotUnique = (1==) $ length $ (filter testTransitive (map (r ++) (powerList $ ((trClos r) \\ r))))
  where r = nub rNotUnique

-- Test using quickCheck:
--
-- quickCheck testTrClosRinR
-- quickCheck testTransitive
-- quickCheck testTrClosRoRinR
-- quickCheck testTrClosIsMinimal

-- Copied from SetOrd.hs (instead of exposing this function from the module, since this is not a set operation).
powerList :: [a] -> [[a]]
powerList [] = [[]]
powerList (x:xs) = (powerList xs) 
                     ++ (map (x:) (powerList xs))


-- Assignment 8. Bonus
-- this function calls itself until x == f x, and returns the found x.
-- (\ x -> ((x + a/x) / 2)) performs the so-called Babylonian method,
-- which, for each recursive step, converges to the square root of x.
-- Thus, the more often it is called, the more precise the outcome becomes.
-- The highest precision on a computer is reached when f(x) = x (i.e.
-- the number does not change anymore), which is exactly what this
-- function call does.