module Lab4 where 

import SetOrd
import Data.List
import System.Random
import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)
import Control.Applicative

--3
instance (Arbitrary a, Ord a) => Arbitrary (Set a) where
  arbitrary = sized $ \n ->
    do k <- choose (0,n)
       list2set <$> sequence [ arbitrary | _ <- [1..k] ]
	   
--4 Intersection, union and difference
setIntersection :: (Ord a) => Set a -> Set a -> Set a
setIntersection (Set[]) (Set a) = Set[]
setIntersection (Set a) (Set[]) = Set[]
setIntersection (Set (x:xs)) setB | inSet x setB = insertSet x (setIntersection (Set xs) setB)
                                  | otherwise = setIntersection (Set xs) setB

setUnion :: (Ord a) => Set a -> Set a -> Set a
setUnion (Set []) (Set []) = (Set [])
setUnion (Set[]) (Set a) = (Set a)
setUnion (Set a) (Set []) = (Set a)
setUnion (Set (x:xs)) setB = insertSet x (setUnion (Set xs) setB)

setDifference :: (Ord a) => Set a -> Set a -> Set a
setDifference (Set[]) (Set a) = Set[]
setDifference (Set a) (Set[]) = (Set a)
setDifference (Set (x:xs)) setB | not(inSet x setB) = insertSet x (setDifference (Set xs) setB)
                                | otherwise = (setDifference (Set xs) setB)
								
--INVARIANT FUNCTION FOR EACH OPERATION... QUICKCHECK FUNCTION. A intersect B = subset A || B
test_intersection :: Set Int -> Set Int -> Bool
test_intersection setA setB = subSet (setIntersection setA setB) setA && subSet (setIntersection setA setB) setB

-- Vx E A ^ not E B
test_difference :: Set Int -> Set Int -> Bool
test_difference setA setB = subSet (setIntersection setA setB) setA
-- test_difference (Set[]) _ = True
-- test_difference (Set(x:xs)) setB
  -- | not (inSet x  setB) = test_difference (Set xs) setB
  -- | otherwise = False

test_union :: Set Int -> Set Int -> Bool
test_union setA setB = subSet setA (setUnion setA setB) && subSet setB (setUnion setA setB)
								
test4 :: IO ()
test4 = hspec $ do
 describe "Test For exercise 4" $ do
  it "intersection" $ do
   setIntersection (Set [1,2,3,4]) (Set [-100..100]) == Set [1,2,3,4]	
  it "union" $ do
   setUnion (Set [1,2,3,4]) (Set [5,6,7]) == Set [1,2,3,4,5,6,7]
  it "difference" $ do
   setDifference (Set [1,2,3]) (Set [3,6,7]) == Set [1,2]
  
  
 -- 5

type Rel a = [(a,a)]

infixr 5 @@
(@@) :: Ord a => Rel a -> Rel a -> Rel a
r @@ s = nub [ (x,z) | (x,y) <- r, (w,z) <- s, y == w ]

trClos :: Ord a => Rel a -> Rel a
trClos  [] = []


