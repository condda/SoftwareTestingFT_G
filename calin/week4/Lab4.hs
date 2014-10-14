module Lab4 where 

import System.Random
import Data.List
import SetOrd
import Test.QuickCheck
import Test.Hspec
import Control.Applicative

fp :: Eq a => (a -> a) -> a -> a
fp f = \ x -> if x == f x then x else fp f (f x)

getRandomInt :: Int -> IO Int
getRandomInt n = getStdRandom (randomR (0,n))

--3
getRandomSet :: IO (Set Int)
getRandomSet = do
	gen <- newStdGen
	return $ list2set $ take 10 $ randoms gen


instance (Arbitrary a, Ord a) => Arbitrary (Set a) where
	arbitrary = sized $ \n -> 
		do 
			k <- choose (0,n)
			list2set <$> sequence [ arbitrary | _ <- [1..k] ]

--4

intersectSet :: (Ord a) => Set a -> Set a -> Set a
intersectSet (Set []) set2 = Set []
intersectSet set1 (Set []) = Set []
intersectSet (Set (x:xs)) set2  
		| inSet x set2 =  insertSet x (intersectSet (Set xs) set2)
		| otherwise  = intersectSet (Set xs) set2

--set union already implemented in SetOrd.hs

differenceSet :: (Ord a) => Set a -> Set a -> Set a
differenceSet (Set []) set2 = Set []
differenceSet (Set (x:xs)) set2 
		| not $ inSet x set2 = insertSet x (differenceSet (Set xs) set2)
		| otherwise = differenceSet (Set xs) set2

testIntersect :: Set Int -> Set Int -> Bool
testIntersect set1 set2 = (subSet inters set1) && (subSet inters set2)
							where inters = intersectSet set1 set2 

testUnion :: Set Int -> Set Int -> Bool
testUnion set1 set2 = (subSet set1 uni) && (subSet set2 uni)
						where uni = unionSet set1 set2

testDifference :: Set Int -> Set Int -> Bool
testDifference set1 set2 = (subSet diff set1) && ((intersectSet diff set2) == Set [])
							where diff = differenceSet set1 set2

-- QuickCheck

-- *Lab4> quickCheck testIntersect 
-- ++ OK, passed 100 tests.
-- *Lab4> quickCheck testUnion  
-- +++ OK, passed 100 tests.
-- *Lab4> quickCheck testDifference 
-- +++ OK, passed 100 tests.

--5

type Rel a = [(a,a)]

infixr 5 @@

(@@) :: Eq a => Rel a -> Rel a -> Rel a
r @@ s = nub [ (x,z) | (x,y) <- r, (w,z) <- s , y == w]

trClos :: Ord a => Rel a -> Rel a
trClos xs  
		| ys == xs = ys
		| otherwise =  trClos ys
	where ys = nub $ sort (( xs @@ xs ) ++ xs)

--6

-- for some reason giving indenting errors..

--testTrClos :: IO()
--testTrClos = hspec $ do
--describe "Transitive closure" $ do
-- it "contains R" $ property $ 
--  testTrClosR
-- it "is transitive" $ property $ 
--  testTrClosTrans

--7

testTrClosR :: Ord a => Rel a -> Bool
testTrClosR xs = all ( `elem` trClosure) xs
					where trClosure = trClos xs

testTrClosTrans :: Ord a => Rel a -> Bool
testTrClosTrans xs = all ( `elem` trClosure) (trClosure @@ trClosure)
					where trClosure = trClos xs

-- *Lab4> quickCheck testTrClosR
-- +++ OK, passed 100 tests.
-- *Lab4> quickCheck testTrClosTrans 
-- +++ OK, passed 100 tests.


