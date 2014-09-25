module Lab4 where 

import SetOrd
import Data.List


-- Assignment 1.
-- I still have to learn the relation rules, further more, we had this in our bachelor.
-- I was also confused with assignment 2.2 of the Workshop, but that is now no problem.

-- Assignment 2.
-- Time spent: 2 hours

-- Assignment 3.
-- randSet n = 

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



-- Assingment 5.
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
trClos' r nxt tot = if comp `allIn` tot then tot else trClos' r comp (relUnion comp tot)
  where comp = r @@ nxt

trClos :: Ord a => Rel a -> Rel a
trClos r = trClos' r r r

testTrClos r = (r @@ r) `allIn` (trClos r)

-- Bonus
fp :: Eq a => (a -> a) -> a -> a
fp f = \ x -> if x == f x then x else fp f (f x)
