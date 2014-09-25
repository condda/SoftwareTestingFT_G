module Lab4 where 

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

-- TODO: setUnion not checked!!
createSubSet 0 as = Set []
createSubSet len as = setUnion subs (createSubSet restLen (drop len as))
  where subs = list2set $ take len as
        subsLst = set2list subs
        restLen = len - (length subsLst)

randSet len min max
  | len < 0 = error "length cannot be lower than 0."
  | otherwise = do
    g <- newStdGen
    return $ createSubSet len (randomRs (min, max) g)

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



-- Assingment 5. - Time spent: 30 mins.
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
-- this function calls itself until x == f x, and returns the found x.
-- (\ x -> ((x + a/x) / 2)) performs the so-called Babylonian method,
-- which, for each recursive step, converges to the square root of x.
-- Thus, the more often it is called, the more precise the outcome becomes.
-- The highest precision on a computer is reached when f(x) = x (i.e.
-- the number does not change anymore), which is exactly what this
-- function call does.
