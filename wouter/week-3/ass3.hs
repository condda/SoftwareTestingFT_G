module Ass3 where

import Week3

-- Assignment 1. (TIME SPENT: 0:30)
contradiction :: Form -> Bool
contradiction f = not (satisfiable f)

tautology :: Form -> Bool
tautology f = all (\ v -> eval v f) (allVals f)

entails' :: [Valuation] -> Form -> Form -> Bool
entails' [] f1 f2 = True
entails' (v:vs) f1 f2 = (not (eval v f2)) || (eval v f1) && (entails' vs f1 f2)

entails :: Form -> Form -> Bool
entails f1 f2 = entails' (allVals (Cnj [f1, f2])) f1 f2

equiv' :: [Valuation] -> Form -> Form -> Bool
equiv' [] f1 f2 = True
equiv' (v:vs) f1 f2 = (eval v f1) == (eval v f2) && (equiv' vs f1 f2)

equiv :: Form -> Form -> Bool
equiv f1 f2 = (equiv' (allVals (Cnj [f1, f2])) f1 f2)

-- We can simply test the functions as following:
-- Assuming that satisfiable works, then for each random
-- Form, if satisfiable, then not tautology and vice versa.
testCont x = (satisfiable x) && (not $ contradiction x)
          || (not $ satisfiable x) && (contradiction x) && (not (tautology x))

testTaut x = (tautology x) && (satisfiable x) && (not (contradiction x))
          || (not (tautology x))

testEquivEntails x y
  = ((entails x y) && (entails y x) && (equiv x y)
 || ((not (entails x y)) || (not (entails y x))) && (not (equiv x y)))
 && (entails x (Dsj []))
 && (entails (Cnj []) x)



testRules' x = (testCont x) && (testTaut x)

testRules 0 = do
  x <- getRandomFSmpl
  return $ (testEquivEntails x x) && testRules' x

testRules n = do
  x <- testRules 0
  y <- testRules (n - 1)
  return $ x && y && (testEquivEntails x y)

-- TODO: Still a simple De Morgan test.
testEquivSimple :: Bool
testEquivSimple = equiv
	(Neg (Dsj [(Prop 1), (Prop 2)]))
	(Cnj [(Neg (Prop 1)), (Neg (Prop 2))])





-- Assignment 2. (TIME SPENT: 15:00)
cnf' :: Form -> Form
cnf' (Neg (Prop x)) = Neg (Prop x)
cnf' (Prop x) = Prop x
cnf' (Cnj xs) = Cnj (map cnf' xs)
cnf' (Dsj [x1]) = cnf' x1
cnf' (Dsj (x1:x2:[])) = dist (cnf' x1) (cnf' x2)
cnf' (Dsj (x1:x2:xs)) = cnf' (Dsj ((dist (cnf' x1) (cnf' x2)):xs))

dist :: Form -> Form -> Form
dist (Cnj vs) f2 = Cnj [(dist v f2) | v <- vs]
dist f1 (Cnj vs) = Cnj [(dist f1 v) | v <- vs]
dist x1 x2 = Dsj [x1, x2]

-- The empty disjunction is false (any of empty).
-- The empty conjunction is true (all of empty).
remEmptyDsjAndCnj' :: Form -> Name -> Form
remEmptyDsjAndCnj' (Dsj []) prop = Cnj [(Prop prop), (Neg (Prop prop))]
remEmptyDsjAndCnj' (Cnj []) prop = Dsj [(Prop prop), (Neg (Prop prop))]
remEmptyDsjAndCnj' (Cnj xs) prop = Cnj [(remEmptyDsjAndCnj' x prop) | x <- xs]
remEmptyDsjAndCnj' (Dsj xs) prop = Dsj [(remEmptyDsjAndCnj' x prop) | x <- xs]
remEmptyDsjAndCnj' xs prop = xs

createTautOrContr f | contradiction f = (Cnj [(Prop 1), (Neg (Prop 1))])
                    | otherwise       = (Dsj [(Prop 1), (Neg (Prop 1))])

remEmptyDsjAndCnj :: Form -> Form
remEmptyDsjAndCnj xs | length (propNames xs) == 0 = createTautOrContr xs
                     | otherwise = remEmptyDsjAndCnj' xs ((propNames xs) !! 0)

-- Normalization means removing all redundant parentheses.
remDsj :: [Form] -> [Form]
remDsj [] = []
remDsj ((Dsj x):xs) = (remDsj x) ++ (remDsj xs)
remDsj (x:xs) = (x:(remDsj xs))

normCnf' :: [Form] -> [Form]
normCnf' [] = []
normCnf' ((Dsj x):xs) = (Dsj (remDsj x)):(normCnf' xs)
normCnf' ((Cnj x):xs) = (normCnf' x) ++ (normCnf' xs)
normCnf' (x:xs) = (x:(normCnf' xs))

normCnf :: Form -> Form
normCnf (Cnj x) = Cnj (normCnf' x)
normCnf (Dsj x) = Dsj (remDsj x)
normCnf x = x

-- This is the function to convert any form to CNF.
cnf :: Form -> Form
cnf xs = normCnf $ cnf' $ remEmptyDsjAndCnj $ nnf $ arrowfree xs





-- Assignment 3:
equivAndFstGoodGrammar x y
  | (grC x) && (equiv x y) = True
  | (grC x) = error ("Error in Equivalence! 'CNF':\n\n" ++ (show x) ++ "\n\nOriginal:\n\n" ++ (show y))
  | (equiv x y) = error ("Error in Grammar! 'CNF':\n\n" ++ (show x) ++ "\n\nOriginal:\n\n" ++ (show y))
  | otherwise = error ("Error in Grammar and Equivalence! 'CNF':\n\n" ++ (show x) ++ ", original:\n\n" ++ (show y))

testCnf 0 = do
  x <- getRandomFSmpl
  return $ equivAndFstGoodGrammar (cnf x) x

-- Function for performing n tests on both the grammar and equality.
testCnf n = do
  y <- testCnf 0
  z <- testCnf (n - 1)
  return $ y && z

-- These sets of tests can test the CNF grammar.
grL (Prop p) = True
grL (Neg (Prop p)) = True
grL _ = False

grD (Dsj (a:as)) = all (\x -> x) [ grL x | x <- (a:as) ]
grD x = grL x

grC (Cnj (a:as)) = all (\x -> x) [ grD x | x <- (a:as) ]
grC x = grD x

-- Assignment 4
type Clause = [Int]
type Clauses = [Clause]

parseClause :: Form -> Clause
parseClause (Dsj []) = []
parseClause (Dsj ((Dsj xs):ys)) = parseClause (Dsj (xs++ys))
parseClause (Dsj ((Neg (Prop i)):xs)) = (-i:(parseClause (Dsj ((Prop i):xs))))
parseClause (Dsj ((Prop i):xs)) = (i:(parseClause (Dsj ((Prop i):xs))))
parseClause (Prop p) = [p]
parseClause (Neg (Prop p)) = [-p]
parseClause t = error ("Unexpected token: " ++ (show t))

cnf2cls :: Form -> Clauses
cnf2cls (Cnj []) = []
cnf2cls (Cnj ((Cnj xs):ys)) = cnf2cls (Cnj (xs ++ ys))
cnf2cls (Cnj (c:cs)) = (parseClause c):(cnf2cls (Cnj cs))





-- BONUS: during the lecture it was stated that building a SAT-solver is the bonus assignment.
-- Simply find a clause where two properties are contradictory.
-- If not found, this clause is solvable.
isClauseSolvable :: Clause -> Bool
isClauseSolvable xs = not (any (\t -> t) [x == -y | x <- xs, y <- xs])

-- Returns every combination for which this SAT is solvable.
-- Basically, as the clauses are in CNF, it returns the actual solution.
solveSAT :: Clauses -> Clauses
solveSAT [] = []
solveSAT (x:xs)
  | isClauseSolvable x	= (x:(solveSAT xs))
  | otherwise		= solveSAT xs

--testSolveSAT 0 = do
--  x <- getRandomFSmpl
--  return $ equivAndFstGoodGrammar (anyToCnf x) x

--testSolveSAT n = do
--  y <- testSolveSAT 0
--  z <- testSolveSAT (n - 1)
--  return $ y && z
