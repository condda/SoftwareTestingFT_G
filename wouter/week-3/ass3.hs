module Ass3 where

import Week3

-- Assignment 1. (TIME SPENT: 0:30)
-- Contradiction --
contradiction :: Form -> Bool
contradiction f = not (satisfiable f)

-- Tautology --
tautology :: Form -> Bool
tautology f = all (\ v -> eval v f) (allVals f)

-- Entails --
entails' :: [Valuation] -> Form -> Form -> Bool
entails' [] f1 f2 = True
entails' (v:vs) f1 f2 = (not (eval v f2)) || (eval v f1) && (entails' vs f1 f2)


-- Entails: a |= b (which is equal to b -> a and (not b) || a).
entails :: Form -> Form -> Bool
entails f1 f2
	| (allVals f1) == (allVals f2) = (equiv' (allVals f1) f1 f2)
	| otherwise = False

-- Equiv --
equiv' :: [Valuation] -> Form -> Form -> Bool
equiv' [] f1 f2 = True
equiv' (v:vs) f1 f2 = (eval v f1) == (eval v f2) && (equiv' vs f1 f2)

-- Equiv: a <-> b
equiv :: Form -> Form -> Bool
equiv f1 f2
	| (allVals f1) == (allVals f2) = (equiv' (allVals f1) f1 f2)
	| otherwise	= False

-- Simple De Morgan test.
testEquivSimple :: Bool
testEquivSimple = equiv
	(Neg (Dsj [(Prop 1), (Prop 2)]))
	(Cnj [(Neg (Prop 1)), (Neg (Prop 2))])

-- Assignment 2. (TIME SPENT: 10:00)
-- Step 3. --
-- VERY evil: "the empty disjunction (OR-ing over an empty set of operands) is often defined as having the result 0." - Wikipedia.

cnf :: Form -> Form
cnf (Neg (Prop x)) = Neg (Prop x)
cnf (Prop x) = Prop x
cnf (Cnj xs) = Cnj (map cnf xs)
cnf (Dsj []) = testInCNFAndDo "Dsj []" (Dsj [])
cnf (Dsj [x1]) = testInCNFAndDo "Dsj [x1]" x1
cnf (Dsj (x1:x2:[])) = testInCNFAndDo "Dsj (x1:x2:[])" (dist x1 x2)
cnf (Dsj (x1:x2:xs)) = testInCNFAndDo "Dsj (x1:x2:xs)" (cnf (Dsj ((dist x1 x2):xs)))

dist :: Form -> Form -> Form
dist (Cnj vs) f2 = testInCNFAndDo "(Cnj vs) f2" (Cnj [(dist v f2) | v <- vs])
dist f1 (Cnj vs) = testInCNFAndDo "f1 (Cnj vs)" (Cnj [(dist f1 v) | v <- vs])
dist x1 x2 = testInCNFAndDo "x1 x2" (Dsj [x1, x2])

to_cnf :: Form -> Form
to_cnf xs = cnf $ nnf $ arrowfree xs

testInCNFAndDo :: String -> Form -> Form
testInCNFAndDo a b
  | (gramC b) = b
  | otherwise = error a


equivAndFstGoodGrammar x y
  | (gramC x) = True
  | (gramC x) = error ("Error in Equivalence! 'CNF':\n\n" ++ (show x) ++ "\n\nOriginal:\n\n" ++ (show y))
  | (equiv x y) = error ("Error in Grammar! 'CNF':\n\n" ++ (show x) ++ "\n\nOriginal:\n\n" ++ (show y))
  | otherwise = error ("Error in Grammar and Equivalence! 'CNF':\n\n" ++ (show x) ++ ", original:\n\n" ++ (show y))

testCnf 0 = do
  x <- getRandomFSmpl
  return $ equivAndFstGoodGrammar (to_cnf x) x

-- Assignment 3: Testing assignment 2.
-- We think that the most important properties are:
-- - Equivalence of CNF(X) and x
-- - The grammar should be conform to:
--   - L ::= p | -p
--   - D ::= L | L OR D
--   - C ::= D | D AND C

testCnf n = do
  y <- testCnf 0
  z <- testCnf (n - 1)
  return $ y && z -- Yes, I am aware of the fact that y && z won't do anything for the performance here...

-- These sets of sets can test a grammar.
--   - L ::= p | -p
gramL (Prop p) = True
gramL (Neg (Prop p)) = True
gramL _ = False

--   - D ::= L | L OR D
gramD (Dsj (as)) = all (\x -> x) [ gramD x | x <- as ]
--gramD (Cnj _) = error "Cnj in Dsj"
gramD x = gramL x

--   - C ::= D | D AND C
gramC (Cnj (as)) = all (\x -> x) [ gramC x | x <- as ]
gramC x = gramD x

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
