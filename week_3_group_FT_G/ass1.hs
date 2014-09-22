module Ass3 where

import Week3
import GHC.Exts

-- Assignment 1. (ca. 1:00)
contradiction :: Form -> Bool
contradiction f = not (satisfiable f)

tautology :: Form -> Bool
tautology f = all (\ v -> eval v f) (allVals f)

entails' :: [Valuation] -> Form -> Form -> Bool
entails' [] f1 f2 = True
entails' (v:vs) f1 f2 = ((not (eval v f2)) || (eval v f1)) && (entails' vs f1 f2)

entails :: Form -> Form -> Bool
entails f1 f2 = entails' (allVals (Cnj [f1, f2])) f1 f2

equiv' :: [Valuation] -> Form -> Form -> Bool
equiv' [] f1 f2 = True
equiv' (v:vs) f1 f2 = (eval v f1) == (eval v f2) && (equiv' vs f1 f2)

equiv :: Form -> Form -> Bool
equiv f1 f2 = (equiv' (allVals (Cnj [f1, f2])) f1 f2)

-- Description of method for checking the definitions:
--
-- The testRules-function is used for testing the implemented functions
-- for (n+1)*3 different formulas. The function testRules2 tests
-- equivalence and entails for two different formulas, whereas
-- testRules (n | n > 0) tests equivalence and entails for one (equal) formula
-- and the rules:
--
--   satisfiable x -> -contradiction x
--   -satisfiable x -> contradiction x & -tautology x
--   tautology x -> satisfiable x & -contradiction x
--
-- Testing equivalence and entailment uses the following tests for two forms x
-- and y:
--
--   x entails (Dsj [])                    (True/False |= False)
--   (Cnj []) entails x                    (True |= True/False)
--   x |= y & y |= x -> x == y             (equivalence)
--   x \= y -> (!(x |= y) || !(y |= x))    (not equivalent)

testCont x = (satisfiable x) && (not $ contradiction x)
          || (not $ satisfiable x) && (contradiction x) && (not (tautology x))

testTaut x = (tautology x) && (satisfiable x) && (not (contradiction x))
          || (not (tautology x))

testEquivEntails x y
  = (entails x (Dsj []))
 && (entails (Cnj []) x)
 && (((entails x y) && (entails y x) && (equiv x y)) || 
    (((not $ entails x y) || (not $ entails y x)) && (not $ equiv x y)))

testRules' x = (testCont x) && (testTaut x)

testRules2 = do
  x <- getRandomFSmpl
  y <- getRandomFSmpl
  return $ (testEquivEntails x y)

testRules 0 = do
  x <- getRandomFSmpl
  return $ (testEquivEntails x x) && testRules' x

testRules n = do
  x <- testRules 0
  y <- testRules2
  z <- testRules (n - 1)
  return $ x && y && z



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

-- Here we replace empty disjuncts and conjuncts by equivalent
-- non-empty disjuncts and disjuncts, using the knowledge, that:
--   The empty disjunction is false (any of empty).
--   The empty conjunction is true (all of empty).
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

-- Normalization of a CNF means removing all redundant parentheses.
-- This is exactly what normCnf does.
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

-- Assignment 3: (~ 2:00)
cnfTest cnfF orig = (grC cnfF)
  || error ("Error in Grammar! 'CNF':\n\n" ++ (show cnfF)
  ++ "\n\nOriginal:\n\n" ++ (show orig))


-- Assignment 3:
equivAndFstCnfGrammar x y
  | (grC x) && (equiv x y) = True
  | (grC x) = error ("Error in Equivalence! 'CNF':\n\n" ++ (show x) ++ "\n\nOriginal:\n\n" ++ (show y))
  | (equiv x y) = error ("Error in Grammar! 'CNF':\n\n" ++ (show x) ++ "\n\nOriginal:\n\n" ++ (show y))
  | otherwise = error ("Error in Grammar and Equivalence! 'CNF':\n\n" ++ (show x) ++ ", original:\n\n" ++ (show y))

testCnf 0 = do
  x <- getRandomFSmpl
  return $ equivAndFstCnfGrammar (cnf x) x

-- Function for performing n tests on both the grammar and equality.
testCnf n = do
  y <- testCnf 0
  z <- testCnf (n - 1)
  return $ y && z

-- These sets of tests can test the CNF grammar. We use (a:as) instead of (as) 
-- to ensure the list contains at least one parameter. Cnjs in Cnjs or Dsjs in
-- Dsjs are NOT allowed.
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

literal2cls :: Form -> Int
literal2cls (Prop f) = f
literal2cls (Neg f) = - (literal2cls f)
literal2cls _ = error "literal2cls"
 
disjunctive2cls :: Form -> Clause
disjunctive2cls (Dsj xs) = foldr (\y x -> x ++ (disjunctive2cls y)) [] xs
disjunctive2cls f = [literal2cls f]
 
conjunctive2cls :: Form -> Clauses
conjunctive2cls (Cnj xs) = foldr (\y x -> x ++ (conjunctive2cls y)) [] xs
conjunctive2cls f = [disjunctive2cls f]

cnf2cls = conjunctive2cls

clause2lst [] = []
clause2lst (c:cs) | c < 0 = (Neg (Prop (-c))):(clause2lst cs)
                  | otherwise = (Prop c):(clause2lst cs)

clause2dsjs cs = Dsj (clause2lst cs)
cls2form (cs) = Cnj [clause2dsjs c | c <- cs]

-- We test the cnf2cls-function by converting the found clauses back to forms (using cls2form)
-- and then checking the equivalence between the original and the found form.
--
-- The following can simply be tested (with 100 the number of tests) by:
--
-- testForms 100 test_cnf2cls
--
-- In our case, the results were all True.
test_cnf2cls f = equiv f (cls2form $ cnf2cls (cnf f))


-- BONUS Assignment: during the lecture it was stated that building a SAT-solver is the bonus assignment.

cls_replace :: Int -> [[Int]] -> [[Int]]
cls_replace x ys = filter (\y -> not (any (x ==) y)) (map (cls_replace_neg x) ys)

cls_replace_neg :: Int -> [Int] -> [Int]
cls_replace_neg x ys = filter (-x /=) ys

contradictions [x] = True
contradictions [] = False
contradictions (x:y:ys)
  | x == -y = False
  | otherwise = contradictions (y:ys)

dpll :: Clauses -> Bool
dpll [] = True
dpll yy
  | all (\x -> length x == 1) yy = contradictions (sortWith abs (map (\x -> head x) yy))
  | any (\x -> length x == 0) yy = False
  | otherwise = dpll((cls_replace y yy) ++ [[y]]) || dpll((cls_replace (-y) yy) ++ [[-y]])
    where y = head $ head $ yy



-- Here we test for each f that was found through satisfiable, if it equals to dpll (the SAT-solver whichs checks if it can be satified).
--
-- The following can simply be tested (with 100 the number of tests) by:
--
-- testForms 100 test_dpll_equals_satisfiable
--
-- In our case, the results were all True.
test_dpll_equals_satisfiable f = (satisfiable f) == (dpll $ cnf2cls $ cnf f)
