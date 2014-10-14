module Lab3 where

import Data.List
import Data.Char
import System.Random
import Week3

-- 1
contradiction :: Form -> Bool
contradiction f = not (satisfiable f)

tautology :: Form -> Bool
tautology f = all (\ v -> eval v f) (allVals f)

-- -- logical entailment ( a->b <=> not a || b)
entails :: Form -> Form -> Bool
entails f g =  all (\ v -> eval v h) (allVals h)
	where h = Impl f g

-- -- logical equivalence
equiv :: Form -> Form -> Bool
equiv f g = all (\v -> eval v h) (allVals h)
	where h = Equiv f g
	
testContradiction = contradiction (Cnj [p, Neg p])
testTautology = tautology (Dsj [p, Neg p])
testEquiv = equiv (Impl p q) (Impl (Neg q) (Neg p)) 
testEntails = entails 

-- 2

cnf' :: Form -> Form
cnf' (Prop f) = Prop f
cnf' (Neg (Prop f)) = (Neg (Prop f))
cnf' (Cnj [f,g]) = Cnj [cnf' f, cnf' g]
cnf' (Dsj [f,g]) = dist (cnf' f) (cnf' g)

dist :: Form -> Form -> Form
dist (Cnj [f,g]) h = Cnj [dist f h, dist g h]
dist f (Cnj [g,h]) = Cnj [dist f g, dist f h]
dist f g = Dsj [f,g]

cnf :: Form -> Form
cnf f =  cnf' $ nnf $ arrowfree f


-- 3

isLiteral :: Form -> Bool
isLiteral (Prop f) = True
isLiteral (Neg (Prop f)) = True
isLiteral _ = False

isDisjunctive :: Form -> Bool
isDisjunctive (Dsj xs) = all isDisjunctive xs
isDisjunctive f = isLiteral f 

isConjunctive :: Form -> Bool
isConjunctive (Cnj xs) = all isConjunctive xs
isConjunctive f = isDisjunctive f

testCNF f = isConjunctive (cnf f)

--4
type Clause = [Int]
type Clauses = [Clause]

literal2cls :: Form -> Int
literal2cls (Prop f) = f
literal2cls (Neg (Prop f)) = -f

dsj2cls :: Form -> Clause
dsj2cls (Dsj xs) = foldr (\x ys -> (literal2cls x : ys)) [] xs
dsj2cls f = [literal2cls f]

cnf2cls :: Form -> Clauses
cnf2cls (Cnj xs) = foldr (\x ys -> (dsj2cls x : ys)) [] xs
