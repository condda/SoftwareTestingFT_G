module Lab3 where  

import Data.List
import Data.Char
import System.Random
import Week3



tautology :: Form -> Bool
tautology f = all (\ v -> eval v f) (allVals f)

contradiction :: Form -> Bool
contradiction x = not (satisfiable x)

-- -- logical entailment (A->B) => (¬A || B)
entails :: Form -> Form -> Bool
entails x y =  all (\ v -> eval v f) (allVals f)
	where f = Impl x y

-- logical equivalence
equiv :: Form -> Form -> Bool
equiv x y =  all (\ v -> eval v f) (allVals f)
	where f = Equiv x y
	
--2

cnf :: Form -> Form
cnf (Neg (Prop x)) = Neg (Prop x)
cnf (Prop f) = Prop f
cnf (Cnj xs) = Cnj (map cnf xs)
cnf (Dsj (x1:x2:[])) = dist (cnf x1) (cnf x2)
cnf (Dsj (x1:x2:xs)) = cnf (Dsj ((dist (cnf x1) (cnf x2)):xs))

dist :: Form -> Form -> Form
dist (Cnj f1s) f2 = (Cnj [(dist f f2) | f <- f1s])
dist f1 (Cnj f2s) = (Cnj [(dist f1 f) | f <- f2s])
dist f1 f2 = Dsj [f1,f2]

to_cnf :: Form -> Form
to_cnf f = cnf (nnf (arrowfree f))
--3


--4
type Clause = [Int]
type Clauses = [Clause]

--cnf2cls :: Form -> Clauses
