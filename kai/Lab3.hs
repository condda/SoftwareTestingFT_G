import Week3

-- 1
contradiction :: Form -> Bool
contradiction xs = not $ satisfiable xs
-- not satisfiable <=> contradiction

tautology :: Form -> Bool
tautology f = contradiction (Neg f) 
-- satisfiable Neg f <=> not tautology

entails :: Form -> Form -> Bool
entails f g = tautology (Dsj [(Neg f), g])
-- if for every value the imply (=>) holds its true

equiv :: Form -> Form -> Bool
equiv f g = tautology (Cnj [Dsj [Neg f, g], Dsj [f, Neg g]])
-- if always the equivalence holds (<=>)

test_contradiction_1 = contradiction (Cnj [Neg (Prop 1), Prop 1])
test_contradiction_2 = not $ contradiction (Cnj [Prop 1, Prop 1])

test_tautology_1 = tautology (Dsj [Neg (Prop 1), Prop 1])
test_tautology_2 = not $ tautology (Prop 1)

test_equiv_equals_entails f g = (equiv f g) == ((entails f g) && (entails g f))

-- 2
cnf :: Form -> Form
cnf (Neg x) = Neg (cnf x)
cnf (Prop x) = Prop x
cnf (Cnj xs) = Cnj (map cnf xs)
cnf (Dsj xs) = dist (map cnf xs) 

dist :: [Form] -> Form
dist ((Cnj ys):xs) = Cnj (map (\z -> dist (z:xs)) ys)
dist [x,(Cnj ys)] = Cnj (map (\z -> dist [x,z]) ys)
dist xs = Dsj xs

to_cnf :: Form -> Form
to_cnf xs = cnf $ nnf $ arrowfree xs




-- 3
-- test it by putting in a random form, check if input and output are equiv
-- and if cnf is in the correct form
is_literal :: Form -> Bool
is_literal (Prop f) = True
is_literal (Neg f) = is_literal f
is_literal _ = False

is_disjunctive :: Form -> Bool
is_disjunctive (Dsj xs) = all is_disjunctive xs
is_disjunctive f = is_literal f

is_conjunctive :: Form -> Bool
is_conjunctive (Cnj xs) = all is_conjunctive xs
is_conjunctive f = is_disjunctive f

test_to_cnf_is_cnf f = is_conjunctive (to_cnf f)
test_to_cnf_is_equivalent f = equiv f (to_cnf f)
 
-- 4
type Clause = [Int]
type Clauses = [Clause]

literal2cls :: Form -> Int
literal2cls (Prop f) = f
literal2cls (Neg f) = literal2cls f
literal2cls _ = error "literal2cls"
 
disjunctive2cls :: Form -> Clause
disjunctive2cls (Dsj xs) = foldr (\y x -> x ++ (disjunctive2cls y)) [] xs
disjunctive2cls f = [literal2cls f]
 
conjunctive2cls :: Form -> Clauses
conjunctive2cls (Cnj xs) = foldr (\y x -> x ++ (conjunctive2cls y)) [] xs
conjunctive2cls f = [disjunctive2cls f]

  
