import Week3
import GHC.Exts
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
--cnf :: Form -> Form
--cnf (Neg x) = Neg (cnf x)
--cnf (Prop x) = Prop x
--cnf (Cnj xs) = Cnj (map cnf xs)
--cnf (Dsj xs) = dist (map cnf xs) 

-- errors
--dist :: [Form] -> Form
--dist [x] = x
--dist ((Cnj xs):ys) = Cnj (map (\x -> dist [x, dist ys]) xs)
--dist ys = 
--  where first_cnj = first_cnj
--dist xs = error (show xs)
--dist [x, (Cnj ys)] = dist [Cnj ys, x]


--dist ((Cnj x):xs) = Cnj (map (\x -> dist [x, dist ys]) xs)
--dist ((Dsj x):xs) = Cnj (map (\x -> dist [x, dist ys]) xs)


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


to_cnf :: Form -> Form
to_cnf xs = cnf' $ nnf $ arrowfree xs


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





poep :: Int -> [[Int]] -> [[Int]]
poep x ys = filter (\y -> not (any (x ==) y)) ys2
  where ys2 = map (plas x) ys

plas :: Int -> [Int] -> [Int]
plas x ys = filter (-x /=) ys

bla [x] = True
bla (x:y:ys)
  | x == -y = False
  | otherwise = bla (y:ys)

dpll :: [[Int]] -> Bool
dpll yy
  | all (\x -> length x == 1) yy = bla (sortWith abs (map (\x -> head x) yy))
  | any (\x -> length x == 0) yy = False
  | otherwise = dpll((poep y yy)) || dpll((poep (-y) yy))
  	where y = head $ head $ yy
    









