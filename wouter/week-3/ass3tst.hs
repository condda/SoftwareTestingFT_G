module Ass3Tst where

import Week3

-- 2
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

remEmptyDsjAndCnj' :: Form -> Form
remEmptyDsjAndCnj' (Dsj []) = Cnj [(Prop 1), (Neg (Prop 1))]
remEmptyDsjAndCnj' (Cnj []) = Dsj [(Prop 1), (Neg (Prop 1))]
remEmptyDsjAndCnj' (Cnj xs) = Cnj [(remEmptyDsjAndCnj' x) | x <- xs]
remEmptyDsjAndCnj' (Dsj xs) = Dsj [(remEmptyDsjAndCnj' x) | x <- xs]
remEmptyDsjAndCnj' xs = xs




cnf :: Form -> Form
cnf (Neg x) = Neg (cnf x)
cnf (Prop x) = Prop x
cnf (Cnj []) = cnf $ Dsj [Neg (Prop 1), Prop 1]
cnf (Dsj []) = cnf $ Cnj [Neg (Prop 1), Prop 1]
cnf (Cnj xs) = Cnj (map cnf xs)
cnf (Dsj xs) = dist (map cnf xs)

dist :: [Form] -> Form
dist ((Cnj []):xs) = dist $ (Dsj [Neg (Prop 1), Prop 1]):xs
dist ((Cnj ys):xs) = Cnj (map (\z -> dist (z:xs)) ys)
dist [x,(Cnj ys)] = Cnj (map (\z -> dist [x,z]) ys)
dist ((Dsj []):xs) = dist $ (Cnj [Neg (Prop 1), Prop 1]):xs
dist xs = Dsj xs

toCnf :: Form -> Form
toCnf xs = cnf $ remEmptyDsjAndCnj' $ nnf $ arrowfree xs



grL (Prop p) = True
grL (Neg (Prop p)) = True
grL _ = False

grD (Dsj (as)) = all (\x -> x) [ grD x | x <- as ]
grD x = grL x

grC (Cnj (as)) = all (\x -> x) [ grC x | x <- as ]
grC x = grD x




-- Equiv --
equiv' :: [Valuation] -> Form -> Form -> Bool
equiv' [] f1 f2 = True
equiv' (v:vs) f1 f2 = (eval v f1) == (eval v f2) && (equiv' vs f1 f2)

-- Equiv: a <-> b
equiv :: Form -> Form -> Bool
equiv f1 f2 = (equiv' (allVals (Cnj [f1, f2])) f1 f2)

equivAndFstGoodGrammar x y
  | (grC x) && (equiv x y) = True
  | (grC x) = error ("Error in Equivalence! 'CNF':\n\n" ++ (show x) ++ "\n\nOriginal:\n\n" ++ (show y))
  | (equiv x y) = error ("Error in Grammar! 'CNF':\n\n" ++ (show x) ++ "\n\nOriginal:\n\n" ++ (show y))
  | otherwise = error ("Error in Grammar and Equivalence! 'CNF':\n\n" ++ (show x) ++ ", original:\n\n" ++ (show y))


testCnf 0 = do
  x <- getRandomFSmpl
  return $ equivAndFstGoodGrammar (toCnf x) x

testCnf n = do
  y <- testCnf 0
  z <- testCnf (n - 1)
  return $ y && z -- Yes, I am aware of the fact that y && z won't do anything for the performance here...

