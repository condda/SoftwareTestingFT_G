module Ass3 where

import Week3

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
