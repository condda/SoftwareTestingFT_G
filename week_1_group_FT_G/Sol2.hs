
-- copy from the book
logEquiv2 :: (Bool -> Bool -> Bool) -> (Bool -> Bool -> Bool) -> Bool

logEquiv2 bf1 bf2 =
  and [(bf1 p q) == (bf2 p q) | p <- [True,False],
                               	q <- [True,False]]

(==>) :: Bool -> Bool -> Bool
True ==> x = x
False ==> x = True
-- end copy

-- 2.13
test1 = logEquiv2 (\ p q -> not p == q) (\p q -> not q == p)
-- theorem 2.12.1 is True
test2 = logEquiv2 (\ p q -> p ==> q) (\p q -> not p)
-- theorem 2.12.2 is False





-- We created a countIf to count the number of passing elements in a list
-- with a little help from our friends of stackoverflow, check:
-- http://stackoverflow.com/questions/25647823/cant-get-point-free-notation-to-compile-in-haskell/25648161#25648161

(.:) = (.).(.)
infixr 9 .:

countIf :: (a -> Bool) -> [a] -> Int
countIf = length .: filter

-- end

-- 2.51
unique :: (a -> Bool) -> [a] -> Bool
unique = (1==) .: countIf
--unique = (1==) . length .: filter

-- 2.52
parity :: [Bool] -> Bool
parity = even . countIf id 
--parity [] = True
--parity (x:xs)
--  | x = not (parity xs)
--  | otherwise = parity xs

-- 2.53
evenNR :: (a -> Bool) -> [a] -> Bool
evenNR = even .: countIf
--evenNR = ((even) . length) .: filter
