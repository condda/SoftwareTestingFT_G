
-- 1.1
my_length :: [a] -> Int
my_length = foldr (\_ x -> succ x) 0

-- 1.2
my_elem x = foldr (\n m -> (x==n) || m) False

-- 1.3
my_or = foldr (||) False


-- 1.4
my_map f = foldr (\x y -> (f x) : y) [] 

-- 1.5
my_filter_helper p x y
  | p x = x : y
  | otherwise = y
my_filter p = foldr (my_filter_helper p) []


-- 1.6
my_addadd x y = foldr (:) y x 

-- 1.7  I dont know reversal, but i know reverse
my_reverse = foldr (\x y -> y ++ [x]) []

-- 2
my_reverse_2 = foldl (flip (:)) []

-- 3


-- 4
data Creature = Lady | Tiger
  deriving (Eq,Show)
type Sign = (Creature, Creature) -> Bool
sign1, sign2 :: Sign
sign1 (x,y) = x == Lady || y == Lady
sign2 (x,y) = x == Tiger

solution2 :: [(Creature,Creature)]
solution2 =
  [ (x,y) | x <- [Lady,Tiger],
            y <- [Lady,Tiger],
            sign1 (x,y) == sign2 (x,y) ]

-- solution: [(Tiger,Lady)]

-- 5

data Islander = Knight | Knave
  deriving (Eq,Show)
john, bill :: (Islander,Islander) -> Bool
john (x,y) = x == y
bill (x,y) = x /= y
solution3 :: [(Islander,Islander)]
solution3 = [(x,y) | x <- [Knight,Knave],
                     y <- [Knight,Knave],
                     john (x,y) == (x == Knight),
                     bill (x,y) == (y == Knight) ]

-- 6
data Boy = Matthew | Peter | Jack | Arnold | Carl
  deriving (Eq,Show)
boys = [Matthew, Peter, Jack, Arnold, Carl]
matthew, peter, jack, arnold, carl :: Boy -> Bool
matthew = \ x -> not (x==Matthew) && not (x==Carl)
peter = \ x -> x==Matthew || x==Jack
jack = \ x -> not (matthew x) && not (peter x)
arnold = \ x -> matthew x /= peter x
carl = \ x -> not (arnold x)

declarations = [matthew,peter,jack,arnold,carl]
table = zip declarations boys


-- my own functions
solution = [x | x <- [Matthew, Peter, Jack, Arnold, Carl],
              length (filter (\y -> y x) declarations) == 3]

honest x = map snd (filter (\(f, _) -> f x) table)
