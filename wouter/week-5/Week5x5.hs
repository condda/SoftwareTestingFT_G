-- During the lecture the teacher
-- stated that the bonus assignments were:
-- 1. Create a Sudoku generator for a special kind of Sudoku:
--    GGG GGG
--    GGG GGG
--    GGGGGGG
--      GGG
--    GGGGGGG
--    GGG GGG
--    GGG GGG
--
-- Where G is a subgrid.
--
-- 2. Create a Sudoku solver.
--
-- Both aspects work. Both can be demonstrated using the respective
-- functions:
-- - generateSudoku
-- - solveExSudoku
-- Due to the fact that the minimization process takes very long,
-- we created a simple example which is basically a generated Sudoku
-- but with a few fields removed, that is not minimal, but is consistent.
-- Although this may not be a very strong example, the fact alone that
-- the generation of Sudokus is done using rsolveNs [emptyN] should already
-- prove the fact that both the Generator and the Solver work, because
-- the Generator works by SOLVING an empty Sudoku.

module Week5x5 where 

import Data.List
import System.Random

type Row    = Int 
type Column = Int 
type Value  = Int
type Grid   = [[Value]]

positions, values :: [Int]
positions = [1..21]
values    = [1..9] 

blocks :: [[Int]]
blocks = [[1..3],[4..6],[7..9],[10..12],[13..15],[16..18],[19..21]]

showVal :: Value -> String
showVal 0 = " "
showVal d = show d

showRow :: [Value] -> IO()
showRow [a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,a20,a21] = 
 do  putChar '|'         ; putChar ' '
     putStr (showVal a1) ; putChar ' '
     putStr (showVal a2) ; putChar ' '
     putStr (showVal a3) ; putChar ' '
     putChar '|'         ; putChar ' '
     putStr (showVal a4) ; putChar ' '
     putStr (showVal a5) ; putChar ' '
     putStr (showVal a6) ; putChar ' '
     putChar '|'         ; putChar ' '
     putStr (showVal a7) ; putChar ' '
     putStr (showVal a8) ; putChar ' '
     putStr (showVal a9) ; putChar ' '
     putChar '|'         ; putChar ' '
     putStr (showVal a10) ; putChar ' '
     putStr (showVal a11) ; putChar ' '
     putStr (showVal a12) ; putChar ' '
     putChar '|'         ; putChar ' '
     putStr (showVal a13) ; putChar ' '
     putStr (showVal a14) ; putChar ' '
     putStr (showVal a15) ; putChar ' '
     putChar '|'         ; putChar ' '
     putStr (showVal a16) ; putChar ' '
     putStr (showVal a17) ; putChar ' '
     putStr (showVal a18) ; putChar ' '
     putChar '|'         ; putChar ' '
     putStr (showVal a19) ; putChar ' '
     putStr (showVal a20) ; putChar ' '
     putStr (showVal a21) ; putChar ' '
     putChar '|'         ; putChar '\n'

showRow2 :: [Value] -> IO()
showRow2 [a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,a20,a21] = 
 do  putChar ' '         ; putChar ' '
     putStr (showVal a1) ; putChar ' '
     putStr (showVal a2) ; putChar ' '
     putStr (showVal a3) ; putChar ' '
     putChar ' '         ; putChar ' '
     putStr (showVal a4) ; putChar ' '
     putStr (showVal a5) ; putChar ' '
     putStr (showVal a6) ; putChar ' '
     putChar '|'         ; putChar ' '
     putStr (showVal a7) ; putChar ' '
     putStr (showVal a8) ; putChar ' '
     putStr (showVal a9) ; putChar ' '
     putChar '|'         ; putChar ' '
     putStr (showVal a10) ; putChar ' '
     putStr (showVal a11) ; putChar ' '
     putStr (showVal a12) ; putChar ' '
     putChar '|'         ; putChar ' '
     putStr (showVal a13) ; putChar ' '
     putStr (showVal a14) ; putChar ' '
     putStr (showVal a15) ; putChar ' '
     putChar '|'         ; putChar ' '
     putStr (showVal a16) ; putChar ' '
     putStr (showVal a17) ; putChar ' '
     putStr (showVal a18) ; putChar ' '
     putChar ' '         ; putChar ' '
     putStr (showVal a19) ; putChar ' '
     putStr (showVal a20) ; putChar ' '
     putStr (showVal a21) ; putChar ' '
     putChar ' '         ; putChar '\n'

showGrid :: Grid -> IO()
showGrid [a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,a20,a21] =
 do putStrLn ("+-------+-------+-------+       +-------+-------+-------+")
    showRow a1; showRow a2; showRow a3;
    putStrLn ("+-------+-------+-------+       +-------+-------+-------+")
    showRow a4; showRow a5; showRow a6;
    putStrLn ("+-------+-------+-------+-------+-------+-------+-------+")
    showRow a7; showRow a8; showRow a9;
    putStrLn ("+-------+-------+-------+-------+-------+-------+-------+")
    showRow2 a10; showRow2 a11; showRow2 a12;
    putStrLn ("+-------+-------+-------+-------+-------+-------+-------+")
    showRow a13; showRow a14; showRow a15;
    putStrLn ("+-------+-------+-------+-------+-------+-------+-------+")
    showRow a16; showRow a17; showRow a18;
    putStrLn ("+-------+-------+-------+       +-------+-------+-------+")
    showRow a19; showRow a20; showRow a21;
    putStrLn ("+-------+-------+-------+       +-------+-------+-------+")
showGrid n = do putStrLn (show (length n))

type Sudoku = (Row,Column) -> Value

sud2grid :: Sudoku -> Grid
sud2grid s = 
  [ [ s (r,c) | c <- [1..21] ] | r <- [1..21] ] 

grid2sud :: Grid -> Sudoku
grid2sud gr = \ (r,c) -> pos gr (r,c) 
  where 
  pos :: [[a]] -> (Row,Column) -> a 
  pos gr (r,c) = (gr !! (r-1)) !! (c-1)

showSudoku :: Sudoku -> IO()
showSudoku = showGrid . sud2grid

bl :: Int -> [Int]
bl x = concat $ filter (elem x) blocks 

subGrid :: Sudoku -> (Row,Column) -> [Value]
subGrid s (r,c) = 
  [ s (r',c') | r' <- bl r, c' <- bl c ]

freeInSeq :: [Value] -> [Value]
freeInSeq seq = values \\ seq 

freeInRow :: Sudoku -> (Row,Column) -> [Value]
freeInRow s (r,c)
  -- Overlaps
  -- Top:
  | c>= 7 && c<= 9   && ((r>= 7 && r<= 9) || (r>=13 && r<=15)) = freeInSeq [ s(r,i) | i <- [1..15] ]
  -- Bottom:
  | c>= 13 && c<= 15 && ((r>= 7 && r<= 9) || (r>=13 && r<=15)) = freeInSeq [ s(r,i) | i <- [7..21] ]
  -- Partial Sudokus
  -- Middle
  | c>=7 && c<=15 && r>=7 && r<=15 = freeInSeq [ s(r,i) | i <- [7..15] ]
  -- Top
  | c<=9 = freeInSeq [ s(r,i) | i <- [1..9] ]
  -- Bottom
  | c>=13 = freeInSeq [ s(r,i) | i <- [13..21] ]
  | otherwise = error $ "Error: col " ++ (show c) ++ ", row" ++ (show r)

freeInColumn :: Sudoku -> (Row,Column) -> [Value]
freeInColumn s (r,c) = freeInRow (\ (r,c) -> s (c,r)) (c,r)

freeInSubgrid :: Sudoku -> (Row,Column) -> [Value]
freeInSubgrid s (r,c) = freeInSeq (subGrid s (r,c))

freeAtPos :: Sudoku -> (Row,Column) -> [Value]
freeAtPos s (r,c) = 
  (freeInRow s (r,c)) 
   `intersect` (freeInColumn s (r,c)) 
   `intersect` (freeInSubgrid s (r,c)) 

injective :: Eq a => [a] -> Bool
injective xs = nub xs == xs

rowInjective :: Sudoku -> [Int] -> Row -> Bool
rowInjective s poss r = injective vs where 
   vs = filter (/= 0) [ s (r,i) | i <- poss ]

colInjective :: Sudoku -> [Int] -> Column -> Bool
colInjective s poss c = injective vs where 
   vs = filter (/= 0) [ s (i,c) | i <- poss ]

subgridInjective :: Sudoku -> (Row,Column) -> Bool
subgridInjective s (r,c) = injective vs where 
   vs = filter (/= 0) (subGrid s (r,c))

rowsAndColsInjective s possR possC =
  [ rowInjective s possC r  | r <- possR] ++
  [ colInjective s possR c  | c <- possC]

consistent :: Sudoku -> Bool
consistent s = and $
               rowsAndColsInjective s [1..9] [1..9]
                ++
               rowsAndColsInjective s [1..9] [13..21]
                ++
               rowsAndColsInjective s [13..21] [1..9]
                ++
               rowsAndColsInjective s [13..21] [13..21]
                ++
               rowsAndColsInjective s [7..15] [7..15]
                ++
               [ subgridInjective s (r,c) |
                    r <- [1,4,7,10,13,16,19], c <- [1,4,7,10,13,16,19],
                    r /= 10 || (c == 1 || c == 4 || c == 16 || c == 19),
                    c /= 10 || (r == 1 || r == 4 || r == 16 || r == 19)]

extend :: Sudoku -> ((Row,Column),Value) -> Sudoku
extend = update

update :: Eq a => (a -> b) -> (a,b) -> a -> b 
update f (y,z) x = if x == y then z else f x 

type Constraint = (Row,Column,[Value])

type Node = (Sudoku,[Constraint])

showNode :: Node -> IO()
showNode = showSudoku . fst

solved  :: Node -> Bool
solved = null . snd

extendNode :: Node -> Constraint -> [Node]
extendNode (s,constraints) (r,c,vs) = 
   [(extend s ((r,c),v),
     sortBy length3rd $ 
         prune (r,c,v) constraints) | v <- vs ]

length3rd :: (a,b,[c]) -> (a,b,[c]) -> Ordering
length3rd (_,_,zs) (_,_,zs') = 
  compare (length zs) (length zs')

sameSudoku (r,c) (x,y)
  | r <= 9 && x <= 9 && c <= 9 && y <= 9 = True
  | r >= 13 && x >= 13 && c <= 9 && y <= 9 = True
  | r <= 9 && x <= 9 && c >= 13 && y >= 13 = True
  | r >= 13 && x >= 13 && c >= 13 && y >= 13 = True
  | r >= 7 && x >= 7 && r <= 15 && x <= 15 && c >= 7 && y >= 7 && c <= 15 && y <= 15 = True
  | otherwise = False

prune :: (Row,Column,Value) 
      -> [Constraint] -> [Constraint]
prune _ [] = []
prune (r,c,v) ((x,y,zs):rest)
  | sameSudoku (r,c) (x,y) && r == x = (x,y,zs\\[v]) : prune (r,c,v) rest
  | sameSudoku (r,c) (x,y) && c == y = (x,y,zs\\[v]) : prune (r,c,v) rest
  | sameblock (r,c) (x,y) = 
        (x,y,zs\\[v]) : prune (r,c,v) rest
  | otherwise = (x,y,zs) : prune (r,c,v) rest

sameblock :: (Row,Column) -> (Row,Column) -> Bool
sameblock (r,c) (x,y) = bl r == bl x && bl c == bl y 

initNode :: Grid -> [Node]
initNode gr = let s = grid2sud gr in 
              if (not . consistent) s then [] 
              else [(s, constraints s)]

openPositions :: Sudoku -> [(Row,Column)]
openPositions s = [ (r,c) | r <- positions,  
                            c <- positions,
                            ((r <= 9 || r > 12) && (c <= 9 || c > 12)) -- outside sudokus.
                            || (r > 6 && r <= 15 && c > 6 && c <= 15), -- middle sudoku.
                            s (r,c) == 0 ]

constraints :: Sudoku -> [Constraint] 
constraints s = sortBy length3rd 
    [(r,c, freeAtPos s (r,c)) | 
                       (r,c) <- openPositions s ]

data Tree a = T a [Tree a] deriving (Eq,Ord,Show)

exmple1 = T 1 [T 2 [], T 3 []]
exmple2 = T 0 [exmple1,exmple1,exmple1]

grow :: (node -> [node]) -> node -> Tree node 
grow step seed = T seed (map (grow step) (step seed))

count :: Tree a -> Int 
count (T _ ts) = 1 + sum (map count ts)

search :: (node -> [node]) 
       -> (node -> Bool) -> [node] -> [node]
search children goal [] = []
search children goal (x:xs) 
  | goal x    = x : search children goal xs
  | otherwise = search children goal 
                                ((children x) ++ xs)

solveNs :: [Node] -> [Node]
solveNs = search succNode solved 

succNode :: Node -> [Node]
succNode (s,[]) = []
succNode (s,p:ps) = extendNode (s,ps) p 

solveAndShow :: Grid -> IO()
solveAndShow gr = solveShowNs (initNode gr)

solveShowNs :: [Node] -> IO()
solveShowNs = sequence_ . fmap showNode . solveNs

emptyN :: Node
emptyN = (\ _ -> 0,constraints (\ _ -> 0))

getRandomInt :: Int -> IO Int
getRandomInt n = getStdRandom (randomR (0,n))

getRandomItem :: [a] -> IO [a]
getRandomItem [] = return []
getRandomItem xs = 
  do n <- getRandomInt maxi
     return [xs !! n]
     where maxi = length xs - 1

randomize :: Eq a => [a] -> IO [a]
randomize xs = do y <- getRandomItem xs 
                  if null y 
                    then return []
                    else do ys <- randomize (xs\\y)
                            return (head y:ys)

sameLen :: Constraint -> Constraint -> Bool
sameLen (_,_,xs) (_,_,ys) = length xs == length ys

getRandomCnstr :: [Constraint] -> IO [Constraint]
getRandomCnstr cs = getRandomItem (f cs) 
  where f [] = []
        f (x:xs) = takeWhile (sameLen x) (x:xs)

rsuccNode :: Node -> IO [Node]
rsuccNode (s,cs) = 
  do xs <- getRandomCnstr cs
     if null xs 
        then return []
        else return (extendNode (s,cs\\xs) (head xs))

rsolveNs :: [Node] -> IO [Node]
rsolveNs ns = rsearch rsuccNode solved (return ns)

rsearch :: (node -> IO [node]) 
        -> (node -> Bool) -> IO [node] -> IO [node]
rsearch succ goal ionodes = 
  do xs <- ionodes 
     if null xs then return []
     else 
       if goal (head xs) then return [head xs]
       else 
        do ys <- rsearch succ goal (succ (head xs))
           if (not . null) ys then return [head ys]
           else 
             if null (tail xs) then return []
             else rsearch succ goal (return $ tail xs)

genRandomSudoku :: IO Node
genRandomSudoku = do [r] <- rsolveNs [emptyN]
                     return r

randomS = genRandomSudoku >>= showNode

uniqueSol :: Node -> Bool
uniqueSol node = singleton (solveNs [node]) where 
  singleton [] = False
  singleton [x] = True
  singleton (x:y:zs) = False

eraseS :: Sudoku -> (Row,Column) -> Sudoku
eraseS s (r,c) = update s ((r,c),0)

eraseN :: Node -> (Row,Column) -> Node
eraseN n (r,c) = (s, constraints s) 
  where s = eraseS (fst n) (r,c) 

minimalize :: Node -> [(Row,Column)] -> Node
minimalize n [] = n
minimalize n ((r,c):rcs) 
   | uniqueSol n' = minimalize n' rcs
   | otherwise    = minimalize n  rcs
  where n' = eraseN n (r,c)

filledPositions :: Sudoku -> [(Row,Column)]
filledPositions s = 
  [ (r,c) | r <- positions,  
            c <- positions, s (r,c) /= 0 ]

genProblem :: Node -> IO Node
genProblem n = do ys <- randomize xs
                  return (minimalize n ys)
   where xs = filledPositions (fst n)

generateSudoku :: IO ()
generateSudoku = do [r] <- rsolveNs [emptyN]
                    showNode r

-- Example for solving.
solveExample :: [[Int]]
solveExample = [[0,0,0,9,3,2,6,1,7,0,0,0,7,6,8,1,2,9,4,3,5],[7,6,3,5,1,8,9,4,2,0,0,0,2,5,9,3,4,6,7,1,8],[2,1,9,6,7,4,3,8,5,0,0,0,4,3,1,5,8,7,6,2,9],[3,9,1,8,6,5,2,7,4,0,0,0,8,2,7,4,9,1,5,6,3],[4,7,5,2,9,1,8,6,3,0,0,0,1,9,3,8,6,5,2,7,4],[8,2,6,7,4,3,1,5,9,0,0,0,5,4,6,2,0,0,0,9,1],[6,4,2,1,5,9,0,0,0,9,5,4,6,1,2,9,5,4,3,8,7],[1,5,8,3,2,7,4,9,6,2,1,7,3,8,5,7,1,2,9,4,6],[9,3,7,4,8,6,5,2,1,3,8,6,9,7,4,6,3,8,1,5,2],[0,0,0,0,0,0,8,5,9,7,4,1,2,6,3,0,0,0,0,0,0],[0,0,0,0,0,0,6,4,2,8,3,9,7,5,1,0,0,0,0,0,0],[0,0,0,0,0,0,1,7,3,5,6,2,4,9,8,0,0,0,0,0,0],[3,6,4,2,7,8,9,1,5,4,7,3,8,2,6,4,3,5,1,9,7],[9,7,8,3,5,1,2,6,4,1,9,8,5,3,7,2,9,1,4,6,8],[5,2,1,9,6,4,3,8,7,6,2,5,1,4,9,7,8,6,3,5,2],[4,5,7,1,9,2,8,3,6,0,0,0,6,5,1,8,4,7,9,2,3],[1,3,2,5,8,6,7,4,9,0,0,0,7,9,3,1,5,2,8,4,6],[8,9,6,7,4,3,1,5,2,0,0,0,4,8,2,9,6,3,7,1,5],[2,4,9,8,3,5,6,7,1,0,0,0,9,6,8,3,2,4,5,7,1],[7,8,5,6,1,9,4,2,3,0,0,0,2,7,4,5,1,8,6,3,9],[6,1,3,4,2,7,5,9,8,0,0,0,3,1,5,6,7,9,2,8,4]]


solveExSudoku :: IO ()
solveExSudoku = do [r] <- rsolveNs (initNode solveExample)
                   showSudoku $ grid2sud solveExample
                   showNode r

