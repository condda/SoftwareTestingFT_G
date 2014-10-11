module Lab5

where
import Data.List
import Week5
import Test.QuickCheck
import Test.Hspec
import System.Random

--1
hspecSudoku :: IO()
hspecSudoku = hspec $ do
  describe "Sudoku" $ do
    it "Rows should be all different" $ do
	  node <- genRandomSudoku
	  (consistent $ fst node) `shouldBe` True

-- instance (Arbitrary a, Ord a) => Arbitrary (Sudoku) where
-- arbitrary = TODO:

-- - Change example for RGD scenario.
-- - Functional requirements: list them and explain how do we get the requirements.
 -- $ \n ->
  -- do k <- choose (0,n)
  -- list2set <$> sequence [ arbitrary | _ <- [1..k] ]
	  
--2

nodeIO2node = do
  [r] <- rsolveNs [emptyN]
  node <- genProblem r
  return (and $ (uniqueSol node) : [not (uniqueSol (eraseN (node) (x,y))) | (x,y) <- filledPositions (fst node)])
  
  -- 1: isunique = true
  -- 2: when erase -> all = false
  
---3
genSud :: [(Int,Int)] -> IO ()
genSud xs = do 
  n <- genRandomSudoku
  prob <- (genProblem (deleteValues (getxy xs) n))
  showNode prob

getxy :: [(Int,Int)] -> [(Row,Column)]
getxy [] = []
getxy ((r,c):rcs) = block (r,c) ++ getxy rcs

block :: (Int, Int) -> [(Row,Column)]
block (r,c) = [(x, y) | x <- bl (3*r), y <- bl (3*c)]

deleteValues :: [(Row,Column)] -> Node -> Node
deleteValues [] n = n
deleteValues ((r,c):rcs) n = eraseN (deleteValues (rcs) n) (r,c)


-- genSud [] n = True
-- genSud ((r,c)) _ = False
-- genSud ((r,c):rcs) n = eraseN n (r,c) 

-- [[(1,2)(1,3)()][()()()][()()()]]
-- 1 4 7
-- 4
-- 7
  -- node <- genRandomSudoku
  -- r <- getStdRandom (randomR (1,9))
  -- c <- getStdRandom (randomR (1,9))
  
  -- return (subGrid (fst node) (c, r), c,r)

 
 -- 1: Generar Sudoku
 -- 2: Borrar bloques.