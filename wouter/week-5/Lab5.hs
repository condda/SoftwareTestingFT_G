module Lab5

where
import Data.List
import Week5
import Test.Hspec
import Test.QuickCheck

-- Wouter's version, based on Kai's work :-)
-- Assignment 1.

testRandomSudoku = do
  node <- genRandomSudoku
  return $ consistent $ fst node

-- Specifications for Random Sudoku Generator
testGenRandomSudoku :: IO ()
testGenRandomSudoku = hspec $ do
  describe "genRandomSudoku" $ do
    it "is consistent" $ do
      node <- genRandomSudoku
      (consistent $ fst node) `shouldBe` True
    it "is solved" $ do
      node <- genRandomSudoku
      (solved node) `shouldBe` True

-- Specifications for Problem Generator
testGenProblem :: IO ()
testGenProblem = hspec $ do
  describe "genProblem" $ do
    it "is consistent" $ do
      node <- genRandomSudoku
      s <- genProblem node
      (consistent $ fst s) `shouldBe` True
    it "is minimal" $ do
      node <- genRandomSudoku
      s <- genProblem node
      (isMinimal $ s) `shouldBe` True

solvedAndConsistent x = solved x && consistent (fst x)

-- Specifications for Sudoku Solver
testSolveNs :: IO()
testSolveNs = hspec $ do
  describe "solveNs" $ do
    it "should be able to examples 1..3" $ do
      all (\t -> t) [(solved $ head $ solveNs $ initNode s) | s <- [example1,example2,example3]] `shouldBe` True

    it "should be able to solve a randomly generated problem." $ do
      node <- genRandomSudoku
      s <- genProblem node
      (solvedAndConsistent $ head $ solveNs $ [node]) `shouldBe` True

    it "should return an empty list of nodes for insolvable sudokus." $ do
      length (solveNs $ initNode example4) `shouldBe` 0

-- It is possible to write tests for QuickCheck, by implementing
-- an instance of Arbitrary which generates random Sudoku puzzles, and
-- apply property tests to them.



-- Assignment 2.
isUniqueWith n x = (not $ uniqueSol $ eraseN n x )

-- The function testIsMinimal tests if a node is minimal for n random cases.
testIsMinimal 1 = do
	node <- (rsolveNs [emptyN])
	node2 <- genProblem (head $ node)
	return $ isMinimal node2

testIsMinimal n = do
        x <- testIsMinimal (n - 1)
        y <- testIsMinimal 1
        return $ x && y

isMinimal :: Node -> Bool
isMinimal n = uniqueSol n && all (isUniqueWith n) (filledPositions $ fst n)

-- Assignment 3.
block :: (Int, Int) -> [Constraint]
block (r,c) = [(x, y, positions) | x <- bl (3*r), y <- bl (3*c)]

emptyNwith3EmptyBlocks :: Node
emptyNwith3EmptyBlocks = (\ _ -> 0, (block (2,1)) ++ (block (3,1)) ++ (block (1,2)) ++ (block (3,2)) ++ (block (1,3)) ++ (block (2,3)))

generateSudoku3EmptyBlocks = do [r] <- rsolveNs [emptyNwith3EmptyBlocks]
                                showNode r

emptyNwith4EmptyBlocks :: Node
emptyNwith4EmptyBlocks = (\ _ -> 0, (block (2,1)) ++ (block (3,1)) ++ (block (1,2)) ++ (block (3,2)))

generateSudoku4EmptyBlocks = do [r] <- rsolveNs [emptyNwith4EmptyBlocks]
                                showNode r

emptyNwith5EmptyBlocks :: Node
emptyNwith5EmptyBlocks = (\ _ -> 0, (block (2,1)) ++ (block (3,1)) ++ (block (1,2)))

generateSudoku5EmptyBlocks = do [r] <- rsolveNs [emptyNwith5EmptyBlocks]
                                showNode r

-- Yes, it is possible to have sudokus with 3, 4 or 5 empty blocks.
-- In order to test this, we wrote a function that tests whether
-- the results of the version with 5 empty blocks (and thus the hardest) is solvable
-- using a unique solution:
check5Sudoku = do [r] <- rsolveNs [emptyNwith5EmptyBlocks]
                  return $ uniqueSol r

-- Assignment 4 and 5.
-- Assignment 4 is located in Lab5Nrc, using Week5Nrc.
-- Time spent: 1 hour on assignment 4, which by accident also completed assignment 5.

-- Assignment 6.
--
-- To develop a generator for easy/hard problems, first we need to take a look at
-- what classifies a sudoku easy/hard by humans.
--
-- The paper proposes states that the most important factors on the difficulty are:
--
-- - Complexity of individual (logic) steps;
-- - Structure of dependency among steps.
--
-- The paper presents a method that classifies Sudoku puzzles using a combination of
-- four different already existing classification methods.
--
-- One (easy) way to generate easy/hard puzzles is to implement this (very complex) classifier and generate
-- puzzles until the classifier classifies the found puzzle as either easy or hard.
--
-- A simpler, yet, probably not as reliable classifier, would be a classifier that classifies
-- a Sudoku puzzle as easy if it does not require backtracking to solve, and only
-- requires a set of proven trivial Sudoku techniques, and otherwise as hard.
--
-- Generating different puzzles until the classifier either marks it easy or hard, will then
-- result in either a hard or an easy puzzle.
