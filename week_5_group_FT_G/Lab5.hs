module Lab5

where
import Data.List
import Week5
import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Monadic (assert, monadicIO, pick, pre, run)

-- Assignment 1.
--
-- There are several ways to use QuickCheck for testing a Sudoku generator and solver.
--
-- However, unless we want to change the code of the Sudoku assignment,
-- we need to use the QuickCheck Monadic library for testing the solver and the generator,
-- because the generator uses the IO monad, and to test the solver with arbitrary
-- Sudokus, Sudokus need to be generated.

-- The following functions represent the QuickCheck-checkable properties:

-- Test if every generated Sudoku is consistent.
prop_consistent :: Property
prop_consistent = monadicIO $ do [r] <- run $ rsolveNs [emptyN]
                                 assert $ consistent (fst r)

-- Test if every generated Sudoku is solved.
prop_solved :: Property
prop_solved = monadicIO $ do [r] <- run $ rsolveNs [emptyN]
                             assert $ solved r

problemSolEqOrig :: Node -> Node -> Bool
problemSolEqOrig p r = (sud2grid (fst sol)) == (sud2grid (fst r)) && (snd sol) == (snd r)
  where sol = (solveNs [p]) !! 0

-- Test if, for a generated problem, the solution is always the same.
-- Also test if the 
prop_same :: Property
prop_same = monadicIO $ do [r] <- run $ rsolveNs [emptyN]
                           p <- run $ genProblem r
                           assert $ problemSolEqOrig p r

-- Test if, for a generated problem, the Sudoku is minimal.
-- Note, this property uses the solution of assignment 2.
prop_min = monadicIO $  do [r] <- run $ rsolveNs [emptyN]
                           p <- run $ genProblem r
                           assert $ isMinimal p

-- Specifications for Random Sudoku Generator.
-- Note: may take quite some time.
testSudokuSolverAndGenerator :: IO ()
testSudokuSolverAndGenerator = hspec $ do
  describe "genRandomSudoku" $ do
    it "returns a consistent Sudoku" $ prop_consistent
    it "returns a solved Sudoku" $ prop_solved
  describe "genProblem" $ do
    it "returns a Sudoku for which the solution is always equal to the original" $ prop_same
    it "returns a minimal Sudoku" $ prop_min
  describe "solveNs" $ do
    it "should be able to solve examples 1..3" $ do
      all (\t -> t) [(solved $ head $ solveNs $ initNode s) | s <- [example1,example2,example3]] `shouldBe` True
    it "should return an empty list of nodes for insolvable sudokus." $ do
      length (solveNs $ initNode example4) `shouldBe` 0

solvedAndConsistent x = solved x && consistent (fst x)

-- Assignment 2.
isNotUniqueWithout n x = (not $ uniqueSol $ eraseN n x )

isMinimal :: Node -> Bool
isMinimal n = uniqueSol n && all (isNotUniqueWithout n) (filledPositions $ fst n)

-- The test was already defined in the first assignment, and can be checked using:
-- quickCheck prop_min

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

prop_sudoku_3empty_consistent_unique :: Property
prop_sudoku_3empty_consistent_unique = monadicIO $ do [r] <- run $ rsolveNs [emptyNwith3EmptyBlocks]
                                                      assert $ uniqueSol r

prop_sudoku_4empty_consistent_unique :: Property
prop_sudoku_4empty_consistent_unique = monadicIO $ do [r] <- run $ rsolveNs [emptyNwith4EmptyBlocks]
                                                      assert $ uniqueSol r

prop_sudoku_5empty_consistent_unique :: Property
prop_sudoku_5empty_consistent_unique = monadicIO $ do [r] <- run $ rsolveNs [emptyNwith5EmptyBlocks]
                                                      assert $ uniqueSol r

test3_4_5_emptyBlocksSudokus = hspec $ do
  describe "emptyNwith3EmptyBlocks" $ do
    it "should return a unique solution while being consistent when solved." $ prop_sudoku_3empty_consistent_unique
  describe "emptyNwith4EmptyBlocks" $ do
    it "should return a unique solution while being consistent when solved." $ prop_sudoku_3empty_consistent_unique
  describe "emptyNwith5EmptyBlocks" $ do
    it "should return a unique solution while being consistent when solved." $ prop_sudoku_3empty_consistent_unique


-- Assignment 4 and 5.
-- Assignment 4 is located in Lab5Nrc.hs, using Week5Nrc.hs.
-- The formal constraint:
--
-- for each location (r,c) in Sudoku S:
--   r should be injective
--   c should be injective
--   subgrid (r,c) should be injective
--   nrcSubgrid (r,c) should be injective,
--
-- where subgrid consists of the grids [[1..3], [4..6], [7..9]]^2
-- and nrcSubgrid consists of the grids [[2..4], [6..8]]^2
--
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

-- THE BONUS ASSIGNMENTS, BOTH THE SOLVER AND THE GENERATOR FOR THE SUDOKU:
--
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
-- CAN BE FOUND IN Week5x5.hs!!!!
