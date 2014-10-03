module Lab5Nrc

where
import Data.List
import Week5Nrc
import Test.Hspec
import Test.QuickCheck

nrcExample :: Grid
nrcExample = [[0,0,0,3,0,0,0,0,0],
              [0,0,0,7,0,0,3,0,0],
              [2,0,0,0,0,0,0,0,8],
              [0,0,6,0,0,5,0,0,0],
              [0,9,1,6,0,0,0,0,0],
              [3,0,0,0,7,1,2,0,0],
              [0,0,0,0,0,0,0,3,1],
              [0,8,0,0,4,0,0,0,0],
              [0,0,2,0,0,0,0,0,0]]

-- Assignment 4 and 5.
--
-- (See Week5Nrc.hs)
-- The new formal constraint is that all subgrids [2..4]x[2..4] and [6..8]x[6..8] have to be injective.
-- This constraint is formulated by subgridNrcInjective.
--
-- The new constraint was for consistency (which was also added to consistent):
--
--   subgridNrcInjective :: Sudoku -> (Row,Column) -> Bool
--   subgridNrcInjective s (r,c) = injective vs where 
--     vs = filter (/= 0) (subGridNrc s (r,c))
--
-- For solving, finding the free positions (which was used in freeAtPos) was done using:
--
--   freeInSubgridNrc :: Sudoku -> (Row,Column) -> [Value]
--   freeInSubgridNrc s (r,c) = freeInSeq (subGridNrc s (r,c))
--
-- Where subGridNrc:
--
--   subGridNrc :: Sudoku -> (Row,Column) -> [Value]
--   subGridNrc s (r,c) = 
--     [ s (r',c') | r' <- blNrc r, c' <- blNrc c ]
--
-- And blNrc:
--
--   blNrc :: Int -> [Int]
--   blNrc x = concat $ filter (elem x) blocksNrc
--
-- (Which was also used for sameBlock-function, which is used for pruning as well).
--
-- The new constraint uses blocksNrc for locating the blocks with the new constraint:
--
--   blocksNrc = [[2..4],[6..8]]
--
-- The new program can be used in the same way as the old program: by calling main.
testNrc = solveAndShow nrcExample
