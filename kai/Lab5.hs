module Lab5

where
import Data.List
import Week5
import Test.Hspec
import Test.QuickCheck



--test_for_random_sudoku = do
--  node <- genRandomSudoku
--  return $ all null (map (\x -> freeInRow (fst node) x) positions)

test_for_random_sudoku2 = do
  node <- genRandomSudoku
  return $ consistent $ fst node


test1 :: IO ()
test1 = hspec $ do
  describe "genRandomSudoku" $ do
    it "is consistent" $ do
      node <- genRandomSudoku
      (consistent $ fst node) `shouldBe` True
    it "is solved" $ do
      node <- genRandomSudoku
      (solved node) `shouldBe` True
    


------2
isUniqueWith n x = (not $ uniqueSol $ eraseN n x)

is_minimal = do
	node <- (rsolveNs [emptyN])
	node2 <- genProblem (head $ node)
	return $ isMinimal node2

isMinimal :: Node -> Bool
isMinimal n = uniqueSol n && all (isUniqueWith n) (filledPositions $ fst n)

-- 3
eraseBlock :: Node -> (Row, Column) -> Node
eraseBlock n (r,c) = minimalize n [(x,y) | x <- bl (3*r), y <- bl (3*c)]

eraseBlocks n [] = n
eraseBlocks n (x:xs) = eraseBlocks (eraseBlock n x) xs

mai2 :: IO ()
mai2 = do [r] <- rsolveNs [emptyN]
          showNode r
          s  <- genProblem $ eraseBlocks r [(1,1), (3,1), (3,3), (1,3)]
          showNode s
block :: (Int, Int) -> [Constraint]
block (r,c) = [(x, y, positions) | x <- bl (3*r), y <- bl (3*c)]

emptyNwithEmptyBlocks :: Node
emptyNwithEmptyBlocks = (\ _ -> 0, (block (2,1)) ++ (block (3,1)) ++ (block (1,2)) ++ (block (3,2)) ++ (block (1,3)) ++ (block (2,3)))

mai3 = do [r] <- rsolveNs [emptyNwithEmptyBlocks]
          showNode r


singleton (_, _, [_]) = True
singleton (_, _, _) = False


singleton2 [_] = True
singleton2 _ = False


countNakedSingles :: Node -> Int
countNakedSingles (s,c) = length $ filter singleton c


countHiddenSingles :: Node -> Int
countHiddenSingles (s,c) = length $ filter (hiddenSingle c) c


hiddenSingle :: [Constraint] -> Constraint -> Bool

hiddenSingle c p@(_,_,vs) = singleton2 $ vs `intersect` ( map head (filter singleton2 $ group $ sort $ blaRow c p))



--bla :: [Constraint] -> Constraint -> Bool
--bla xs (r,c,xs) = any (row) xs

blaRow :: [Constraint] -> Constraint -> [Int]
blaRow [] _ = []
blaRow ((r',c',vs):ys) (r,c,xs)
	| r' == r = vs ++ (blaRow ys (r,c,xs))
	| otherwise = (blaRow ys (r,c,xs))




stats :: Node -> [Int]
stats n
  | solved n = []
  | otherwise = (countNakedSingles n + countHiddenSingles n):stats(head $ succNode n)



--bla :: IO ()
--main = hspec $ do
--  describe "Prelude" $ do
--    describe "read" $ do
--      it "can parse integers" $ do
--        read "10" `shouldBe` (10 :: Int)