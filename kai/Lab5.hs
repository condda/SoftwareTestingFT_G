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






















--type Node5 = (Node, Node, Node, Node, Node)
--get1node5 (x,_,_,_,_) = x
--get2node5 (_,x,_,_,_) = x
--get3node5 (_,_,x,_,_) = x
--get4node5 (_,_,_,x,_) = x
--get5node5 (_,_,_,_,x) = x

--generateNode5 :: IO Node5
--generateNode5 = do
--  [n1] <- rsolveNs [emptyN]
--  return $ mergeOuter $ (n1, emptyN, emptyN, emptyN, emptyN)

--rsolveNode5 :: Node5 -> IO Node5
--rsolveNode5 (n1, n2, n3, n4, n5) = do
--  [n1'] <- rsolveNs [n1]
--  [n2'] <- rsolveNs [n2]
--  [n3'] <- rsolveNs [n3]
--  [n4'] <- rsolveNs [n4]
--  [n5'] <- rsolveNs [n5]
--  return (n1', n2', n3', n4', n5')


--const2Pos :: Constraint -> (Row, Column)
--const2Pos (r,c, _) = (r,c)

--diago :: (Row, Column) -> Node -> Node -> Node
--diago (x,y) (s1,c1) (s2, c2)  = (\(r,c) -> if (not $ sameblock (x,y) (r,c)) then s2 (tr r, tr c) else s1 (r,c) ,
--  (filter (\c -> not $ constInSameBlock (x,y) c) c1) ++ (map trConstraint (filter (constInSameBlock (tr x, tr y)) c2)))

--constInSameBlock :: (Row, Column) -> Constraint -> Bool
--constInSameBlock x c = not $ sameblock x $ const2Pos c

--trConstraint :: Constraint -> Constraint
--trConstraint (r,c,vs) = (tr r, tr c, vs)

--trPosition :: (Row, Column) -> (Row, Column)
--trPosition (r,c) = (tr r, tr c)

--mergeOuter :: Node5 -> Node5
--mergeOuter (n1, n2, n3, n4, n5) = (n1, diago (7,7) n2 n1, diago (1,7) n3 n1, diago (7,1) n4 n1, diago (1,1) n5 n1)

--tr 1 = 7
--tr 2 = 8
--tr 3 = 9
--tr 4 = 4
--tr 5 = 5
--tr 6 = 6
--tr 7 = 1
--tr 8 = 2
--tr 9 = 3

--showAll :: IO()
--showAll = do
--  (n1, n2, n3, n4, n5) <- generateNode5
--  showNode n1
--  showNode n2
--  showNode n3
--  showNode n4
--  showNode n5


--showAll2 :: IO()
--showAll2 = do
--  bla <- generateNode5
--  (n1, n2, n3, n4, n5) <- genProblem5 $ return bla
--  showNode n1
--  showNode n2
--  showNode n3
--  showNode n4
--  showNode n5


--genProblem5 :: IO Node5 -> IO Node5
--genProblem5 (n1, n2, n3, n4, n5) = do
--  n1' <- genProblem n1
--  (n1'', n2'', n3'', n4'', n5'') <- deleteQQQSuds (n1', n2, n3, n4, n5)
--  n2p <- genProblem n2''
--  n3p <- genProblem n3''
--  n4p <- genProblem n4''
--  n5p <- genProblem n5''
--  return (n1'', n2p, n3p, n4p, n5p)


--deleteQQQSuds :: Node5 -> Node5
--deleteQQQSuds (n1, n2, n3, n4, n5)  = (n1, deleteQQQ (7,7) n2 n1, deleteQQQ (1,7) n3 n1, deleteQQQ (7,1) n4 n1, deleteQQQ (1,1) n5 n1)

--deleteQQQ :: (Row, Column) -> Node -> Node -> Node
--deleteQQQ (x,y) (s1,c1) (s2,c2) = (\(r,c) -> if (not $ sameblock (x,y) (r,c)) then s2 (tr r, tr c) else s2 (r,c) ,
--  (filter (\c -> not $ constInSameBlock (x,y) c) c1) ++ (map trConstraint (filter (constInSameBlock (tr x, tr y)) c2)))




--in_grid x y (r,c,vs) = (x >= r && x + 9 <= r) && (y >= c && y + 9 <= c)
--not_in_grid x y (r,c,vs) = not ((x >= r && x + 9 <= r) && (y >= c && y + 9 <= c))

--moveS :: Int -> Int -> ((Row,Column) -> Value) -> (Row,Column) -> Value
--moveS r' c' s = (\(r,c) -> s (r - r',c - c'))

--moveC :: Int -> Int -> Constraint -> Constraint
--moveC r' c' (r,c,vs) = (r - r', c - c', vs)

--extract_subsudoku :: Int -> Int -> [Node] -> [Node]
--extract_subsudoku _ _ [] = []
--extract_subsudoku x y ((parent_s, parent_c):ns) = ((moveS x y) $ parent_s, map (moveC x y) $ filter (in_grid x y) parent_c):(extract_subsudoku ns)



--merge_nodes :: [Node] -> [Node] -> [Node]
--merge_nodes [] [] = []
--merge_nodes ((s1, c1):xs) ((s2, c2):ys) = ((\x y -> if s1 x y /= 0 then s1 x y else s2 x y), c1 ++ c2): (merge_nodes xs ys)

--call_function_on_subsudoku :: Int -> Int -> [Node] -> ([Node] -> IO [Node]) -> IO [Node]
--call_function_on_subsudoku r c n f = do
--  sub <- f $ extract_subsudoku r c n
--  return $ merge_nodes (extract_subsudoku (-r) (-c) sub) (fst n , filter (not $ in_grid r c) $ snd n)

--main5 :: IO ()
--main5 = do sub_node <- rsolveNs [emptyN]
--           sub_node <- call_function_on_subsudoku 7 7 sub_node rsolveNs
--           sub_node <- call_function_on_subsudoku 0 12 sub_node rsolveNs
--           sub_node <- call_function_on_subsudoku 12 0 sub_node rsolveNs
--           sub_node <- call_function_on_subsudoku 12 12 sub_node rsolveNs
           
--           showNode (head $ sub_node)










--valuesFor :: IO ()
--main = hspec $ do
--  describe "Prelude" $ do
--    describe "read" $ do
--      it "can parse integers" $ do
--        read "10" `shouldBe` (10 :: Int)