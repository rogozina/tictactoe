module TicTacToe where

import Data.List

data XO = X | O
    deriving (Eq)
data Cell = Occupied XO | Empty
    deriving (Eq)
newtype Board = Board {getBoard :: [Cell]}

newtype PlayerIO = PlayerIO { runPlayer :: [Cell] -> XO -> IO Int}
 
game :: PlayerIO -> PlayerIO -> IO ()
game player1 player2 = do
    putStrLn "Cells's coordinates:\n1|2|3\n------\n4|5|6\n------\n7|8|9\n"
    playRound (replicate 9 Empty) X player1 player2

playRound :: [Cell] -> XO -> PlayerIO -> PlayerIO -> IO ()
playRound board xo player1 player2 = do
  putStrLn $ (show xo) ++ " 's turn."
  putStrLn $ show $ Board board
  cell <- runPlayer player1 board xo
  case assignCellSafe board cell xo of
    Nothing -> do
      putStrLn "Invalid index"
      playRound board xo player1 player2
    Just newBoard -> do
      if isWin newBoard xo then do
        putStrLn $ ((show xo) ++ " wons!")
        putStrLn $ show $ Board newBoard
        return ()
      else if emptyCells newBoard == [] then do
        putStrLn $ ("Tie!")
        putStrLn $ show $ Board newBoard
        return ()
      else playRound newBoard (nextMove xo) player2 player1

nextMove X = O
nextMove O = X

assignCellSafe :: [Cell] -> Int -> XO -> Maybe [Cell]
assignCellSafe board idx xo = 
        if any (==idx) (emptyCells board) then Just $ assignCell board idx xo
        else Nothing

assignCell :: [Cell] -> Int -> XO -> [Cell]
assignCell board idx xo = fmap (\(i,x) -> if (i == idx) then Occupied xo else x) 
                               (zip [0..8] board)
emptyCells :: [Cell] -> [Int]
emptyCells board = [i | (i,x) <- zip [0..8] board, x == Empty]

------Win Checks--------      
winIdxs = [[3*i + j | j <- [0,1,2]]| i <- [0,1,2]] ++
        [[3*i + j | i <- [0,1,2]]| j <- [0,1,2]] ++
        [[3*i + i | i <- [0,1,2]]] ++
        [[3*i + j | i <- [0,1,2], j <- [0,1,2], i == 2-j ]]

isWin :: [Cell] -> XO -> Bool
isWin board xo = any id [check board idxs xo | idxs <- winIdxs]

check :: [Cell] -> [Int] -> XO -> Bool
check board idxs xo = all (== Occupied xo) [(board !! idx) | idx <- idxs]  

-------Printing--------
instance Show XO where
  show X = "X"
  show O = "O"

instance Show Cell where
  show (Occupied X)     = "X"
  show (Occupied O)    = "O"
  show Empty            = " "

instance Show Board where
  show (Board board) =
    renderRow firstRow ++
    "\n----------\n" ++
    renderRow secondRow ++
    "\n----------\n" ++
    renderRow thirdRow ++ "\n"
    where firstRow  = take 3 board
          secondRow = drop 3 . take 6 $ board
          thirdRow  = drop 6 board
          renderRow row = intercalate " | " $ (fmap show) row


