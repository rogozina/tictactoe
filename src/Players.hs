module Players  where

import TicTacToe
import System.IO

playerCL :: PlayerIO
playerCL =  PlayerIO $ \_ _ -> do
  putStr $ "Pick a cell\nInput: "
  hFlush stdout
  cell <- getLine
  return (read cell)

playerMinimax :: PlayerIO
playerMinimax =  PlayerIO $ \board xo -> do
  let cell = pickMove xo board
  putStrLn $ "Move: " ++ show cell
  return (cell)

assignMinimax :: XO -> Int -> [Cell] -> Int
assignMinimax xo depth board 
          | isWin board X = 10 - depth
          | isWin board O = -10 + depth
          | emptyCells board == [] = 0
          | otherwise = case xo of
              X -> foldr max (-100) $ 
                map (assignMinimax O (depth+1)) 
                (nextBoards xo board)
              O -> foldr min 100 $ 
                map (assignMinimax X (depth+1)) 
                (nextBoards xo board)


pickMove :: XO -> [Cell] -> Int
pickMove X board = fst $ foldr 
            (\(i,x) (j,ini) -> if ini < x then (i,x) else (j,ini)) 
            (0, -100) $ fmap (fmap (assignMinimax O 0)) (nextBoardsIdx O board)
pickMove O board = fst $ foldr 
            (\(i,x) (j,ini) -> if ini > x then (i,x) else (j,ini))
            (0, 100) $ fmap (fmap (assignMinimax X 0)) (nextBoardsIdx O board)

nextBoards :: XO -> [Cell] -> [[Cell]] 
nextBoards xo board = [ assignCell board i xo | i <- emptyCells board]

nextBoardsIdx :: XO -> [Cell] -> [(Int,[Cell])] 
nextBoardsIdx xo board = [ (i,assignCell board i xo) | i <- emptyCells board]

