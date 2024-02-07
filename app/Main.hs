module Main (main) where

import TicTacToe (game)
import Players (playerCL, playerMinimax)

main :: IO ()
main = game playerCL playerMinimax
