module Main where

import Player1
import Player2
import Board

main :: IO ()
main = do
  putStrLn $ showBoard result
  putStrLn $ whoWon result
  where 
    result = playGame Player1.move Player2.move
