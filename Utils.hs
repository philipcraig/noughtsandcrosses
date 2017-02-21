module Utils
( result,
  playgame,
  finished,
  whoWon,
  getstart,
  displayBoard
) where

import Data.List
import Data.Maybe


result b
  | (b!!0 /= '-') && (b!!0 == b!!1) && (b!!1==b!!2) =  (b!!0)
  | (b!!3 /= '-') && (b!!3 == b!!4) && (b!!4==b!!5) =  (b!!3)
  | (b!!6 /= '-') && (b!!6 == b!!7) && (b!!7==b!!8) =  (b!!6)
  | (b!!0 /= '-') && (b!!0 == b!!3) && (b!!3==b!!6) =  (b!!0)
  | (b!!1 /= '-') && (b!!1 == b!!4) && (b!!4==b!!7) =  (b!!1)
  | (b!!2 /= '-') && (b!!2 == b!!5) && (b!!5==b!!8) =  (b!!2)
  | (b!!0 /= '-') && (b!!0 == b!!4) && (b!!4==b!!8) =  (b!!0)
  | (b!!2 /= '-') && (b!!2 == b!!4) && (b!!4==b!!6) =  (b!!2)
  | otherwise = '-'
  
-- Are there blank squares available?
finished b = do
  if (result b) /= '-'
    then True
  else
    isNothing ('-' `elemIndex` b) 


getstart = ['-','-','-','-','-','-','-','-','-']   

displayBoard b = do
  " " ++ show (b!!0) ++ show (b!!1) ++ show (b!!2) ++ "\n\
  \ " ++ show (b!!3) ++ show (b!!4) ++ show (b!!5) ++ "\n\
  \ " ++ show (b!!6) ++ show (b!!7) ++ show (b!!8) ++ "\n" 
   
whoWon b s1 s2
  | (result b) == s1 = "Player 1 wins"  
  | (result b) == s2 = "Player 2 wins"  
  | (result b) == '-' =  "Tie game"  
  | otherwise = "No idea how you got here"

replaceNth n newVal vec
     | n == 0 = newVal:(tail vec)
     | otherwise = (head vec):(replaceNth (n-1) newVal (tail vec))


applyMove index symbol board = do
  if (board !! index) /= '-'
    then error ((show index) ++ (displayBoard board) ++ "\nEverything went wrong")
  else
    replaceNth index symbol board    
     

playgame m1 m2 s1 s2 b bprev = do 
  if finished b  
    then b
  else playgame m2 m1 s2 s1 (applyMove (m1 s1 b) s1 b) b 



