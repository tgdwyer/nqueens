module Main where

import Lib (nQueens, eightQueens)

-- Q . . . . . . .
-- . . . . Q . . .
-- . Q . . . . . .
-- . . . . . Q . .
-- . . Q . . . . .
-- . . . . . . Q .
-- . . . Q . . . .
-- . . . . . . . Q
q1 = [0,2,4,6,1,3,5,7]

main :: IO ()
main = print $ nQueens 4
