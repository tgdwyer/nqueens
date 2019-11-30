module Lib
( checkBoard
, nQueens
, eightQueens
) where
import Control.Monad
import Data.List (tails)

type Board = [Int]

-- Check the first queen against all her sisters.
check :: Board -> Bool
check [] = True
check (q:qs) = all ok $ zip [1..] qs
  where ok (hdist,q') = q /= q' -- not same row
                        && vdist /= hdist -- not on a diagonal
                          where vdist = abs (q-q')

-- list monad version
nQueensLM n = extend n n []

extend :: Int -> Int -> Board -> [Board]
extend n 0 partial = return partial
extend n remaining partial = do
  -- Non-deterministically "choose" a value.
  q <- [0..n-1]
  -- Glue it on the front.
  let extended = q:partial
  -- Make sure it's legal.
  guard $ check extended
  -- Recurse.
  extend n (remaining-1) extended

-- check a full board
checkBoard qs = all check $ tails qs

nQueensLC n =
    let
        -- for a given set of columns,
        -- prepend every column possible
        addQueen :: Board -> [Board]
        addQueen cols = filter check [r:cols | r <- [0..n-1]]

        -- generate k columns of feasible queens
        queens :: Int -> [Board] -> [Board]
        queens 0 s = s
        queens k s = queens (k-1) (concatMap addQueen s)
    in
        queens n [[]]


nQueens :: Int -> [Board]
nQueens = nQueensLC

eightQueens :: [Board]
eightQueens = nQueens 8



