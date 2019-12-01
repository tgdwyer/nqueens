module Lib
( checkBoard
, nQueens
, eightQueens
) where
import Control.Monad
import Data.List (tails,unfoldr)

type Board = [Int]

-- | Check the first queen against all her sisters.
check :: Board -> Bool
check [] = True
check (q:qs) = all ok $ zip [1..] qs
  where ok (hdist,q') = q /= q' -- not same row
                        && vdist /= hdist -- not on a diagonal
                          where vdist = abs (q-q')

-- | Build a set of nQueens solution boards by recursing inside the list monad.
nQueens_ListMonad :: Int -> [Board]
nQueens_ListMonad n = let
    extend :: Int -> Board -> [Board]
    extend 0 fullBoard = [fullBoard]
    extend remaining partialBoard = do
      -- Non-deterministically "choose" a value.
      q <- [0..n-1]
      -- Glue it on the front.
      let extended = q:partialBoard
      -- Make sure it's legal.
      guard $ check extended
      -- Recurse.
      extend (remaining-1) extended
  in
    extend n []

-- | check a full board
checkBoard qs = all check $ tails qs

-- | fold the concatMap of a function that uses a list comprehension to generate partial solutions
nQueens_ListComprehension :: Int -> [Board]
nQueens_ListComprehension n =
  let
    -- for a given partial board (set of columns),
    -- prepend every column possible
    addQueen :: Board -> [Board]
    addQueen board = [boards | boards <- [r:board | r <- [0..n-1]], checkBoard boards]
  in
    foldr (const $ concatMap addQueen) [[]] [0..n-1]

-- | create a list of solutions to nQueens for a given n
nQueens :: Int -> [Board]
nQueens = nQueens_ListMonad

eightQueens :: [Board]
eightQueens = nQueens 8



