module Day10 where

import Control.Monad (mfilter)
import Data.Array.Unboxed
import Data.Containers.ListUtils (nubOrd)
import Data.List (singleton)

data Grid = Grid (UArray (Int, Int) Int) (Int, Int) deriving (Show)

getInput :: IO Grid
getInput = do
  contents <- lines <$> readFile "app/day10/input.txt"
  let bnds = (length contents, length (head contents))
      arr = listArray ((1, 1), bnds) (map (read . singleton) (concat contents))
  return $ Grid arr bnds

allFrom :: UArray (Int, Int) Int -> Int -> (Int, Int) -> [(Int, Int)]
allFrom arr k (y, x) = maybe [] search $ mfilter (== k) (arr !? (y, x))
  where
    search 9 = [(y, x)]
    search _ = concatMap (allFrom arr (k + 1)) neighbors
    neighbors = [(y, x + 1), (y, x - 1), (y + 1, x), (y - 1, x)]

solve :: Grid -> [[(Int, Int)]]
solve (Grid arr bnds) = map (allFrom arr 0) (range ((1, 1), bnds))

part1 :: IO ()
part1 = getInput >>= print . length . concatMap nubOrd . solve

part2 :: IO ()
part2 = getInput >>= print . length . concat . solve
