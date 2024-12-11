module Day10 where

import Control.Monad (mfilter)
import Data.Array.Unboxed
import Data.List (singleton)
import qualified Data.Set as S

data Grid = Grid (UArray (Int, Int) Int) (Int, Int) deriving (Show)

getInput :: IO Grid
getInput = do
  contents <- lines <$> readFile "app/day10/input.txt"
  let bnds = (length contents, length (head contents))
      arr = listArray ((1, 1), bnds) (map (read . singleton) (concat contents))
  return $ Grid arr bnds

allFrom1 :: UArray (Int, Int) Int -> Int -> (Int, Int) -> S.Set (Int, Int)
allFrom1 arr k (y, x) = maybe S.empty search $ mfilter (== k) (arr !? (y, x))
  where
    search 9 = S.singleton (y, x)
    search _ = foldMap (allFrom1 arr (k + 1)) neighbors
    neighbors = [(y, x + 1), (y, x - 1), (y + 1, x), (y - 1, x)]

solve1 :: Grid -> Int
solve1 (Grid arr bnds) = sum $ map (S.size . allFrom1 arr 0) (range ((1, 1), bnds))

allFrom2 :: UArray (Int, Int) Int -> Int -> (Int, Int) -> Int
allFrom2 arr k (y, x) = maybe 0 search $ mfilter (== k) (arr !? (y, x))
  where
    search 9 = 1
    search _ = sum $ map (allFrom2 arr (k + 1)) neighbors
    neighbors = [(y, x + 1), (y, x - 1), (y + 1, x), (y - 1, x)]

solve2 :: Grid -> Int
solve2 (Grid arr bnds) = sum $ map (allFrom2 arr 0) (range ((1, 1), bnds))

part1 :: IO ()
part1 = getInput >>= print . solve1

part2 :: IO ()
part2 = getInput >>= print . solve2
