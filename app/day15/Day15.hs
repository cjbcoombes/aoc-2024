{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}

module Day15 where

import Control.Monad (when)
import Control.Monad.ST
import Data.Array.Base hiding (bounds)
import Data.Array.ST
import Data.Foldable (foldlM)
import Debug.Trace
import Grid hiding (arr, bounds)

getInput :: IO (Grid Char, Pos, [Dir])
getInput = do
  contents <- lines <$> readFile "app/day15/input.txt"
  let (gridLines, code) = break null contents
      grid = gridFromList gridLines
      dir '^' = up
      dir 'v' = down
      dir '<' = left
      dir '>' = right
      pos = head $ filter ((== '@') . gridAt grid) (gridRange grid)
  return (grid, pos, map dir (concat code))

readOrWall :: STUArray s Pos Char -> Pos -> ST s Char
readOrWall arr (x, y) = do
  (_, (h, w)) <- getBounds arr
  if 0 <= x && x <= w && 0 <= y && y <= h
    then readArray arr (y, x)
    else return '#'

gridStep :: STUArray s Pos Char -> Pos -> Dir -> ST s Pos
gridStep arr pos dir = do
  let nextPos = step pos dir
  next <- readOrWall arr nextPos
  case next of
    '.' -> do
      writeArray arr (fl pos) '.'
      writeArray arr (fl nextPos) '@'
      return nextPos
    'O' -> do
      (spcPos, spc) <- findSpc nextPos
      if spc == '.'
        then do
          writeArray arr (fl pos) '.'
          writeArray arr (fl spcPos) 'O'
          writeArray arr (fl nextPos) '@'
          return nextPos
        else return pos
    '#' -> return pos
  where
    findSpc pos' = do
      x <- readOrWall arr pos'
      if x == 'O'
        then findSpc (step pos' dir)
        else return (pos', x)

solve1 :: (Grid Char, Pos, [Dir]) -> Int
solve1 (Grid arr bounds, pos, dirs) =
  sum . map (\(x, y) -> 100 * y + x) . filter ((== 'O') . gridAt grid') $ gridRange grid'
  where
    grid' = Grid arr' bounds
    arr' = runSTUArray $ do
      a <- thawSTUArray arr
      foldlM (gridStep a) pos dirs
      return a

part1 :: IO ()
part1 = getInput >>= print . solve1

part2 :: IO ()
part2 = getInput >>= print
