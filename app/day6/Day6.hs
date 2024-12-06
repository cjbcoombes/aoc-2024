{-# LANGUAGE LambdaCase #-}

module Day6 where

import Data.Array.Unboxed
import Data.List (elemIndex, findIndex)
import Data.Maybe (fromJust)
import qualified Data.Set as S
import Utils

newtype Grid = Grid (UArray (Int, Int) Char) deriving (Show)

data Pos = Pos {pos :: (Int, Int), dir :: (Int, Int)} deriving (Eq, Ord, Show)

getInput :: IO (Grid, Pos)
getInput = do
  contents <- lines <$> readFile "app/day6/input.txt"
  let y = fromJust $ findIndex ('^' `elem`) contents
      x = fromJust $ elemIndex '^' (contents !! y)
      arr = listArray ((1, 1), (length contents, length (head contents))) (concat contents)
  return (Grid arr, Pos (x + 1, y + 1) (0, -1))

rot :: (Int, Int) -> (Int, Int)
rot (dx, dy) = (-dy, dx)

rotPos (Pos p d) = Pos p (rot d)

stepPos (Pos (x, y) (dx, dy)) = Pos (x + dx, y + dy) (dx, dy)

step :: Grid -> Pos -> Maybe Pos
step (Grid arr) p@(Pos (x, y) (dx, dy)) =
  (arr !? (y + dy, x + dx)) >>= \case
    '#' -> step (Grid arr) (rotPos p)
    _ -> Just (stepPos p)

posSet :: Grid -> Pos -> S.Set Pos
posSet g p = case step g p of
  Nothing -> S.singleton p
  Just p' -> S.insert p (posSet g p')

solve1 :: Grid -> Pos -> Int
solve1 g p = S.size . S.map pos $ posSet g p

loops :: Grid -> Pos -> Bool
loops g = loops' S.empty
  where
    loops' s p =
      S.member p s
        || case step g p of
          Nothing -> False
          Just p' -> loops' (S.insert p s) p'

solve2 :: Grid -> Pos -> Int
solve2 (Grid arr) p = count (flip loops p . putBlock arr) (range . bounds $ arr)
  where
    putBlock arr' (x, y) =
      if arr' ! (y, x) /= '.'
        then Grid arr'
        else Grid $ arr' // [((y, x), '#')]

part1 :: IO ()
part1 = getInput >>= print . uncurry solve1

part2 :: IO ()
part2 = getInput >>= print . uncurry solve2