module Day18 where

import Data.List.Split (splitOn)
import Data.Sequence ((|>))
import qualified Data.Sequence as Q
import qualified Data.Set as S

getInput :: IO [(Int, Int)]
getInput = do
  contents <- lines <$> readFile "app/day18/input.txt"
  return [(read x, read y) | [x, y] <- map (splitOn ",") contents]

edgeX :: Int
edgeX = 70

edgeY :: Int
edgeY = 70

numCorrupted :: Int
numCorrupted = 1024

inBounds :: (Int, Int) -> Bool
inBounds (x, y) = 0 <= x && 0 <= y && x <= edgeX && y <= edgeY

bfs1 :: S.Set (Int, Int) -> Q.Seq (Int, Int, Int) -> S.Set (Int, Int) -> Int
bfs1 _ Q.Empty _ = -1
bfs1 seen ((x, y, n) Q.:<| xs) blocked
  | not (inBounds (x, y))
      || (x, y) `S.member` seen
      || (x, y) `S.member` blocked =
      bfs1 seen xs blocked
  | x == edgeX && y == edgeY = n
  | otherwise =
      bfs1
        ((x, y) `S.insert` seen)
        (xs |> (x - 1, y, n + 1) |> (x + 1, y, n + 1) |> (x, y - 1, n + 1) |> (x, y + 1, n + 1))
        blocked

part1 :: IO ()
part1 = do
  inp <- take numCorrupted <$> getInput
  print (bfs1 S.empty (Q.singleton (0, 0, 0)) (S.fromList inp))

part2 :: IO ()
part2 = getInput >>= print
