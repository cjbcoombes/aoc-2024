module Day8 where

import Control.Monad (guard)
import qualified Data.Map as M
import qualified Data.Set as S

getInput :: IO ((Int, Int), M.Map Char [(Int, Int)])
getInput = do
  contents <- lines <$> readFile "app/day8/input.txt"
  let coords = zip [0 ..] (map (zip [0 ..]) contents)
      bounds = (length (head contents), length contents)
      mapUpdate m x y c = if c == '.' then m else M.insertWith (++) c [(x, y)] m
      antMap = foldr (\(y, row) acc1 -> foldr (\(x, c) acc2 -> mapUpdate acc2 x y c) acc1 row) M.empty coords
  return (bounds, antMap)

solveAnt :: [Int] -> (Int, Int) -> [(Int, Int)] -> S.Set (Int, Int)
solveAnt range bounds l = S.fromList . filter (inBounds bounds) $ do
  a@(ax, ay) <- l
  b@(bx, by) <- l
  guard (a /= b)
  k <- range
  let (dx, dy) = (ax - bx, ay - by)
  return (ax + dx * k, ay + dy * k)

inBounds :: (Int, Int) -> (Int, Int) -> Bool
inBounds (w, h) (x, y) = 0 <= x && x < w && 0 <= y && y < h

solve :: [Int] -> (Int, Int) -> M.Map Char [(Int, Int)] -> Int
solve range bounds m = S.size . S.unions $ map (solveAnt range bounds) (M.elems m)

part1 :: IO ()
part1 = getInput >>= print . uncurry (solve [1, -2])

part2 :: IO ()
part2 = do
  ((w, h), m) <- getInput
  let k = max w h
  print $ solve [(-k) .. k] (w, h) m
