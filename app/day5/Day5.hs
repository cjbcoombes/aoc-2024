{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Day5 where

import Data.List (sortBy)
import Data.List.Split (splitOn)
import Data.Map (fromList, union, (!))
import Data.Tuple (swap)

getInput :: IO ([(Int, Int)], [[Int]])
getInput = do
  contents <- lines <$> readFile "app/day5/input.txt"
  let (edges, rows) = drop 1 <$> break null contents
      edges' = map ((\[a, b] -> (a, b)) . map read . splitOn "|") edges
      rows' = map (map read . splitOn ",") rows
  return (edges', rows')

sorting :: [(Int, Int)] -> (Int, Int) -> Bool
sorting edges = (m !)
  where
    m = fromList (map (,True) edges) `union` fromList (map ((,False) . swap) edges)

valid :: ((Int, Int) -> Bool) -> [Int] -> Bool
valid c (a : b : xs) = c (a, b) && valid c (b : xs)
valid _ _ = True

solve1 :: [(Int, Int)] -> [[Int]] -> Int
solve1 edges rows = sum m
  where
    v = valid (sorting edges)
    r = filter v rows
    m = map (\x -> x !! (length x `quot` 2)) r

solve2 :: [(Int, Int)] -> [[Int]] -> Int
solve2 edges rows = sum m
  where
    c = sorting edges
    v = not . valid c
    r = filter v rows
    s = map (sortBy (\x y -> if c (x, y) then LT else GT)) r
    m = map (\x -> x !! (length x `quot` 2)) s

part1 :: IO ()
part1 = getInput >>= print . uncurry solve1

part2 :: IO ()
part2 = getInput >>= print . uncurry solve2
