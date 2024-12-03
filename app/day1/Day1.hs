{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}

module Day1 where

import Data.List (sort)

getInput :: IO ([Int], [Int])
getInput = do
  contents <- readFile "app/day1/input.txt"
  return $ unzip (map ((\[a, b] -> (a, b)) . map read . words) (lines contents))

solve1 :: ([Int], [Int]) -> Int
solve1 (as, bs) = (sum . map abs) (zipWith (-) (sort as) (sort bs))

solve2 :: ([Int], [Int]) -> Int
solve2 (as, bs) = sum (map (\x -> x * length (filter (== x) bs)) as)

part1 :: IO ()
part1 = getInput >>= print . solve1

part2 :: IO ()
part2 = getInput >>= print . solve2
