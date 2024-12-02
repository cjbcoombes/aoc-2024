{-# OPTIONS_GHC -Wno-type-defaults #-}

module Day1 where

import Control.Monad (foldM)
import Data.List (sort)

getInput :: IO ([Int], [Int])
getInput = foldM accum ([], []) [1 .. 1000]
  where
    accum (as, bs) _ = do
      [a, b] <- map read . words <$> getLine
      return (a : as, b : bs)

solve1 :: ([Int], [Int]) -> Int
solve1 (as, bs) = (sum . map abs) (zipWith (-) (sort as) (sort bs))

solve2 :: ([Int], [Int]) -> Int
solve2 (as, bs) = sum (map (\x -> x * length (filter (== x) bs)) as)

part1 :: IO ()
part1 = getInput >>= print . solve1

part2 :: IO ()
part2 = getInput >>= print . solve2
