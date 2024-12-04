{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Redundant if" #-}
module Day4 where

import Data.Array

getInput :: IO (Array (Int, Int) Char)
getInput = do
  contents <- lines <$> readFile "app/day4/input.txt"
  return $ listArray ((1, 1), (length contents, length (head contents))) (concat contents)

checkXmas :: Array (Int, Int) Char -> (Int, Int) -> Int
checkXmas arr (y, x) = dir (1, 0) + dir (-1, 0) + dir (0, 1) + dir (0, -1) + dir (1, 1) + dir (1, -1) + dir (-1, 1) + dir (-1, -1)
  where
    rng = bounds arr
    dir (dy, dx) = if inDir (dy, dx) then 1 else 0
    inDir (dy, dx) =
      if inRange rng (y + dy * 3, x + dx * 3)
        then map (\i -> arr ! (y + dy * i, x + dx * i)) [0, 1, 2, 3] == "XMAS"
        else False

solve1 :: Array (Int, Int) Char -> Int
solve1 arr = sum $ map (checkXmas arr) (range (bounds arr))

checkMasx :: Array (Int, Int) Char -> (Int, Int) -> Bool
checkMasx arr (y, x) =
  if inRange rng (y - 1, x - 1) && inRange rng (y + 1, x + 1)
    then ([a, b] `elem` ["MS", "SM"]) && ([c, d] `elem` ["MS", "SM"]) && e == 'A'
    else False
  where
    rng = bounds arr
    a = arr ! (y - 1, x - 1)
    b = arr ! (y + 1, x + 1)
    c = arr ! (y - 1, x + 1)
    d = arr ! (y + 1, x - 1)
    e = arr ! (y, x)

solve2 :: Array (Int, Int) Char -> Int
solve2 arr = sum $ map ((\x -> if x then 1 else 0) . checkMasx arr) (range (bounds arr))

part1 :: IO ()
part1 = getInput >>= print . solve1

part2 :: IO ()
part2 = getInput >>= print . solve2
