{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignoreU "Redundant if" #-}
module Day4 where

import Data.Array.Unboxed

getInput :: IO (UArray (Int, Int) Char)
getInput = do
  contents <- lines <$> readFile "app/day4/input.txt"
  return $ listArray ((1, 1), (length contents, length (head contents))) (concat contents)

checkXmas :: UArray (Int, Int) Char -> (Int, Int) -> Int
checkXmas arr (y, x) = dir (1, 0) + dir (-1, 0) + dir (0, 1) + dir (0, -1) + dir (1, 1) + dir (1, -1) + dir (-1, 1) + dir (-1, -1)
  where
    dir (dy, dx) = if inDir (dy, dx) then 1 else 0
    inDir (dy, dx) =
        traverse (\i -> arr !? (y + dy * i, x + dx * i)) [0, 1, 2, 3] == Just "XMAS"

solve1 :: UArray (Int, Int) Char -> Int
solve1 arr = sum $ map (checkXmas arr) (range (bounds arr))

checkMasx :: UArray (Int, Int) Char -> (Int, Int) -> Int
checkMasx arr (y, x) = (dir (1,1) + dir(-1,-1)) * (dir(-1,1) + dir(1,-1))
  where
    dir (dy, dx) = if inDir (dy, dx) then 1 else 0
    inDir (dy, dx) =
        traverse (\i -> arr !? (y + dy * i, x + dx * i)) [-1, 0, 1] == Just "MAS"

solve2 :: UArray (Int, Int) Char -> Int
solve2 arr = sum $ map (checkMasx arr) (range (bounds arr))

part1 :: IO ()
part1 = getInput >>= print . solve1

part2 :: IO ()
part2 = getInput >>= print . solve2
