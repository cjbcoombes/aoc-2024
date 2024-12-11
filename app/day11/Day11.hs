module Day11 where

import qualified Data.Map as M

getInput :: IO [Int]
getInput = map read . words <$> readFile "app/day11/input.txt"

mkMap :: [Int] -> M.Map Int Int
mkMap = foldr (\k m -> M.insertWith (+) k 1 m) M.empty

step :: M.Map Int Int -> M.Map Int Int
step m =
  foldr
    ( \(k, v) m' ->
        foldr (\k' m'' -> M.insertWith (+) k' v m'') m' (expand k)
    )
    M.empty
    (M.assocs m)
  where
    expand 0 = [1]
    expand x =
      let str = show x
          len = length str
       in if even len
            then let (l, r) = splitAt (len `quot` 2) str in [read l, read r]
            else [2024 * x]

solve :: Int -> [Int] -> Int
solve k vals = sum $ foldr (const step) (mkMap vals) [1 .. k]

part1 :: IO ()
part1 = getInput >>= print . solve 25

part2 :: IO ()
part2 = getInput >>= print . solve 75
