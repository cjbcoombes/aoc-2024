module Day2 where

import Utils

getInput :: IO [[Int]]
getInput = do
  contents <- readFile "app/day2/input.txt"
  return $ map (map read . words) (lines contents)

solve1 :: [[Int]] -> Int
solve1 xs = count valid ys
  where
    -- fun use of <*>, but I didn't come up with it
    ys = map (zipWith (-) <*> drop 1) xs
    valid x = all (allSat [(1 <=), (<= 3)]) x || all (allSat [(-3 <=), (<= -1)]) x

solve2 :: [[Int]] -> Int
solve2 = count skipValid
  where
    -- fun use of <*>, but I didn't come up with it
    pairs = zipWith (-) <*> drop 1
    valid x =
      let y = pairs x
       in all (allSat [(1 <=), (<= 3)]) y || all (allSat [(-3 <=), (<= -1)]) y
    cut l x = uncurry (++) (fmap (drop 1) (splitAt x l))
    skipValid x = any (valid . cut x) [0 .. (length x)]

part1 :: IO ()
part1 = getInput >>= print . solve1

part2 :: IO ()
part2 = getInput >>= print . solve2
