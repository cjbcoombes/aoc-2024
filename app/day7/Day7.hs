{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Day7 where

import Data.Foldable (foldl')
import Data.Function ((&))

data Row = Row Integer [Integer] deriving (Show)

getInput :: IO [Row]
getInput = do
  contents <- lines <$> readFile "app/day7/input.txt"
  let mkrow (a, b) = Row (read a) (map read . drop 1 . words $ b)
  return $ map (mkrow . break (== ':')) contents

solveRow :: [Integer -> Integer -> Integer] -> Row -> Integer
solveRow _ (Row _ []) = 0
solveRow ops (Row s (x : xs)) = if valid then s else 0
  where
    valid = any ((== s) . foldl' (&) x) (mapM (\y -> [(`op` y) | op <- ops]) xs)
    -- and this one from the internet
    valid' = s `elem` foldl' (\a e -> filter (<= s) $ ops <*> a <*> [e]) [x] xs

conc :: Integer -> Integer -> Integer
conc a b = (a * 10 ^ dig b) + b
  where
    dig n
      | n < 10 = 1 :: Integer
      | otherwise = 1 + dig (n `quot` 10)

part1 :: IO ()
part1 = getInput >>= print . sum . map (solveRow [(+), (*)])

part2 :: IO ()
part2 = getInput >>= print . sum . map (solveRow [(+), (*), conc])
