{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Day3 where

import Parser
import Result

getInput :: IO String
getInput = readFile "app/day3/input.txt"

mulParser :: Parser Char String Int
mulParser = expr
  where
    num = (read :: String -> Int) <$> takeWhile1P (`elem` "0123456789")
    lp = exactP '('
    rp = exactP ')'
    comma = exactP ','
    mul = stringP "mul"
    expr = mul *> (lp *> ((*) <$> num <*> (comma *> num)) <* rp)

solve1 :: String -> Int
solve1 [] = 0
solve1 s@(_ : xs) =
  case runParser mulParser s of
    Accept (n, rest) -> n + solve1 rest
    Reject _ -> solve1 xs

solve2 :: String -> Int
solve2 [] = 0
solve2 s@(_ : xs) =
  case runParser mulParser s of
    Accept (n, rest) -> n + solve2 rest
    Reject _ -> case runParser dont s of
      Accept (_, rest) -> solve2 (untilDo rest)
      Reject _ -> solve2 xs
  where
    doo = stringP "do()"
    dont = stringP "don't()"
    untilDo [] = []
    untilDo s@(_ : xs) = case runParser doo s of
      Accept (_, rest) -> rest
      Reject _ -> untilDo xs

part1 :: IO ()
part1 = getInput >>= print . solve1

part2 :: IO ()
part2 = getInput >>= print . solve2
