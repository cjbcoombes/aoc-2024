{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Day3 where

import Control.Applicative ((<|>))
import Parser
import Result

getInput :: IO String
getInput = readFile "app/day3/input.txt"

mulP :: Parser Char String Int
mulP = mul *> (lp *> ((*) <$> num <*> (comma *> num)) <* rp)
  where
    num = (read :: String -> Int) <$> takeWhile1P (`elem` "0123456789")
    lp = exactP '('
    rp = exactP ')'
    comma = exactP ','
    mul = stringP "mul"

doP :: Parser Char String Int
doP = 0 <$ stringP "do()"

dontP :: Parser Char String Int
dontP = 0 <$ stringP "don't()"

anyP :: Parser Char String Int
anyP = 0 <$ predP (const True)

solve1 :: String -> Int
solve1 s =
  case runParser parser s of
    Accept (i, []) -> i
    Accept _ -> error "didn't consume all"
    Reject _ -> error "parser failed"
  where
    parser = (0 <$ eofP) <|> ((+) <$> mulP <*> parser) <|> (anyP *> parser)

solve2 :: String -> Int
solve2 s =
  case runParser enabled s of
    Accept (i, []) -> i
    Accept _ -> error "didn't consume all"
    Reject _ -> error "parser failed"
  where
    wrap p = (0 <$ eofP) <|> p <|> (anyP *> wrap p)
    enabled = wrap $ ((+) <$> mulP <*> enabled) <|> (dontP *> disabled)
    disabled = wrap (doP *> enabled)

part1 :: IO ()
part1 = getInput >>= print . solve1

part2 :: IO ()
part2 = getInput >>= print . solve2
