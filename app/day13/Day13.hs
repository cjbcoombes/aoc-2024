{-# OPTIONS_GHC -Wno-unused-do-bind #-}

module Day13 where

import Control.Monad (guard)
import Data.Maybe (fromMaybe)
import Data.Void (Void)
import Text.Megaparsec (Parsec, eof, many, optional, parseMaybe, some)
import Text.Megaparsec.Char (digitChar, eol, string)

type Parser = Parsec Void String

data Machine = Machine (Integer, Integer) (Integer, Integer) (Integer, Integer) deriving (Show, Eq)

getInput :: IO [Machine]
getInput = concat . parseMaybe (many machine <* eof) <$> readFile "app/day13/input.txt"
  where
    machine :: Parser Machine
    machine = do
      (ax, ay) <- (,) <$> (string "Button A: X+" *> some digitChar) <*> (string ", Y+" *> some digitChar) <* eol
      (bx, by) <- (,) <$> (string "Button B: X+" *> some digitChar) <*> (string ", Y+" *> some digitChar) <* eol
      (px, py) <- (,) <$> (string "Prize: X=" *> some digitChar) <*> (string ", Y=" *> some digitChar) <* eol
      optional eol
      return $ Machine (read ax, read ay) (read bx, read by) (read px, read py)

solve :: [Machine] -> Integer
solve = sum . map (cost . fromMaybe (0, 0) . solveMachine)
  where
    cost (a, b) = 3 * a + b

solveMachine :: Machine -> Maybe (Integer, Integer)
solveMachine (Machine (ax, ay) (bx, by) (px, py)) =
  if denom == 0
    then do
      (rx, 0) <- Just $ px `quotRem` bx
      (ry, 0) <- Just $ py `quotRem` by
      guard (rx == ry)
      return (0, rx)
    else do
      (a, 0) <- Just $ anom `quotRem` denom
      (b, 0) <- Just $ bnom `quotRem` denom
      return (a, b)
  where
    denom = abs $ ax * by - ay * bx
    anom = abs $ px * by - py * bx
    bnom = abs $ px * ay - py * ax

convertMachine :: Machine -> Machine
convertMachine (Machine (ax, ay) (bx, by) (px, py)) =
  Machine (ax, ay) (bx, by) (10000000000000 + px, 10000000000000 + py)

part1 :: IO ()
part1 = getInput >>= print . solve

part2 :: IO ()
part2 = getInput >>= print . solve . map convertMachine
