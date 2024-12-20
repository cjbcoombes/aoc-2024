{-# OPTIONS_GHC -Wno-unused-do-bind #-}

module Day17 where

import Control.Monad (forM_)
import Data.Array.Unboxed
import Data.Bits (shiftR, xor)
import Data.List (intercalate, unfoldr)
import Data.Maybe (fromJust)
import Data.Void (Void)
import Text.Megaparsec (Parsec, eof, many, optional, parseMaybe, some)
import Text.Megaparsec.Char (char, digitChar, eol, string)

type Parser = Parsec Void String

-- A, B, C, Code, PPtr
data Config = Config Int Int Int (UArray Int Int) Int [Int]

instance Show Config where
  show (Config a b c code pos out) =
    "A:"
      ++ show a
      ++ " B:"
      ++ show b
      ++ " C:"
      ++ show c
      ++ " "
      ++ concatMap showPos (range (bounds code))
      ++ " "
      ++ intercalate "," (map show (reverse out))
    where
      showPos p = (if p == pos then ">" else "") ++ show (code ! p)

getInput :: IO Config
getInput = fromJust . parseMaybe p <$> readFile "app/day17/input.txt"
  where
    num = read <$> some digitChar :: Parser Int
    p = do
      a <- string "Register A: " *> num <* eol
      b <- string "Register B: " *> num <* eol
      c <- string "Register C: " *> num <* eol
      eol
      string "Program: "
      code <- many (num <* optional (char ','))
      eol
      eof
      return $ Config a b c (listArray (0, length code - 1) code) 0 []

step :: Config -> Maybe Config
step (Config a b c code pos out) = do
  opcode <- code !? pos
  arg <- code !? (pos + 1)
  return (process opcode arg)
  where
    combo arg = case arg of
      0 -> 0
      1 -> 1
      2 -> 2
      3 -> 3
      4 -> a
      5 -> b
      6 -> c
      x -> error $ "invalid combo arg " ++ show x
    trunc = (`mod` 8)
    process opcode arg =
      case opcode of
        0 -> Config (a `shiftR` combo arg) b c code (pos + 2) out
        1 -> Config a (b `xor` arg) c code (pos + 2) out
        2 -> Config a (trunc (combo arg)) c code (pos + 2) out
        3 ->
          if a == 0
            then Config a b c code (pos + 2) out
            else Config a b c code arg out
        4 -> Config a (b `xor` c) c code (pos + 2) out
        5 -> Config a b c code (pos + 2) (trunc (combo arg) : out)
        6 -> Config a (a `shiftR` combo arg) c code (pos + 2) out
        7 -> Config a b (a `shiftR` combo arg) code (pos + 2) out
        x -> error $ "invalid opcode " ++ show x

{-
2 4 write A mod 8 to B
1 3 B xor 3
7 5 A >> B into C
1 5 B xor 5
0 3 A >> 3
4 1 B xor C into B
5 5 output B mod 8
3 0 jump to start

2 4 write A mod 8 to B
1 3 B xor 3
7 5 A >> B into C
4 1 B xor C into B
1 5 B xor 5
0 3 A >> 3
5 5 output B mod 8
3 0 jump to start

B = x
B = B xor A
A << 3
B = B xor 5
A << B
B = B xor 3
A += B mod 8
-}

solve2 :: [Int] -> [Int]
solve2 [] = [0]
solve2 (x : xs) = concatMap valid (solve2 xs)
  where
    comp n =
      let n3 = (n `mod` 8) `xor` 3
       in (n3 `xor` 5 `xor` shiftR n n3) `mod` 8
    valid prev = filter ((== x) . comp) $ map (+ (prev * 8)) [0 .. 7]

part1 :: IO ()
part1 = do
  c <- getInput
  let steps = unfoldr (fmap (\x -> (x, x)) . step) c
  forM_ steps print

part2 :: IO ()
part2 = do
  (Config _ _ _ code _ _) <- getInput
  let arr = elems code
      a' = solve2 arr
  print arr
  print (minimum a')
