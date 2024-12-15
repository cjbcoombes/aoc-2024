{-# HLINT ignore "Use bimap" #-}
{-# HLINT ignore "Redundant bracket" #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}

module Day14 where

import Control.Monad (forM_)
import Data.Maybe (isJust)
import qualified Data.Set as S
import Data.Void (Void)
import Text.Megaparsec (Parsec, eof, many, optional, parseMaybe, some)
import Text.Megaparsec.Char (char, digitChar, eol, string)
import Utils

type Parser = Parsec Void String

data Robot = Robot (Int, Int) (Int, Int) deriving (Show, Eq)

zone :: (Int, Int)
zone = (101, 103)

getInput :: IO [Robot]
getInput = concat . parseMaybe (many robot <* eof) <$> readFile "app/day14/input.txt"
  where
    num :: Parser Int
    num = do
      c <- optional (char '-')
      n <- read <$> some digitChar
      return $ if isJust c then (-n) else n
    robot :: Parser Robot
    robot = do
      pos <- (,) <$> (string "p=" *> num) <*> (char ',' *> num)
      char ' '
      vel <- (,) <$> (string "v=" *> num) <*> (char ',' *> num)
      eol
      return $ Robot pos vel

simRobot :: Int -> Robot -> (Int, Int)
simRobot n (Robot (x, y) (vx, vy)) =
  ((x + vx * n) `mod` fst zone, (y + vy * n) `mod` snd zone)

solve1 :: [Robot] -> Int
solve1 rs =
  product
    [ count ((\(x, y) -> (fx x mx) && (fy y my)) . simRobot 100) rs
      | fx <- [(<), (>)],
        fy <- [(<), (>)]
    ]
  where
    mx = fst zone `quot` 2
    my = snd zone `quot` 2

drawStep :: Int -> [Robot] -> Maybe String
drawStep n rs = if check set then Just str else Nothing
  where
    set = S.fromList (map (simRobot n) rs)
    str =
      concatMap
        ( \y ->
            '\n'
              : map
                ( \x ->
                    if S.member (x, y) set then '#' else '.'
                )
                [0 .. (fst zone - 1)]
        )
        [0 .. (snd zone - 1)]

-- filter for tree-like things by looking for 15 filled cells in a row
check :: S.Set (Int, Int) -> Bool
check s = any ((> 15) . streak) s
  where
    streak (x, y) = length $ takeWhile (`S.member` s) (map (,y) [x ..])

-- inspect for first tree occurrence
solve2 :: [Robot] -> IO ()
solve2 rs =
  forM_
    [1 .. 20000]
    ( \n -> case drawStep n rs of
        Just str -> print n >> putStrLn str >> putStrLn ""
        Nothing -> return ()
    )

part1 :: IO ()
part1 = getInput >>= print . solve1

part2 :: IO ()
part2 = getInput >>= solve2
