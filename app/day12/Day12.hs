module Day12 where

import Control.Monad.State.Lazy
import Data.Array.Unboxed
import qualified Data.Set as S
import Utils

data Grid = Grid (UArray (Int, Int) Char) (Int, Int) deriving (Show)

getInput :: IO Grid
getInput = do
  contents <- lines <$> readFile "app/day12/input.txt"
  let bnds = (length contents, length (head contents))
      arr = listArray ((1, 1), bnds) (concat contents)
  return $ Grid arr bnds

search1 :: Grid -> (Int, Int) -> Char -> State (S.Set (Int, Int)) (Int, Int)
search1 g@(Grid arr _) (y, x) c =
  do
    seen <- get
    if S.member (y, x) seen || not (isc (y, x))
      then return (0, 0)
      else go
  where
    isc = (== Just c) . (arr !?)
    go = do
      modify (S.insert (y, x))
      (a1, p1) <- search1 g (y + 1, x) c
      (a2, p2) <- search1 g (y - 1, x) c
      (a3, p3) <- search1 g (y, x + 1) c
      (a4, p4) <- search1 g (y, x - 1) c
      let p = 4 - count isc [(y + 1, x), (y - 1, x), (y, x + 1), (y, x - 1)]
      return (a1 + a2 + a3 + a4 + 1, p1 + p2 + p3 + p4 + p)

search2 :: Grid -> (Int, Int) -> Char -> State (S.Set (Int, Int)) (Int, Int)
search2 g@(Grid arr _) (y, x) c =
  do
    seen <- get
    if S.member (y, x) seen || not (isc (y, x))
      then return (0, 0)
      else go
  where
    isc = (== Just c) . (arr !?)
    masks = [[False, False, False], [True, True, False], [False, True, False]]
    go = do
      modify (S.insert (y, x))
      (a1, p1) <- search2 g (y + 1, x) c
      (a2, p2) <- search2 g (y - 1, x) c
      (a3, p3) <- search2 g (y, x + 1) c
      (a4, p4) <- search2 g (y, x - 1) c
      let p =
            count
              ((`elem` masks) . map isc)
              [ [(y - 1, x), (y - 1, x + 1), (y, x + 1)],
                [(y - 1, x), (y - 1, x - 1), (y, x - 1)],
                [(y, x - 1), (y - 1, x - 1), (y - 1, x)],
                [(y, x - 1), (y + 1, x - 1), (y + 1, x)]
              ]
      return (a1 + a2 + a3 + a4 + 1, p1 + p2 + p3 + p4 + p)

solve :: (Grid -> (Int, Int) -> Char -> State (S.Set (Int, Int)) (Int, Int)) -> Grid -> Int
solve search g@(Grid arr bnds) = sum . (`evalState` S.empty) $ mapM go (range ((1, 1), bnds))
  where
    go (y, x) = do
      (a, p) <- search g (y, x) (arr ! (y, x))
      return (a * p)

part1 :: IO ()
part1 = getInput >>= print . solve search1

part2 :: IO ()
part2 = getInput >>= print . solve search2
