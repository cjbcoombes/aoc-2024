{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}

module Grid where

import Data.Array.ST
import qualified Data.Array.Unboxed as U

type Pos = (Int, Int)

type Dir = (Int, Int)

data Grid a = Grid {arr :: U.UArray Pos a, bounds :: Pos}

instance Show (Grid Char) where
  show g = unlines (map (map (gridAt g)) (gridRange2D g))

gridRange :: Grid a -> [(Int, Int)]
gridRange (Grid _ (w, h)) = range ((0, 0), (w - 1, h - 1))

gridRange2D :: Grid a -> [[(Int, Int)]]
gridRange2D (Grid _ (w, h)) =
  [[(x, y) | x <- [0 .. w - 1]] | y <- [0 .. h - 1]]

gridAt :: (U.IArray U.UArray a) => Grid a -> Pos -> a
gridAt g (x, y) = arr g U.! (y, x)

gridAtMaybe :: (U.IArray U.UArray a) => Grid a -> Pos -> Maybe a
gridAtMaybe g (x, y) = arr g U.!? (y, x)

gridFromList :: (U.IArray U.UArray a) => [[a]] -> Grid a
gridFromList [] = error "Empty grid"
gridFromList ls = Grid (U.listArray ((0, 0), (h - 1, w - 1)) (concat ls)) (w, h)
  where
    (h, w) = (length ls, length (head ls))

fl :: (b, a) -> (a, b)
fl (x, y) = (y, x)

up :: Dir
up = (0, -1)

down :: Dir
down = (0, 1)

left :: Dir
left = (-1, 0)

right :: Dir
right = (1, 0)

scale :: Dir -> Int -> Dir
scale (dx, dy) k = (k * dx, k * dy)

step :: Pos -> Dir -> Pos
step (x, y) (dx, dy) = (x + dx, y + dy)
