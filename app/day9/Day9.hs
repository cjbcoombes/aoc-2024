{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Day9 where

import Data.List (singleton)
import qualified Data.Map as M
import Data.Maybe (fromMaybe)

data Block = Block Int Int Int deriving (Eq, Show) -- start index, size, file index

getInput :: IO ([Block], [Block])
getInput = do
  contents <- map (read . singleton) . filter (/= '\n') <$> readFile "app/day9/input.txt"
  let (files, spaces) = uninterleave (zip (scanl (+) 0 contents) contents)
      files' = zipWith (\fi (i, n) -> Block i n fi) [0 ..] files
      spaces' = map (\(i, n) -> Block i n 0) spaces
  return (files', spaces')
  where
    uninterleave [] = ([], [])
    uninterleave [x] = ([x], [])
    uninterleave (x : y : zs) = let (xs, ys) = uninterleave zs in (x : xs, y : ys)

blockCost :: Block -> Int
blockCost (Block i n fi) = (i * n + n * (n - 1) `quot` 2) * fi

solve1 :: [Block] -> [Block] -> Int
solve1 [] _ = 0
solve1 (f : fs) [] = blockCost f + solve1 fs []
solve1 (f@(Block fi fn ffi) : fs) ((Block si sn _) : ss)
  | fi < si = solve1 (f : fs) []
  | fn == sn = blockCost (Block si sn ffi) + solve1 fs ss
  | fn < sn = blockCost (Block si fn ffi) + solve1 fs (Block (si + fn) (sn - fn) 0 : ss)
  | otherwise = blockCost (Block si sn ffi) + solve1 (Block fi (fn - sn) ffi : fs) ss

mkMap :: [Block] -> M.Map Int [Block]
mkMap [] = M.empty
mkMap (s@(Block _ n _) : ss) = M.insertWith (++) n [s] (mkMap ss)

ordApp :: [Block] -> [Block] -> [Block]
ordApp [] bs = bs
ordApp (x : xs) bs = ordIns x (ordApp xs bs)
  where
    ordIns x [] = [x]
    ordIns x@(Block xi _ _) (b@(Block bi _ _) : bs)
      | xi <= bi = x : b : bs
      | otherwise = b : ordIns x bs

solve2 :: [Block] -> M.Map Int [Block] -> Int
solve2 [] _ = 0
solve2 (f@(Block i n fi) : fs) m =
  case findSpace of
    Nothing -> blockCost f + solve2 fs m
    Just (b, m') -> blockCost b + solve2 fs m'
  where
    candidates = concatMap (fromMaybe [] . flip M.lookup m) [n .. 9]
    best [] = Nothing
    best (x : xs) = Just $ maybe x (mn x) (best xs)
    mn x@(Block xi _ _) y@(Block yi _ _) = if xi < yi then x else y
    findSpace = best candidates >>= (\(Block _ sn _) -> findSpaceExact sn)
    findSpaceExact k =
      case M.lookup k m of
        Nothing -> Nothing
        Just [] -> Nothing
        Just ((Block si sn _) : ss)
          | i < si -> Nothing
          | otherwise -> Just (Block si n fi, M.insertWith ordApp (sn - n) [Block (si + n) (sn - n) 0] . M.insert sn ss $ m)

part1 :: IO ()
part1 = getInput >>= print . (\(f, s) -> solve1 (reverse f) s)

part2 :: IO ()
part2 = getInput >>= print . (\(f, s) -> solve2 (reverse f) (mkMap s))
