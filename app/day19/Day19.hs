module Day19 where

import Control.Monad
import Control.Monad.State
import qualified Data.ByteString as B
import Data.Char (ord)
import Data.List.Split (splitOn)
import qualified Data.Trie as T

getInput :: IO (T.Trie Int, [B.ByteString])
getInput = do
  ts : _ : pats <- lines <$> readFile "app/day19/input.txt"
  let towels = map ((\x -> (x, B.length x)) . makePattern) $ splitOn ", " ts
      patterns = map makePattern pats
      makePattern = B.pack . map (fromIntegral . ord)
  return (T.fromList towels, patterns)

valid :: T.Trie Int -> B.ByteString -> State (T.Trie Int) Int
valid parts tgt = do
  known <- get
  res <-
    case T.lookup tgt known of
      Just a -> return a
      Nothing -> foldM (\a e -> (+ a) <$> valid parts e) 0 rems
  modify (T.insert tgt res)
  return res
  where
    rems = [x | (p, _, x) <- T.matches parts tgt, not (B.null p)]

baseTrie :: T.Trie Int
baseTrie = T.singleton B.empty 1

part1 :: IO ()
part1 = do
  (parts, pats) <- getInput
  print $ length (filter ((/= 0) . (`evalState` baseTrie) . valid parts) pats)

part2 :: IO ()
part2 = do
  (parts, pats) <- getInput
  print $ sum (map ((`evalState` baseTrie) . valid parts) pats)
