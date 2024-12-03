module Utils where

count :: (Foldable a) => (b -> Bool) -> a b -> Int
count p = foldr (\e a -> if p e then a + 1 else a) 0

allSat :: [a -> Bool] -> a -> Bool
allSat ps x = all ($ x) ps

anySat :: [a -> Bool] -> a -> Bool
anySat ps x = any ($ x) ps
