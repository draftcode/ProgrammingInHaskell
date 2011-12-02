module Choices where

subs :: [a] -> [[a]]
subs [] = [[]]
subs (x:xs) = yss ++ map (x:) yss
              where yss = subs xs

interleave :: a -> [a] -> [[a]]
interleave x [] = [[x]]
interleave x ys@(y:yr) = (x:ys):map (y:) (interleave x yr)

perms :: [a] -> [[a]]
perms [] = [[]]
perms (x:xs) = concat (map (interleave x) (perms xs))

choices :: [a] -> [[a]]
-- choices xs = concat (map perms (subs xs))
choices xs = [x | xxs <- subs xs, x <- perms xxs]

-- main = print $ choices [1,2,3]
