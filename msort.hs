import Merge

halve :: [a] -> ([a],[a])
halve l = (take n l, drop n l)
          where
            n = (length l) `div` 2

msort :: Ord a => [a] -> [a]
msort [] = []
msort [x] = [x]
msort l = merge (msort left) (msort right)
          where
            (left, right) = halve l

main = print $ msort [5,4,3,2,1,0]

