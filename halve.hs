halve :: [a] -> ([a], [a])
halve xs = (take (l `div` 2) xs, drop (l `div` 2) xs)
  where
    l = length xs

main = print $ halve [1,2,3,4,5,6]
