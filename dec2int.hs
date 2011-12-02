dec2int :: [Int] -> Int
dec2int = foldl (\r x -> 10*r + x) 0

main = print $ dec2int [2,3,4,5]
