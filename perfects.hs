factors :: Int -> [Int]
factors n = [x | x <- [1..n], n `mod` x == 0]

perfects :: Int -> [Int]
perfects n = [x| x <- [1..n], sum (factors x) == 2*x]

main = print $ perfects 500
