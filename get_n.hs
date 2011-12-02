(!!) :: [a] -> Int -> a
(!!) (x:_) 1 = x
(!!) (_:xs) n = xs Main.!! (n-1)

main = print $ [True, True, False] Main.!! 3
