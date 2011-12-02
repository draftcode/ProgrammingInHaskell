replicate :: Int -> a -> [a]
replicate 0 _ = []
replicate n x = x:(Main.replicate (n-1) x)

main = print $ Main.replicate 3 3
