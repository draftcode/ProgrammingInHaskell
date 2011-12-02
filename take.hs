take :: Int -> [a] -> [a]
take 0 _ = []
take n (x:xs) = x:(Main.take (n-1) xs)

main = print $ Main.take 3 [1,2,3]
