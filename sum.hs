sum :: Num a => [a] -> a
sum [x] = x
sum (x:xs) = x + (Main.sum xs)

main = print $ Main.sum [2,3,4]
