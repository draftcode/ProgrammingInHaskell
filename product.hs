product :: Num a => [a] -> a

product [x] = x
product (x:xs) = x * Main.product xs

main = print $ Main.product [2, 3, 4]
