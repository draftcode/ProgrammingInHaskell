last :: [a] -> a
last [x] = x
last (_:xs) = Main.last xs

main = print $ Main.last [1]
