all :: [Bool] -> Bool
all [True] = True
all (True:xs) = Main.all xs
all _ = False

main = print $ Main.all [True, True]
