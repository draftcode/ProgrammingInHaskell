and :: [Bool] -> Bool
and [] = True
and (True:xs) = Main.and xs
and _ = False

main = print $ Main.and [True]
