any :: [Bool] -> Bool
any [] = False
any (True:xs) = True
any (False:xs) = Main.any xs

main = print $ Main.any [False, True, False, False]
