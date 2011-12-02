elem :: Eq a => a -> [a] -> Bool
elem _ [] = False
elem t (x:xs) | t == x    = True
              | otherwise = Main.elem t xs

main = print $ Main.elem 5 [1,2,3,4,6,7]
