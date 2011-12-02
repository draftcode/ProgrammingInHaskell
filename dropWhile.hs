dropWhile :: (a -> Bool) -> [a] -> [a]
dropWhile _ [] = []
dropWhile p (x:xs) | p x = Main.dropWhile p xs
                   | otherwise = x:xs

main = print $ Main.dropWhile even [2, 4, 3, 6]
