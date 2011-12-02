takeWhile :: (a -> Bool) -> [a] -> [a]
takeWhile _ [] = []
takeWhile p (x:xs) | p x = x:(Main.takeWhile p xs)
                   | otherwise = []

main = print $ Main.takeWhile even [2, 4]
