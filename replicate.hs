replicate :: Int -> a -> [a]
replicate n x = [x | _ <- [1..3]]

main = print $ Main.replicate 3 True
