init :: [a] -> [a]
-- init xs = take (length xs - 1) xs

-- init [x] = []
-- init (x:xs) = x:(Main.init xs)

main = print $ Main.init [1,2,3,4]
