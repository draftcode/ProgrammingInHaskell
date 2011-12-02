map_f :: (a -> b) -> [a] -> [b]
map_f f = foldr (\x r-> f x:r) []

main = print $ map_f (*2) [1,2,3]
