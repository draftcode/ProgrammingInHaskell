filter_p :: (a -> Bool) -> [a] -> [a]
filter_p p = foldr (\x r-> if p x then x:r else r) []

main = print $ filter_p even [2,3,5,6,8]
