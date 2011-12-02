-- main = print $ [(x,y) | x <- [1,2,3], y <- [4,5,6]]
main = print $ concat [[(x,y) | y <- [4,5,6]] | x <- [1,2,3]]
