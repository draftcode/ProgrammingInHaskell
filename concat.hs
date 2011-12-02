concat :: [[a]] -> [a]
concat [] = []
concat ([]:xs) = Main.concat xs
concat ((x:xs):xr) = x:(Main.concat (xs:xr))

main = print $ Main.concat [[1],[2,3],[4,5,6]]
