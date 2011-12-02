data Tree = Leaf Int | Node Tree Tree

instance Show Tree where
  showsPrec _ (Leaf n) = showString (show n)
  showsPrec _ (Node l r) = showString ("<" ++ show l ++ "|" ++ show r ++ ">")

balance :: [Int] -> Tree
balance [x] = Leaf x
balance lis = Node (balance (take n lis)) (balance (drop n lis))
  where n = (length lis) `div` 2


main = print $ balance [1, 2, 3, 4, 5, 6, 7, 8]

