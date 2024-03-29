data Tree = Leaf Int | Node Tree Int Tree

t :: Tree
t = Node (Node (Leaf 1) 3 (Leaf 4)) 5 (Node (Leaf 6) 7 (Leaf 9))

occurs :: Int -> Tree -> Bool
occurs m (Leaf n) = m == n
occurs m (Node l n r) =
  case compare m n of
    EQ -> True
    LT -> occurs m l
    GT -> occurs m r

main = print $ occurs 0 t
