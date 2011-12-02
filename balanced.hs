data Tree = Leaf Int | Node Tree Tree

leafs :: Tree -> Int
leafs (Leaf _) = 1
leafs (Node l r) = leafs l + leafs r

balanced :: Tree -> Bool
balanced (Leaf _) = True
balanced (Node l r) = leafs l == leafs r

t :: Tree
t = Node (Node (Leaf 1) (Leaf 4)) (Node (Leaf 2) (Leaf 3))

main = print $ balanced t

