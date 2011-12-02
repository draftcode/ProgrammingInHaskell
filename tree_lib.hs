data Tree a = Leaf | Node (Tree a) a (Tree a)

repeat :: a -> Tree a
repeat x = t where t = Node t x t

take :: Int -> Tree a -> Tree a
take 0 _ = Leaf
take _ Leaf = Leaf
take n (Node l x r) = Node (Main.take (n-1) l) x (Main.take (n-1) r)

replicate :: Int -> a -> Tree a
replicate n = Main.take n . Main.repeat

