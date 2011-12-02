s :: (c -> a) -> (c -> b) -> (b -> c) -> (b -> a)
s x y z = x . z . (y . z)

t :: (c -> a) -> (a -> b) -> (c -> b)
t x y = y . x

k :: a -> b -> a
k x y = x

last :: [a] -> a
last = head . reverse

-- last = t reverse head

-- last xs = xs !! (length xs - 1)

-- last [x] = x
-- last (x:xs) = Main.last xs

-- last xs = head (drop (length xs - 1) xs)

main = print $ Main.last ['x', 'y', 'z']
