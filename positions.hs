find :: Eq a => a -> [(a,b)] -> [b]
find k t = [v | (k', v) <- t, k == k']

positions :: Eq a => a -> [a] -> [Int]
-- positions x xs = [i | (x',i) <- zip xs [0..n], x == x']
  -- where n = length xs - 1
positions x xs = find x (zip xs [0..n])
  where n = length xs - 1

main = print $ positions False [True, False, True, False]
