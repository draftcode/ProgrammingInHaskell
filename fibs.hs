fibs :: [Integer]
fibs = 0:1:(map (\(x,y)->x+y) (zip fibs (tail fibs)))

fib :: Int -> Integer
fib n = fibs !! n

-- main = print $ fib 9

main = print $ head (dropWhile (\x-> x < 1000) fibs)
