unfold :: (a -> Bool) -> (a -> b) -> (a -> a) -> a -> [b]
unfold p h t x | p x       = []
               | otherwise = h x : unfold p h t (t x)

map_f f = unfold (\l -> length l == 0) (f . head) tail

main = print $ map_f (*2) [1,2,3]

