unfold :: (a -> Bool) -> (a -> b) -> (a -> a) -> a -> [b]
unfold p h t x | p x       = []
               | otherwise = h x : unfold p h t (t x)

iterate :: (a -> a) -> a -> [a]
iterate f = unfold (\x -> False) id f

main = print $ take 8 (Main.iterate (*2) 1)
