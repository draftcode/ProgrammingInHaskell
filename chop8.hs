unfold :: (a -> Bool) -> (a -> b) -> (a -> a) -> a -> [b]
unfold p h t x | p x       = []
               | otherwise = h x : unfold p h t (t x)

type Bit = Int

chop8 :: [Bit] -> [[Bit]]
chop8 = unfold (\l -> length l == 0) (take 8) (drop 8)

main = print $ chop8 [1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16]

