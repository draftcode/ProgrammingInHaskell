safetail :: [a] -> [a]
safetail xs = if null xs then xs else tail xs

-- safetail xs | null xs   = []
            -- | otherwise = tail xs

-- safetail [] = []
-- safetail (_:xs) = xs


main = print $ 1:(safetail [])
