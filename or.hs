(||) :: Bool -> Bool -> Bool
True  || True  = True
True  || False = True
False || True  = True
False || False = False

-- True  || _     = True
-- False || True  = True
-- False || False = False

-- True  || _ = True
-- False || b = b

-- b || c | b == c    = b
       -- | otherwise = True

main = print $ (False Main.|| True)
