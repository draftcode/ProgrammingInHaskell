module IsChoice where

remove :: Eq a => a -> [a] -> Maybe [a]
remove _ [] = fail "Not found"
remove x (y:ys) | x == y = return ys
                | otherwise = do l <- remove x ys
                                 return (y:l)

removeAll :: Eq a => [a] -> [a] -> Maybe [a]
removeAll [] ys = return ys
removeAll (x:xs) ys = do l <- remove x ys
                         removeAll xs l

isChoice :: Eq a => [a] -> [a] -> Bool
isChoice xs ys = case removeAll xs ys of
                   Nothing -> False
                   Just _ -> True

-- main = print $ isChoice [1,2,3] [1,2,4,5,6]
