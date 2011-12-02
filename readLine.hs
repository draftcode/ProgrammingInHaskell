getNextChar :: String -> IO String
getNextChar s = do x <- getChar
                   case x of
                     '\n' -> return s
                     '\DEL' -> do putStr "\ESC[1D"
                                  getNextChar (init s)
                     _ -> getNextChar (s ++ [x])

readLine :: IO String
readLine = getNextChar []

main = do s <- readLine
          putStr s
