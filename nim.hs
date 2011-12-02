import Screen

type Board = [Int]

initialBoard :: Board
initialBoard = [5,4,3,2,1]

numOfPlayers :: Int
numOfPlayers = 2

makeMove :: Board -> Int -> Int -> Board
makeMove (b:bs) 1 num = (maximum [0, b-num]):bs
makeMove (b:bs) pos num = b:(makeMove bs (pos-1) num)

showBoard :: Board -> IO ()
showBoard [] = return ()
showBoard (b:bs) = do putStr (show (5 - length bs) ++ " : " ++ take b (repeat '*') ++ "\n")
                      showBoard bs

selectRow :: Int -> Board -> IO Int
selectRow player b =
  do putStr ("Player " ++ show player ++  ": Select a row -> ")
     s <- getLine
     let row = read s in
       if 1 <= row && row <= 5 && (b !! (row-1)) /= 0
         then return row
         else selectRow player b

selectNum :: Int -> Int -> Board -> IO Int
selectNum player row b =
  do putStr ("Player " ++ show player ++  ": Select num -> ")
     s <- getLine
     let num = read s in
       if (b !! (row-1)) >= num
         then return num
         else selectNum player row b

interact :: Board -> Int -> IO ()
interact b player = do showBoard b
                       row <- selectRow player b
                       num <- selectNum player row b
                       let nextBoard = makeMove b row num in
                         if all (==0) nextBoard
                           then putStr ("Player " ++ show player ++ " win!\n")
                           else Main.interact nextBoard ((player+1) `mod` numOfPlayers)

main = Main.interact initialBoard 0
