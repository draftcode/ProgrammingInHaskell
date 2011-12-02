import LifeGame
import Screen
import System.IO

moveDown :: Board -> Pos -> IO ()
moveDown b (x,y) = waitingLoop b b (wrap (x,y+1))

moveUp :: Board -> Pos -> IO ()
moveUp b (x,y) = waitingLoop b b (wrap (x,y-1))

moveLeft :: Board -> Pos -> IO ()
moveLeft b (x,y) = waitingLoop b b (wrap (x-1,y))

moveRight :: Board -> Pos -> IO ()
moveRight b (x,y) = waitingLoop b b (wrap (x+1,y))

toggle :: Board -> Pos -> IO ()
toggle b p = waitingLoop b (if elem p b then filter (==p) b else p:b) p

process :: Char -> Board -> Pos -> IO ()
process c b p
  | c == 'j' = moveDown b p
  | c == 'k' = moveUp b p
  | c == 'h' = moveLeft b p
  | c == 'l' = moveRight b p
  | c == ' ' = toggle b p
  | c == 'r' = beginLife b

waitingLoop :: Board -> Board -> Pos -> IO ()
waitingLoop prevb b p@(x,y) = do showcellsDiff b prevb
                                 goto (x+1,y+1)
                                 c <- getChar
                                 process c b p

main = do cls
          hSetEcho stdin False
          showFrame 1 1
          waitingLoop [] [] (1,1)
