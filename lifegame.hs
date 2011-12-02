module LifeGame where
import Screen
import Control.Concurrent

width :: Int
width = 10

height :: Int
height = 10

type Board = [Pos]

showFrame :: Int -> Int -> IO ()
showFrame x y | x == 1 && y == 1 = do writeat (x,y) "+"
                                      showFrame (x+1) y
              | x == (width+2) && y == 1 = do writeat (x,y) "+"
                                              showFrame 1 (y+1)
              | x == (width+2) && y == (height+2) = writeat (x,y) "+"
              | y == 1 || y == (height+2) = do writeat (x,y) "-"
                                               showFrame (x+1) y
              | x == 1 = do writeat (x,y) "|"
                            showFrame (width+2) y
              | x == (width+2) = do writeat (x,y) "|"
                                    showFrame 1 (y+1)

showcells :: Board -> IO ()
showcells b = seqn [writeat p "O" | p <- b]

putcell :: Pos -> IO ()
putcell (x,y) = writeat (x+1,y+1) "O"

erasecell :: Pos -> IO ()
erasecell (x,y) = writeat (x+1,y+1) " "

showcellsDiff :: Board -> Board -> IO ()
showcellsDiff [] prevb = seqn [erasecell p | p <- prevb]
showcellsDiff (p:bs) prevb =
  if elem p prevb
    then showcellsDiff bs (filter (/=p) prevb)
    else do putcell p
            showcellsDiff bs prevb

isAlive :: Board -> Pos -> Bool
isAlive b p = elem p b

isEmpty :: Board -> Pos -> Bool
isEmpty b p = not (isAlive b p)

neighbs :: Pos -> [Pos]
neighbs (x, y) = map wrap [(x-1,y-1),(x,y-1),
                           (x+1,y+1),(x-1,y),
                           (x+1,y),(x-1,y+1),
                           (x,y+1),(x+1,y-1)]

wrap :: Pos -> Pos
wrap (x,y) = (((x-1) `mod` width) + 1,
              ((y-1) `mod` height) + 1)

liveneighbs :: Board -> Pos -> Int
liveneighbs b = length . filter (isAlive b) . neighbs

survivors :: Board -> [Pos]
survivors b = [p | p <- b, elem (liveneighbs b p) [2,3]]

births :: Board -> [Pos]
births b = [p | p <- rmdups (concat (map neighbs b)),
                isEmpty b p,
                liveneighbs b p == 3]

rmdups :: Eq a => [a] -> [a]
rmdups [] = []
rmdups (x:xs) = x:rmdups (filter (/= x) xs)

nextgen :: Board -> Board
nextgen b = survivors b ++ births b


life :: Board -> Board -> IO ()
life prevb b = do showcellsDiff b prevb
                  threadDelay 50000
                  life b (nextgen b)

beginLife :: Board -> IO ()
beginLife b = do cls
                 showFrame 1 1
                 life [] b

glider :: Board
glider = [(4,2),(2,3),(4,3),(3,4),(4,4)]

block :: Board
block = [(2,2),(3,2),(2,3),(3,3)]

-- main = beginLife glider

