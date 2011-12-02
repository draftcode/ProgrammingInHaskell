import Data.Char

llet2int :: Char -> Int
llet2int c = ord c - ord 'a'

int2llet :: Int -> Char
int2llet n = chr (ord 'a' + n)

ulet2int :: Char -> Int
ulet2int c = ord c - ord 'A'

int2ulet :: Int -> Char
int2ulet n = chr (ord 'A' + n)

shift :: Int -> Char -> Char
shift n c | isLower c = int2llet ((llet2int c+n) `mod` 26)
          | isUpper c = int2ulet ((ulet2int c+n) `mod` 26)
          | otherwise = c

encode :: Int -> String -> String
encode n xs = [shift n x | x <- xs]

main = print $ encode 33 "Never did I dreamed that I would be a singer."
