import Parser

int :: Parser Int
int = do (char '-')
         l <- many1 digit
         return (-1 * (read l))
      +++
      do l <- many1 digit
         return (read l)

main = print $ parse int "123"
