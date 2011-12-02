module Expr where
import Parser

p :: Parser [Int]
p = do symbol "["
       n <- natural
       ns <- many (do symbol ","
                      natural)
       symbol "]"
       return (n:ns)

expr :: Parser Int
expr = do t <- term
          (do l <- many ((do symbol "+"
                             t <- term
                             return t) +++
                           (do symbol "-"
                               t <- term
                               return (-t)))
              return (foldl (+) t l)) +++ return t

term :: Parser Int
term = do f <- pow_or_factor
          (do symbol "*"
              t <- term
              return (f * t)) +++
            (do symbol "/"
                t <- term
                return (f `div` t)) +++ return f

pow_or_factor :: Parser Int
pow_or_factor = do f <- factor
                   (do symbol "^"
                       p <- pow_or_factor
                       return (f ^ p)) +++ return f

factor :: Parser Int
factor = do symbol "("
            e <- expr
            symbol ")"
            return e
         +++ natural

eval :: String -> Int
eval xs = case parse expr xs of
            [(n, [])] -> n
            [(_, out)] -> error ("unused input " ++ out)
            [] -> error "invalid input"

-- main = print $ eval "1-2-3+4"

