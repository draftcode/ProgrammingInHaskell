import Parser

comment :: Parser ()
comment = do string "-- "
             many (sat (/='\n'))
             char '\n'
             return ()

main = print $ parse comment "-- abcdef\n\n"
