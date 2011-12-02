import Parser
import Tautology


prop :: Parser Prop
prop = do t <- term
          (do symbol "||"
              p <- prop
              return (Or t p)) +++ return t

term :: Parser Prop
term = do f <- factor
          (do symbol "&&"
              t <- term
              return (And f t)) +++ return f

factor :: Parser Prop
factor = do (do symbol "("
                p <- prop
                symbol ")"
                return p) +++
             (do symbol "not"
                 f <- factor
                 return (Not f)) +++
             (do symbol "True"
                 return (Const True)) +++
             (do symbol "False"
                 return (Const False)) +++
             (do tok <- token alphanum
                 return (Var tok))

main = do s <- getLine
          case parse prop s of
            [(tree,"")] -> print (isTaut tree)
            [(_,s)] -> print ("Error : " ++ s)
            [] -> print "Error"

