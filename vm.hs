data Expr = Val Int | Add Expr Expr | Mult Expr Expr

type Cont = [Op]
type Stack = [Int]
data Op = EVAL Expr | ADD | MULT

eval :: Expr -> Cont -> Stack -> Int
eval (Val n) c stk = exec c (n:stk)
eval (Add x y) c stk = eval x (EVAL y:ADD:c) stk
eval (Mult x y) c stk = eval x (EVAL y:MULT:c) stk

exec :: Cont -> Stack -> Int
exec [] (n:_) = n
exec (EVAL y:c) stk = eval y c stk
exec (ADD:c) (n:m:stk) = exec c ((n+m):stk)
exec (MULT:c) (n:m:stk) = exec c ((m*n):stk)

value :: Expr -> Int
-- value (Val n) = n
-- value (Add x y) = value x + value y
value e = eval e [] []

main = print $ value (Add (Mult (Val 2) (Val 3)) (Val 4))
