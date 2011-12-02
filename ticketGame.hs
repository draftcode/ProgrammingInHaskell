import IsChoice
import Choices
import List

data Op = Add | Sub | Mul | Div
-- data Op = Add | Sub | Mul | Div | Exp

valid :: Op -> Int -> Int -> Bool
-- valid Add _ _ = True
valid Add x y = x /= 0 && x <= y
valid Sub x y = y /= 0 && x > y
-- valid Sub x y = True
-- valid Mul _ _ = True
valid Mul x y = x /= 1 && y /= 1 && x <= y
valid Div x y = x /= 0 && y /= 0 && y /= 1 && x `mod` y == 0
-- valid Exp x y = x >= 0 && y >= 0

apply :: Op -> Int -> Int -> Int
apply Add x y = x + y
apply Sub x y = x - y
apply Mul x y = x * y
apply Div x y = x `div` y
-- apply Exp x y = x ^ y

data Expr = Val Int | App Op Expr Expr

instance Show Expr where
  showsPrec _ (Val n) = showString (show n)
  showsPrec _ (App Add x y) = showString ("(" ++ show x ++ "+" ++ show y ++ ")")
  showsPrec _ (App Sub x y) = showString ("(" ++ show x ++ "-" ++ show y ++ ")")
  showsPrec _ (App Mul x y) = showString ("(" ++ show x ++ "*" ++ show y ++ ")")
  showsPrec _ (App Div x y) = showString ("(" ++ show x ++ "/" ++ show y ++ ")")

values :: Expr -> [Int]
values (Val n) = [n]
values (App _ l r) = values l ++ values r

instance Eq Expr where
  (==) x y = (==) (values x) (values y)

instance Ord Expr where
  compare x y = compare (values x) (values y)

eval :: Expr -> Maybe Int
eval (Val n) | n > 0     = return n
             | otherwise = fail "Negative integer"
eval (App o l r) = do x <- eval l
                      y <- eval r
                      if valid o x y
                        then return (apply o x y)
                        else fail "Invalid application"

solution :: Expr -> [Int] -> Int -> Bool
solution e ns n = isChoice (values e) ns && eval e == Just n

e :: Expr
e = App Mul (App Add (Val 1) (Val 50)) (App Sub (Val 25) (Val 10))

split :: [a] -> [([a],[a])]
split [] = []
split [_] = []
split (x:xs) = ([x],xs):[(x:ls,rs) | (ls,rs) <- split xs]

exprs :: [Int] -> [Expr]
exprs [] = []
exprs [n] = [Val n]
exprs ns = [e | (ls, rs) <- split ns,
                l <- exprs ls,
                r <- exprs rs,
                e <- combine l r]

combine :: Expr -> Expr -> [Expr]
combine l r = [App o l r | o <- ops]

ops :: [Op]
-- ops = [Add, Sub, Mul, Div, Exp]
ops = [Add, Sub, Mul, Div]

solutions :: [Int] -> Int -> [Expr]
solutions ns n = [e | ns' <- choices ns,
                      e <- exprs ns',
                      eval e == Just n]

type Result = (Expr, Int)
results :: [Int] -> [Result]
results [] = []
results [n] = [(Val n, n) | n > 0]
results ns = [res | (ls,rs) <- split ns,
                    lx <- results ls,
                    ry <- results rs,
                    res <- combine' lx ry]

combine' :: Result -> Result -> [Result]
combine' (l,x) (r,y) = [(App o l r, apply o x y) | o <- ops, valid o x y]

solutions' :: [Int] -> Int -> [Expr]
solutions' ns n = [e | ns' <- choices ns,
                       (e, m) <- results ns',
                       m == n]

solutions'' :: [Int] -> Int -> [Expr]
solutions'' ns n = l
  where (x,l) = foldl (\r x-> foldl selectMax r (results x)) (0,[]) (choices ns)
                  where selectMax :: (Int,[Expr]) -> Result -> (Int,[Expr])
                        selectMax (_,[]) (e,x) = (abs (n-x),[e])
                        selectMax l@(m,r) (e,x) | m < abs (n-x) = l
                                                | m == abs (n-x) = (m,e:r)
                                                | otherwise = (abs (n-x), [e])

-- main = print $ length [e | ns' <- choices ns, e <- exprs ns',
                           -- eval e /= Nothing]
  -- where ns = [1,3,7,10,25,50]

main = print $ sort $ solutions'' [1,3,7,10,25,50] 831
