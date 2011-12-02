module Tautology where

data Prop = Const Bool
          | Var Char
          | Not Prop
          | And Prop Prop
          | Or Prop Prop
          | Imply Prop Prop
          | Equal Prop Prop

instance Show Prop where
  showsPrec _ (Const b) = showString (show b)
  showsPrec _ (Var c) = showString (show c)
  showsPrec _ (Not p) = showString ("(not " ++ show p ++ ")")
  showsPrec _ (And p q) = showString (show p ++ " && " ++ show q)
  showsPrec _ (Or p q) = showString ("(" ++ show p ++ " || " ++ show q ++ ")")
  showsPrec _ (Imply p q) = showString ("(" ++ show p ++ " => " ++ show q ++ ")")
  showsPrec _ (Equal p q) = showString ("(" ++ show p ++ " <=> " ++ show q ++ ")")

type Assoc a b = [(a,b)]
type Subst = Assoc Char Bool

find :: Eq a => a -> Assoc a b -> b
find x ((k,v):xs) | k == x = v
                  | otherwise = find x xs

eval :: Subst -> Prop -> Bool
eval _ (Const b) = b
eval s (Var x) = find x s
eval s (Not p) = not (eval s p)
eval s (And p q) = eval s p && eval s q
eval s (Or p q) = eval s p || eval s q
eval s (Imply p q) = eval s p <= eval s q
eval s (Equal p q) = eval s p == eval s q

vars :: Prop -> [Char]
vars (Const b) = []
vars (Var x) = [x]
vars (Not p) = vars p
vars (And p q) = vars p ++ vars q
vars (Or p q) = vars p ++ vars q
vars (Imply p q) = vars p ++ vars q
vars (Equal p q) = vars p ++ vars q

rmdups :: Eq a => [a] -> [a]
rmdups [] = []
rmdups (x:xs) = x:rmdups (filter (/= x) xs)

bools :: Int -> [[Bool]]
bools 0 = [[]]
bools n = map (False:) bss ++ map (True:) bss
  where bss = bools (n-1)

substs :: Prop -> [Subst]
substs p = map (zip vs) (bools (length vs))
  where vs = rmdups (vars p)

p1 :: Prop
p1 = And (Var 'A') (Not (Var 'A'))
p2 :: Prop
p2 = Imply (And (Var 'A') (Var 'B')) (Var 'A')
p3 :: Prop
p3 = Imply (Var 'A') (And (Var 'A') (Var 'B'))
p4 :: Prop
p4 = Imply (And (Var 'A') (Imply (Var 'A') (Var 'B'))) (Var 'B')
p5 :: Prop
p5 = Or (Var 'A') (Not (Var 'A'))
p6 :: Prop
p6 = Equal (Or (Var 'A') (Not (Var 'A'))) (Or (Var 'B') (Not (Var 'B')))

isTaut :: Prop -> Bool
isTaut p = and [eval s p | s <- substs p]

-- main = print $ isTaut p6
