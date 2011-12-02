data Nat = Zero | Succ Nat

add :: Nat -> Nat -> Nat
add Zero m = m
add (Succ n) m =  Succ (add n m)

mult :: Nat -> Nat -> Nat
mult Zero m = Zero
mult (Succ n) m = add m (mult n m)

nat2int :: Nat -> Int
nat2int Zero = 0
nat2int (Succ n) = 1+(nat2int n)

int2nat :: Int -> Nat
int2nat 0 = Zero
int2nat n = Succ (int2nat (n-1))

main = print $ nat2int (mult (int2nat 3) (int2nat 4))
