(&&) :: Bool -> Bool -> Bool
-- True && True = True
-- _    && _    = False

-- x && y = if x == True then if y == True then True else False else False

-- True  && b = b
-- False && _ = False

x && y = if x == True then y else False

main = print $ True Main.&& True
