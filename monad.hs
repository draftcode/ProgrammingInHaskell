import Prelude hiding (Maybe, Just, Nothing)

data Maybe a = Nothing | Just a

instance Monad Maybe where
  return v = Just v
  (Nothing) >>= _  = Nothing
  (Just v) >>= g = g v
  fail _ = Nothing

instance Monad [] where
  return v     = [v]
  [] >>= g     = []
  (x:xs) >>= g = g x ++ (xs >>= g)
  fail s       = []
