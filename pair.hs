newtype Pair b a = Pair {getPair :: (a, b)}
instance Functor (Pair c) where
    fmap f (Pair (x, y)) = Pair (f x, y)


newtype CoolBool = CoolBool { getCoolBool :: Bool}
hello :: CoolBool -> String
hello (CoolBool _) = "hello"