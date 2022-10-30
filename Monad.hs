import Control.Monad

applyMaybe :: Maybe a -> (a -> Maybe b) -> Maybe b
applyMaybe Nothing _ = Nothing
applyMaybe (Just x) f = f x

foo :: Maybe String
foo = Just 3 >>= (\x -> Just "!" >>= (\y -> Just (show x ++ y)))
bar :: Maybe String
bar = do
    x <- Just 3
    y <- Just "!"
    Just (show x ++ y)

listOfTuples :: [(Int, Char)]
listOfTuples = do
    n <- [1,2]
    ch <- ['a', 'b']
    return (n, ch)

sevensOnly :: [Int]
sevensOnly = do
    n <- [1..50]
    guard ('7' `elem` show n)
    return n