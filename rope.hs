import Control.Arrow (Arrow(second))
type Birds = Int
type Pole = (Birds, Birds)

landLeft :: Birds -> Pole -> Maybe Pole
landLeft n (left, right)
    | abs ((left + n) - right) > 3 = Nothing
    | otherwise = Just (left + n, right)
landRight :: Birds -> Pole -> Maybe Pole
landRight n (left, right)
    | abs(left - (right + n)) > 3 = Nothing
    | otherwise = Just (left, right + n)

banana :: Pole -> Maybe Pole
banana _ = Nothing

(-:) :: t1 -> (t1 -> t2) -> t2
x -: f = f x

routine :: Maybe Pole
routine = do
    start <- return (0,0)
    first <- landLeft 3 start
    second <- landRight 1 first
    landLeft 1 second
