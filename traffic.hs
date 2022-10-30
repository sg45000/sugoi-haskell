module Traffic (
    TrafficLight(Green, Yellow, Red),
) where

-- class Eq a where
--     (==) :: a -> a -> Bool
--     (/=) :: a -> a -> Bool
--     x == y = not (x /= y)
--     x /= y = not (x == y)

data TrafficLight = Green | Yellow | Red

instance Eq TrafficLight where
    Red == Red = True
    Green == Green = True
    Yellow == Yellow = True
    _ == _ = False


instance Show TrafficLight where
    show Red = "Error!"
    show Yellow = "Warning!"
    show Green = "No Problem!"