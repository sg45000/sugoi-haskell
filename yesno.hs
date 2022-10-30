import Tree as T (Tree( EmptyTree ))
import Traffic (TrafficLight(Green, Yellow, Red))

class YesNo a where
    yesno :: a -> Bool

instance YesNo Integer where
    yesno 0 = False
    yesno _ = True

instance YesNo [a] where
    yesno [] = False
    yesno _ = True

instance YesNo Bool where
    yesno = id

instance YesNo (Maybe a) where
    yesno Nothing = False
    yesno (Just _) = True

instance YesNo (Tree a) where
    yesno EmptyTree = False
    yesno _ = True

instance YesNo TrafficLight where
    yesno Red = False
    yesno _ = True

yesnoIf :: (YesNo y)=> y -> a -> a -> a
yesnoIf y yes no = if yesno y then yes else no