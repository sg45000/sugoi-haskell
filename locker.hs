

import qualified Data.Map as Map
data LockerState = Taken | Free deriving (Show, Eq)
type Code = String
type LockerMap = Map.Map Int (LockerState, Code)

lockerLookup :: Int -> LockerMap -> Either String Code
lockerLookup num lockerMap = case num `Map.lookup` lockerMap of
                                Nothing -> Left $ "Locker " ++ show num ++ " doesn't exist!"
                                Just (state, code) -> if state /= Taken
                                                        then Right code
                                                        else Left $ "Locker " ++ show num ++ " already used!"
lockers :: LockerMap
lockers = Map.fromList 
    [
        (1,(Taken, "1111")),
        (2, (Taken, "2222")),
        (3, (Free, "3333")),
        (4, (Taken, "4444")),
        (5, (Free, "5555")),
        (6, (Free, "6666")),
        (7, (Taken, "7777")),
        (8, (Taken, "8888")),
        (9, (Taken, "9999"))
    ]