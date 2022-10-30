solveRPN :: String -> Double
solveRPN = head . foldl fordingFunction [] . words
    where fordingFunction (x:y:ys) "*" = (y * x) : ys
          fordingFunction (x:y:ys) "-" = (y - x) : ys
          fordingFunction (x:y:ys) "+" = (y + x) : ys
          fordingFunction acc numstr = read numstr : acc