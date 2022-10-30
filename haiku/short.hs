import Data.Char (toUpper)
main = do
    interact shortLinesOnly
shortLinesOnly :: String -> String
shortLinesOnly = unlines . filter (\line -> length line < 10) . lines