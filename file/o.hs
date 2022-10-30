import System.IO
main = do
    handle <- openFile "file/o.txt" ReadMode
    contents <- hGetContents handle
    putStrLn contents
    hClose handle