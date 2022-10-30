import System.IO (IOMode(ReadMode), withFile, hGetContents)
main = withFile "file/o.txt" ReadMode $ \handle -> do
    contents <- hGetContents handle
    putStrLn contents