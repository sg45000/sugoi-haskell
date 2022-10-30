import Data.Char (toUpper)
main = do
    putStrLn "Hello, what's your firstname?"
    firstname <- getLine
    putStrLn "what's your lastname?"
    lastname <- getLine
    let bigFirstName = map toUpper firstname
        bigLastName = map toUpper lastname
    putStrLn ("Hey " ++ bigFirstName ++ " " ++ bigLastName ++ ", you rock!")