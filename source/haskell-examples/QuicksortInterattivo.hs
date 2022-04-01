import Lista (quicksort)

main :: IO ()

main = do 
    putStrLn "Inserisci una lista di interi da ordinare: "
    s <- getLine
    putStrLn "La lista ordinata è: "
    putStrLn $ show (quicksort (read s ::[Int]))