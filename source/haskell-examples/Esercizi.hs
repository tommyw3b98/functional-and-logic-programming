{- Esercizio 1 -}

-- trova il massimo di una coppia di numeri
massimo :: (Num a, Ord a) => (a,a) -> a
massimo (x, y) 
    | x >= y = x
    | otherwise = y

-- data una lista di coppie di numeri ritorna una lista dei massimi di ogni coppia
listaMassimi :: (Num a, Ord a) => [(a,a)] -> [a]
listaMassimi = map massimo 

-- alternativamente possiamo usare la funzione built-in maximum
listaMassimi' :: (Num a, Ord a) => [(a,a)] -> [a]
listaMassimi' = map maximum

-- senza usare la funzione map
listaMassimiRic :: (Num a, Ord a) => [(a,a)] -> [a]
listaMassimiRic [] = []
listaMassimiRic (x:xs) = [massimo x] ++ listaMassimiRic xs

-- solo ricorsione
listaMassimiRic' :: (Num a, Ord a) => [(a,a)] -> [a]
listaMassimiRic' [] = []
listaMassimiRic' ((x,y):pairs) 
    | x >= y = [x] ++ listaMassimiRic' pairs
    | otherwise = [y] ++ listaMassimiRic' pairs