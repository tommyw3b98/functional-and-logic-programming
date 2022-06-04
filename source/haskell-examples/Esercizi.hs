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