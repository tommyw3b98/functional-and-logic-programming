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
listaMassimiRic (x:xs) = massimo x : listaMassimiRic xs

-- solo ricorsione
listaMassimiRic' :: (Num a, Ord a) => [(a,a)] -> [a]
listaMassimiRic' [] = []
listaMassimiRic' ((x,y):pairs) 
    | x >= y = x : listaMassimiRic' pairs
    | otherwise = y : listaMassimiRic' pairs

{- Esercizio 2 -}

-- Media di una lista di numeri
-- NB: la typeclass fractional serve per divisioni su numeri reali
-- NB: fromIntegral casta il risultato di lenght (classe Integral) alla classe Num
average :: (Fractional a) => [a] -> a
average xs = sum xs / fromIntegral (length xs)

{- Esercizio 3 -}

-- dividi per 400, se resto 0 è bisestile
-- altrimenti dividi per 4 e dividi per 100, se entrambe 0 è bisestile

nonBisestile :: Int -> Bool
nonBisestile x 
    | mod x 400 == 0 = False
    | (mod x 4 == 0) && not(mod x 100 == 0) = False 
    | otherwise = True

listaNonBisestili :: [Int] -> [Int]
listaNonBisestili = filter nonBisestile