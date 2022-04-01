{- Ridefiniamo le funzioni standard map e fold-}

{- Si tratta di funzioni higher order che prendono in input altre funzioni -}

{- La funzione map applica la funzione passata a tutti gli elementi di una lista -}

map' :: (a -> b) -> [a] -> [b]

map' _ [] = []
map' f (x : xs) = f x : map f xs

{- La funzione fold combina una lista e una funzione per restituire un unico elemento -}

{- ci sono due varianti a seconda dell'ordine in cui gli elementi vengono processati : foldl -> da sinistra foldr -> da destra -}

{- dobbiamo specificare un valore iniziale da cui iniziare a costruire il nostro risultato, questo valore sarà il secondo parametro passato -}

foldl' :: (b -> a -> b) -> b -> [a] -> b 

foldl' _ i [] = i
foldl' f i (x : xs) = foldl' f (f i x) xs


foldr' :: (a -> b -> b) -> b -> [a] -> b 

foldr' _ i [] = i
foldr' f i (x : xs) = f x (foldr' f i xs)


{- Ora vediamo degli esempi di funzioni che sfruttano fold -}

{- Funzione che somma gli elementi di una lista -}

sommaLista :: (Num a) => [a] -> a

sommaLista = foldl (+) 0

{- Funzione che moltiplica gli elementi di una lista -}

moltiplicaLista :: (Num a) => [a] -> a

moltiplicaLista = foldl (*) 1

{- Funzione che calcola il massimo di una lista -}

massimo :: (Ord a) => [a] -> a

massimo [] = error "empty list"
massimo (x : xs) = foldl max x xs

{- Funzione che calcola il minimo di una lista -}

minimo :: (Ord a) => [a] -> a

minimo [] = error "empty list"
minimo (x : xs) = foldl min x xs

{- Disgiunzione di più espressioni booleane -}

disgiunzione :: [Bool] -> Bool

disgiunzione = foldl (||) False

{- Congiunzione di più espressioni booleane-}

congiunzione :: [Bool] -> Bool

congiunzione = foldl (&&) True