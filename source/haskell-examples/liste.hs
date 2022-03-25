{- Varie funzioni che usano il tipo lista -}


{- Lunghezza della lista -}

lunghezza :: [a] -> Int

{- caso base: se lista vuota ritorna 0 -}
lunghezza [] = 0

{- caso generale (almeno un elemento) -}
lunghezza (_ : xs) = 1 + lunghezza xs


{- Testa della lista (primo elemento) -}

testa :: [a] -> a

testa (x : _) = x


{- Coda della lista (tutti gli altri elementi) -}

coda :: [a] -> [a]

coda (_ : xs) = xs


{- Membro della lista (appartiene o no?) -}
{- Il parametro che passiamo alla funzione deve essere istanza di Eq perché dobbiamo verificare un'uguaglianza -}

membro :: (Eq a) => a -> [a] -> Bool

{- Se la lista è vuota ritorniamo sempre falso per qualsiasi a -}
membro _ [] = False

{- Per il caso generale ci sono 2 sottocasi -}
membro x (y : ys) | x == y = True {- Se troviamo il primo elemento uguale -}
                  | otherwise = membro x ys {- Altrimenti ricorsione sui successivi-}


{- Funzione per determinare se una lista è PREFISSO di un'altra -}

prefisso :: (Eq a) => [a] -> [a] -> Bool

{- Se la prima lista è vuota allora è sicuramente ritorniamo true -}
prefisso [] _ = True

{- Se la seconda lista è vuota e la prima ha almeno un elemento allora ritorniamo false -}
prefisso (_ : _) [] = False 
prefisso (x : xs) (y : ys) | x == y = prefisso xs ys
                           | otherwise = False

{- Funzione per determinare se una lista è SUFFISSO di un'altra -}

suffisso :: (Eq a) => [a] -> [a] -> Bool

suffisso [] _ = True
suffisso (_ : _) [] = False 

{- @ si usa come abbreviazione per dare un nome a un'espressione-}
suffisso lx@(x : xs) ly@(y : ys) | lx == ly = True
                                 | otherwise = suffisso lx ys

{- SOTTOLISTA: sia prefisso che suffisso sono sottoliste, questo è un caso più generale che non considera la posizione -}

sottolista :: (Eq a) => [a] -> [a] -> Bool

sottolista [] _ = True
sottolista (_ : _) [] = False
sottolista lx@(x : xs) ly@(y : ys) | prefisso lx ly = True
                                   | otherwise = sottolista lx ly

{- Aggiungere un elemento in testa -}

inserisci_elem :: a -> [a] -> [a]

inserisci_elem x xs = x : xs 

{- Aggiungere un elemento in coda -}

accoda_elem :: a -> [a] -> [a]

accoda_elem x xs = xs ++ [x]

{- Concatenare due liste-}

concatena :: [a] -> [a] -> [a]

concatena xs ys = xs ++ ys

{- Invertire la lista -}

inverti :: [a] -> [a]

inverti [] = []
inverti (x : xs) = inverti xs ++ [x]

{- Rimuovere tutte le occorrenze un elemento -}

rimuovi :: (Eq a) => a -> [a] -> [a]

rimuovi _ [] = []
rimuovi x (y : ys) | x == y = rimuovi x ys {- butta via y -}
                   | otherwise = y : rimuovi x ys {- mantieni y in testa -}

{- Fusione ordinata di due liste ordinate -}

fondi :: (Ord a) => [a] -> [a] -> [a]

fondi xs [] = xs
fondi [] ys = ys
fondi lx@(x : xs) ly@(y : ys) | x < y = x : fondi xs ly 
                              | x > y = y : fondi lx ys
                              | x == y = x : y : fondi xs ys

{- Algoritmo di mergesort -}

mergesort :: (Ord a) => [a] -> [a]

mergesort [] = []
mergesort [x] = [x]
mergesort lx@(x1 : x2 : xs) = fondi (mergesort xs1) (mergesort xs2)
    where
        (xs1, xs2) = dimezza lx


{- Funzione ausiliaria che permette di dividere l'array a metà in ricorsivamente -}

{- Una metà conterrà gli elementi con indice pari, l'altra quelli con indice dispari -}

dimezza :: [a] -> ([a], [a]) 

dimezza [] = ([], [])
dimezza [x] = ([x], [])
dimezza (x1 : x2 : xs) = (x1 : xs1, x2 : xs2)
    where (xs1, xs2) = dimezza xs
