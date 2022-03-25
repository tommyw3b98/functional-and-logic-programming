{- Esempi di funzioni semplici scritte in Haskell -}

{- Funzione signum (ritorna il segno del numero passato), scritta in due diversi stili di programmazione -}

signum' :: (Num a, Ord a) => a -> a 

{- Stile imperativo -}
{-
signum' x = if (x > 0)
                then 1
            else
                if (x == 0)
                    then 0
                else -1
-}

{- Stile dichiarativo -}
signum' x | x > 0     = 1
         | x == 0    = 0
         | otherwise = -1

{- Funzione fattoriale -}

fattoriale :: Int -> Int

fattoriale 0 = 1
fattoriale n | n > 0 = n * fattoriale(n - 1)

{- Serie di Fibonacci -}

fibonacci :: Int -> Int

fibonacci 0 = 0
fibonacci 1 = 1
fibonacci n | n > 1 = fibonacci(n - 1) + fibonacci(n - 2)

{- Operatori logici -}

disgiunzione :: Bool -> Bool -> Bool

disgiunzione True _ = True
disgiunzione False x = x

congiunzione :: Bool -> Bool -> Bool

congiunzione False _ = False
congiunzione True x = x

{- Condizionale ternario -}

condi :: Bool -> a -> a -> a

condi True x _ = x
condi False _ y = y

{- Confronto di due caratteri -}
{- se sono uguali -> 0
   se il primo è minore del secondo -> -1
   altrimenti 1 -}

confronta :: Char -> Char -> Int

confronta c1 c2 | c1 == c2 = 0
                | c1 < c2 = -1
                | otherwise = 1