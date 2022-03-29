{- Definizione della circonferenza dal basso verso l'alto -}
{- L'ordine delle definizioni è irrilevante -}
{- In questo caso le variabili create sono GLOBALI -}

raggio :: Float 
raggio = sqrt 15.196

diametro :: Float
diametro = 2 * raggio

circonferenza :: Float
circonferenza = pi * diametro

{- Definizioni alternative -}


{- Usando le definizioni seguenti possiamo creare variabili LOCALI alla 
definizione di circonferenza (che non hanno quindi valore al di fuori di essa)-}

{- Usando il costrutto LET-IN -}
{-

circonferenza :: Float
circonferenza = let
                    diametro = 2 * raggio
                    raggio = sqrt 15.196
                in
                    pi * diametro

-}

{- Usando il costrutto WHERE -}
{-

circonferenza :: Float
circonferenza = pi * diametro
    where
    	diametro = 2 * raggio
    	raggio = sqrt 15.196
-}