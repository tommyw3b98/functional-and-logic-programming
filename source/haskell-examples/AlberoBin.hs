{- Definizione di un albero binario in haskell tramite un modulo -}

{- O l'albero è vuoto o è una radice collegata ad al più 2 sottoalberi-}

{- Il parametro (a) indica il tipo del contenuto di ogni nodo -}

{- Show è una classe derivata di IO che consente la stampa a schermo -}

module AlberoBin where
    data AlberoBin a = Nil | Nodo a (AlberoBin a) (AlberoBin a)
        deriving(Show)

{- Ricerca di un elemento nell'albero -}

{- Ordine anticipato -}

    cerca_albero_bin_ant :: (Eq a) => a -> AlberoBin a -> Bool

    cerca_albero_bin_ant _ Nil            = False 
    cerca_albero_bin_ant n (Nodo m sx dx) = n == m || 
                                        cerca_albero_bin_ant n sx ||
                                        cerca_albero_bin_ant n dx   

{- Ordine posticipato -}

    cerca_albero_bin_post :: (Eq a) => a -> AlberoBin a -> Bool

    cerca_albero_bin_post _ Nil            = False 
    cerca_albero_bin_post n (Nodo m sx dx) = cerca_albero_bin_post n sx ||
                                         cerca_albero_bin_post n dx ||
                                         n == m

{- Ordine simmetrico -}

    cerca_albero_bin_simm :: (Eq a) => a -> AlberoBin a -> Bool

    cerca_albero_bin_simm _ Nil            = False 
    cerca_albero_bin_simm n (Nodo m sx dx) = cerca_albero_bin_simm n sx ||
                                         n == m ||
                                         cerca_albero_bin_simm n dx 

{- Funzione che stabilisce se un certo albero binario è di ricerca -}

    albero_bin_ric :: (Ord a) => AlberoBin a -> Bool

    albero_bin_ric Nil = True
    albero_bin_ric (Nodo n sx dx) = albero_bin_ric sx && minore_ug sx n &&
                                albero_bin_ric dx && maggiore_ug dx n 
        where
            minore_ug Nil _ = True
            minore_ug (Nodo m sx dx) n = m <= n && minore_ug sx n && minore_ug dx n

            maggiore_ug Nil _ = True
            maggiore_ug (Nodo m sx dx) n = m >= n && maggiore_ug sx n && maggiore_ug dx n


{- Ricerca di un valore in un albero binario di ricerca -}

    cerca_albero_bin_ric :: (Ord a) => a -> AlberoBin a -> Bool

    cerca_albero_bin_ric _ Nil            = False
    cerca_albero_bin_ric n (Nodo m sx dx) | n == m = True
                                          | n < m  = cerca_albero_bin_ric n sx
                                          | otherwise  = cerca_albero_bin_ric n dx