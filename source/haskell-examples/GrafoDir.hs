{- Modulo per lavorare con i grafi diretti -}

{- Da questo modulo esportiamo solo alcuni identificatori-}

{- Dobbiamo importare alcune funzioni definite nel modulo Lista ciò si può fare tramite import specificando il nome del modulo che si vuole importare -}

{- Possiamo accedere alle funzioni del modulo importato tramite dot notation (in questo caso L.nome_funzione_da_importare)-}

{- Per definire il tipo di dato grafo non usiamo l'enumerazione come per gli alberi ma rinominiamo un tipo già esistente -}

{- Il grafo è definito come una coppia, il cui primo elemento è una lista di vertici, e il secondo è una lista di archi (coppie orientate di vertici)-}

module GrafoDir (GrafoDir, grafo_dir, adiac, cerca_grafo_dir_amp, cerca_grafo_dir_prof) where 
    import qualified Lista as L 
    type GrafoDir a = ([a], [(a, a)]) 

{- funzione che controlla che nella lista degli archi non appaiano elementi diversi da quelli nella lista dei vertici -}

    grafo_dir :: (Eq a) => GrafoDir a -> Bool
    grafo_dir (vs,as) = controlla_v vs && controlla_a as vs
        where
            controlla_v []       = True
            controlla_v (v : vs) = not (L.membro v vs) && controlla_v vs

            controlla_a [] _ = True
            controlla_a ((v1, v2) : as) vs = (L.membro v1 vs) && (L.membro v2 vs) && not (L.membro (v1, v2) as) && controlla_a as vs

{- funzione che dato un vertice restituisce quelli adiacenti-}
    
    adiac :: (Eq a) => a -> [(a,a)] -> [a]
    
    adiac _ []              = []
    adiac v ((v1, v2) : as) | v == v1 = v2 : adiac v as
                            | v /= v1 = adiac v as

{- Ricerca di un valore nel grafo -}

{- ricerca in ampiezza -}

    {-Il terzo parametro è il vertice di partenza da cui iniziamo la ricerca -}
    cerca_grafo_dir_amp :: (Eq a) => a -> GrafoDir a -> a -> Bool

    cerca_grafo_dir_amp v (vs, as) i = L.membro i vs && cerca v [i] as [] 'a'

{- ricerca in profondità -}

    cerca_grafo_dir_prof :: (Eq a) => a -> GrafoDir a -> a -> Bool

    cerca_grafo_dir_prof v (vs, as) i = L.membro i vs && cerca v [i] as [] 'p'
 
{- funzione ausiliaria cerca (a seconda del char passato eseguirà una ricerca in ampiezza o in profondità)-}  
    
    cerca :: (Eq a) => a -> [a] -> [(a,a)] -> [a] -> Char -> Bool

    cerca _ [] _ _ _          = False
    cerca v (u : us) as vis t | v == u = True
                              | v /= u = if (L.membro u vis)
                                            then cerca v us as vis t
                                         else
                                            if (t == 'a')
                                                then cerca v (us ++ adiac u as) as (u : vis) t
                                            else cerca v (adiac u as ++ us) as (u : vis) t 
