{- Introduciamo un nuovo tipo di dato per rappresentare i giorni della settimana -}

data Giorno = Lunedi | Martedi | Mercoledi | Giovedi | Venerdi | Sabato | Domenica
    deriving (Eq, Ord, Enum, Show)

{- Funzioni ieri e domani definite sui giorni della settimana -}

domani :: Giorno -> Giorno
domani Domenica = Lunedi
domani g        = succ g

ieri :: Giorno -> Giorno
ieri Lunedi = Domenica
ieri g      = pred g