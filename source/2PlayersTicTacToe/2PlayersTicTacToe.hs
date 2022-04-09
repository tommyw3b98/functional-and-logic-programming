import Data.List

{- Definizioni di nuovi tipi -}


{- Box rappresenta una casella della tabella: può essere vuota o occupata da un giocatore (X o 0).
Una casella vuota è rappresentata dal placeholder (-) -}

data Box = Empty | Full Player
    deriving Eq

{- Player rappresenta il simbolo di un giocatore -}

data Player = X | O
    deriving (Show, Eq)

{- Per visualizzare le mosse sulla tabella creiamo un'istanza della typeclass Show per il tipo Box 
gli spazi ai lati sono aggiunti per centrare i simboli nelle rispettive caselle -}

instance Show Box where
    show Empty =    "  -  "
    show (Full X) = "  X  "
    show (Full O) = "  O  " 


{- Definizioni di funzioni-}


{- Tabella vuota utilizzata all'inizio di una partita -}

emptyBoard :: [Box]
emptyBoard = [Empty, Empty, Empty,
              Empty, Empty, Empty,
              Empty, Empty, Empty]


{- Ritorna una riga della tabella sotto forma di stringa: le caselle sono separate dal carattere "|" -}

renderRow :: [Box] -> String
renderRow row = intercalate "|" $ map show row


{- Stringa separatrice tra righe -}

dividingLine :: String
dividingLine =  "   _____|_____|_____\n" ++ "        |     |     "


{-Indici di colonna -}

topCoordsLine :: String
topCoordsLine = "     1     2     3\n"


{- Funzione per visualizzare la tabella -}

renderBoard :: [Box] -> IO ()
renderBoard board = do
  putStrLn topCoordsLine  
  putStrLn $ "A  " ++ renderRow firstRow
  putStrLn dividingLine
  putStrLn $ "B  " ++ renderRow secondRow
  putStrLn dividingLine
  putStrLn $ "C  " ++ renderRow thirdRow
  where firstRow  = take 3 board
        secondRow = drop 3.take 6 $ board
        thirdRow  = drop 6 board


{- Lista di mosse accettabili -}

validMoves :: [String]
validMoves = ["A1", "A2", "A3", "B1", "B2", "B3", "C1", "C2", "C3"]


{- Funzione che mappa una mossa valida al corrispondente indice di casella, ritorna -1 per mosse invalide -}

getBoxIndex :: String -> Int
getBoxIndex "A1" =  0
getBoxIndex "A2" =  1
getBoxIndex "A3" =  2
getBoxIndex "B1" =  3
getBoxIndex "B2" =  4
getBoxIndex "B3" =  5
getBoxIndex "C1" =  6
getBoxIndex "C2" =  7
getBoxIndex "C3" =  8
getBoxIndex _ = -1


{- Funzione che valida una mossa controllando se appartiene alla lista validMoves -}

validateMove :: String -> Bool
validateMove move = move `elem` validMoves


{- Funzione che controlla se una casella è occupata -}

isEmpty :: Box -> Bool
isEmpty Empty = True
isEmpty (Full _) = False


{- Funzione che data una tabella e un indice, restituisce True se la casella a quell'indice è libera -}

validatePosition :: [Box] -> String -> Bool

validatePosition board m | i == -1 = False
                         | isEmpty $ board !! i = True
                         | otherwise = False
    where i = getBoxIndex m  
 

{- Funzione che prende una lista e un indice, e restituisce una coppia formata dalle liste degli elementi a sinistra e a destra di quello all'indice specificato -}

splitAtIndex :: [a] -> Int -> ([a], [a])

splitAtIndex lst i = (left, right)
    where 
        (left, xs) = splitAt i lst
        right = drop 1 xs 


{- Funzione per l'inserimento di un simbolo Player nella tabella di gioco -}

playMove :: [Box] -> Box -> Int -> [Box] 

playMove board player i = xs ++ [player] ++ ys
    where (xs, ys) = splitAtIndex board i     


{- Funzione che prende una mossa dall'utente e restituisce l'indice di tabella corrispondente -}

getMove :: [Box] -> Player -> IO Int

getMove board currentPlayer = do
    putStrLn $ "\nPlayer " ++ (show currentPlayer) ++ " enter a move (A1 - C3): "
    move <- getLine
    if validateMove move && validatePosition board move 
        then return $ getBoxIndex move
    else do
        putStrLn "\nInvalid move, try again!"
        getMove board currentPlayer


{- Funzione che gestisce l'alternanza dei turni -}

nextPlayer :: Player -> Player

nextPlayer X = O
nextPlayer O = X


{- Funzione che controlla se nella tabella attuale è presente una combinazione vincente -}

checkWinner :: Player -> [Box] -> Bool

checkWinner move board =
  or 
    [board !! 0 == (Full move) && board !! 1 == (Full move) && board !! 2 == (Full move),
     board !! 3 == (Full move) && board !! 4 == (Full move) && board !! 5 == (Full move),
     board !! 6 == (Full move) && board !! 7 == (Full move) && board !! 8 == (Full move),
     board !! 0 == (Full move) && board !! 3 == (Full move) && board !! 6 == (Full move),
     board !! 1 == (Full move) && board !! 4 == (Full move) && board !! 7 == (Full move),
     board !! 2 == (Full move) && board !! 5 == (Full move) && board !! 8 == (Full move),
     board !! 0 == (Full move) && board !! 4 == (Full move) && board !! 8 == (Full move),
     board !! 6 == (Full move) && board !! 4 == (Full move) && board !! 2 == (Full move)]


{- Funzione che controlla se la partita è un pareggio -}

checkTie :: [Box] -> Bool 

checkTie board = all (\box -> not (isEmpty box)) board 


{- Funzione che comunica all'utente l'evento di un pareggio o di una vittoria, in caso contrario chiama ricorsivamente gameLoop con il giocatore successivo -}

checkGameState :: [Box] -> Player -> IO ()

checkGameState board currentPlayer | checkTie board      = putStrLn "\nIt's a tie!"
                                   | checkWinner X board = putStrLn "\nPlayer X won!" 
                                   | checkWinner O board = putStrLn "\nPlayer O won!"
                                   | otherwise = gameLoop board (nextPlayer currentPlayer)


{- Funzione principale che gestice lo stato della partita-}

gameLoop :: [Box] -> Player -> IO ()
gameLoop board currentPlayer = do
    choice <- getMove board currentPlayer
    {- Creiamo una nuova tabella con il simbolo inserito dal giocatore -}
    let newBoard = playMove board (Full currentPlayer) choice 
    renderBoard newBoard
    {- Controlliamo un'eventuale vittoria o pareggio, se nessuno di questi eventi si verifica sarà richiamato ricorsivamente gameLoop con il giocatore successivo -}
    checkGameState newBoard currentPlayer


{- Funzione che chiede all'utente se vuole fare un'altra partita, se l'utente accetta è richiamata main -}

newGame :: IO () 
newGame = do
    putStrLn "\nPlay again? (y/n):"
    choice <- getLine
    if choice == ['y'] then main
    else if choice == ['n'] then return ()
    else do
        putStrLn "\nInvalid character, try again!"
        newGame


main :: IO ()
main = do
    putStrLn "\nWelcome to tic tac toe, good luck and have fun!\n"
    renderBoard emptyBoard
    gameLoop emptyBoard X 
    {- Alla fine di una partita chiediamo all'utente se vuole giocare di nuovo -}
    newGame
    putStrLn "\nBye Bye!" 
    