import Data.List (intercalate)
import Data.Char (toLower)

{- Definizioni di nuovi tipi -}

{- Box rappresenta una casella della tabella: puo' essere vuota o occupata da un giocatore (X o O).
Una casella vuota e' rappresentata dal segnaposto (-) -}

data Box = Empty | Full Player
    deriving Eq

{- Player rappresenta il simbolo di un giocatore -}

data Player = X | O
    deriving (Show, Eq)

{- Per visualizzare le mosse sulla tabella creiamo un'istanza della typeclass Show per il tipo Box. 
Gli spazi ai lati sono aggiunti per centrare i simboli nelle rispettive caselle -}

instance Show Box where
    show Empty    = "  -  "
    show (Full X) = "  X  "
    show (Full O) = "  O  " 

{- Definizioni di funzioni-}

{- Restituisce la tabella vuota, utilizzata all'inizio di una partita -}

emptyBoard :: [Box]
emptyBoard = [Empty, Empty, Empty,
              Empty, Empty, Empty,
              Empty, Empty, Empty]

{- Prende una lista di 3 elementi che costituiscono una riga, la restituisce sotto forma di stringa: le caselle sono separate dal carattere "|" -}

renderRow :: [Box] -> String
renderRow row = intercalate "|" $ map show row

{- Restituisce la stringa separatrice tra righe -}

dividingLine :: String
dividingLine =  "   _____|_____|_____\n" ++ "        |     |     "

{- Restituisce la stringa degli indici di colonna -}

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
  where 
        firstRow  = take 3 board
        secondRow = drop 3 . take 6 $ board
        thirdRow  = drop 6 board

{- Funzione che mappa una mossa valida al corrispondente indice di casella, restituisce -1 per mosse invalide -}

getBoxIndex :: String -> Int
getBoxIndex "a1" =  0
getBoxIndex "a2" =  1
getBoxIndex "a3" =  2
getBoxIndex "b1" =  3
getBoxIndex "b2" =  4
getBoxIndex "b3" =  5
getBoxIndex "c1" =  6
getBoxIndex "c2" =  7
getBoxIndex "c3" =  8
getBoxIndex _    = -1

{- Funzione che controlla se una casella e' occupata -}

isEmpty :: Box -> Bool
isEmpty Empty    = True
isEmpty (Full _) = False

{- Funzione che data una tabella e un indice, controlla che la casella a quell'indice sia libera. 
Se la casella e' libera restituisce il suo indice (0 - 8) in caso contrario restituisce -1 -}

validatePosition :: [Box] -> String -> Int
validatePosition board m 
    | i /= -1 && isEmpty (board !! i) = i
    | otherwise = -1
    where 
        i = getBoxIndex m  
 
{- Funzione che prende una lista e un indice, e restituisce una coppia formata dalle liste degli elementi a sinistra e a destra di quello all'indice specificato -}

splitAtIndex :: [a] -> Int -> ([a], [a])
splitAtIndex lst i = (left, right)
    where 
        (left, xs) = splitAt i lst
        right = drop 1 xs 

{- Funzione per l'inserimento di un simbolo Player nella tabella di gioco, restituisce una nuova tabella -}

playMove :: [Box] -> Box -> Int -> [Box] 
playMove board player i = xs ++ [player] ++ ys
    where 
        (xs, ys) = splitAtIndex board i

{- Funzione che prende una mossa dall'utente e restituisce l'indice di tabella corrispondente.
Se l'utente inserisce una mossa invalida, la funzione viene richiamata ricorsivamente. -}

getMove :: [Box] -> Player -> IO Int
getMove board currentPlayer = do
    putStrLn $ "\nPlayer " ++ show currentPlayer ++ " enter a move (A1 - C3): "
    move <- getLine
    let index = validatePosition board $ map toLower move
    if index /= -1 then 
        return index
    else do
        putStrLn "\nInvalid move, try again!"
        getMove board currentPlayer

{- Funzione che gestisce l'alternanza dei giocatori -}

nextPlayer :: Player -> Player
nextPlayer X = O
nextPlayer O = X

{- Funzione che controlla se nella tabella attuale e' presente una combinazione vincente -}

checkWinner :: Player -> [Box] -> Bool
checkWinner move board =
    or [head board == Full move && board !! 1 == Full move && board !! 2 == Full move,
        board !! 3 == Full move && board !! 4 == Full move && board !! 5 == Full move,
        board !! 6 == Full move && board !! 7 == Full move && board !! 8 == Full move,
        head board == Full move && board !! 3 == Full move && board !! 6 == Full move,
        board !! 1 == Full move && board !! 4 == Full move && board !! 7 == Full move,
        board !! 2 == Full move && board !! 5 == Full move && board !! 8 == Full move,
        head board == Full move && board !! 4 == Full move && board !! 8 == Full move,
        board !! 6 == Full move && board !! 4 == Full move && board !! 2 == Full move]

{- Funzione che controlla se la partita e' un pareggio (tutte le caselle occupate da un giocatore e nessuna condizione di vittoria) -}

checkTie :: [Box] -> Bool 
checkTie board = all (\box -> not (isEmpty box)) board 

{- Funzione che comunica all'utente l'evento di un pareggio o di una vittoria, in caso contrario chiama ricorsivamente gameLoop con il giocatore successivo -}

checkGameState :: [Box] -> Player -> IO ()
checkGameState board currentPlayer
    | checkWinner X board = putStrLn "\nPlayer X won!"
    | checkWinner O board = putStrLn "\nPlayer O won!"
    | checkTie board      = putStrLn "\nIt's a tie!"
    | otherwise = gameLoop board (nextPlayer currentPlayer)

{- Funzione principale che gestice lo stato della partita-}

gameLoop :: [Box] -> Player -> IO ()
gameLoop board currentPlayer = do
    choice <- getMove board currentPlayer
    {- Creiamo una nuova tabella con il simbolo inserito dal giocatore -}
    let newBoard = playMove board (Full currentPlayer) choice 
    renderBoard newBoard
    {- Controlliamo un'eventuale vittoria o pareggio, se nessuno di questi eventi si verifica sara' richiamato ricorsivamente gameLoop con il giocatore successivo -}
    checkGameState newBoard currentPlayer

{- Funzione che chiede all'utente se vuole fare un'altra partita, se l'utente accetta e' richiamata main -}

newGame :: IO () 
newGame = do
    putStrLn "\nPlay again? (y/n):"
    c <- getChar
    let choice = toLower c
    if choice == 'y' then main
    else if choice == 'n' then do
        putStrLn "\nBye Bye!"
    else do
        putStrLn "\nInvalid character, try again!"
        newGame

{- Funzione main, da eseguire per lanciare il programma -}

main :: IO ()
main = do
    putStrLn "\nWelcome to tic tac toe, good luck and have fun!\n"
    renderBoard emptyBoard
    gameLoop emptyBoard X 
    {- Alla fine di una partita chiediamo all'utente se vuole giocare di nuovo -}
    newGame