/* Predicati per rappresentare lo stato delle caselle */

% Una casella e' occupata da un giocatore se contiene una x oppure una o

x(B):- B = x.
o(B):- B = o.

player(B) :- x(B); o(B).

% Una casella e' piena se e' occupata da un giocatore, in caso contrario e' vuota

full(B)  :- player(B).

empty(B) :- \+(full(B)).

/* Predicati per la visualizzazione dello stato della tabella */

% Visualizzazione dello stato delle caselle

showBox(B) :- o(B), write('  O  ').
showBox(B) :- x(B), write('  X  ').
showBox(B) :- empty(B), write('  -  ').

% Visualizzazione dello stato delle righe

showRow(X, Y, Z) :-
    showBox(X),
    write('|'),
    showBox(Y),
    write('|'),
    showBox(Z), nl.

% Visualizzazione della riga separatrice 

showDividingLine :-
    write('   _____|_____|_____'), nl,
    write('        |     |     '), nl.

% Visualizzazione degli indici di colonna

showColIndex :-
    write('     1     2     3'), nl, nl.

% Visualizzazione dello stato della tabella corrente

showBoard([A, B, C, D, E, F, G, H, I]) :-
    showColIndex,
    write('A  '),
    showRow(A, B, C),
    showDividingLine,
    write('B  '),
    showRow(D, E, F),
    showDividingLine,
    write('C  '),
    showRow(G, H, I).

/* Validazioni dell'input */


% Validazione di una mossa inserita dall'utente (a1 - c3)

isValid(M) :-
    M == 'a1';
    M == 'a2';
    M == 'a3';
    M == 'b1';
    M == 'b2';
    M == 'b3';
    M == 'c1';
    M == 'c2';
    M == 'c3'.

% Controlla se la casella all'indice specificato e' vuota
% I: indice di lista (1 - 9) 
% B: lista che rappresenta la tabella di gioco corrente

isEmpty(I, B) :- 
    nth(I, B, E), % Successo se l'I-esimo elemento di B e' uguale ad E
    empty(E).
    
% Predicato per ottenere l'indice corrispondente ad una mossa

getBoxIndex(M, I) :-
    (M == 'a1', I is 1);
    (M == 'a2', I is 2);
    (M == 'a3', I is 3);
    (M == 'b1', I is 4);
    (M == 'b2', I is 5);
    (M == 'b3', I is 6);
    (M == 'c1', I is 7);
    (M == 'c2', I is 8);
    (M == 'c3', I is 9).

/* Logica di gioco */

% Sostituzione dell'N-esimo elemento di una lista con un nuovo elemento
% Argomenti: indice, elemento da inserire, lista corrente, nuova lista
% Caso base: sostitizione del primo elemento
% Caso ricorsivo: se l'indice e' maggiore di 1, si richiama il predicato sulle code delle liste con I diminuito di 1, fino a ricondursi al caso base

replaceAtIndex(1, E, [_ | T], [E | T]).
replaceAtIndex(I, E, [H | T], [H | R]) :-
    I > 1, 
    INew is I - 1, 
    replaceAtIndex(INew, E, T, R).

% Predicato per ottenere una mossa dall'utente e conoscere l'indice corrispondente
% B: lista che rappresenta la tabella di gioco corrente
% P: giocatore attivo nel turno corrente (x oppure o)

getMove(B, P, I) :- 
    player(P),
    ((x(P),             % Turno del giocatore X
    nl, write('Player X enter a move (a1 - c3): '));
    (o(P),              % Turno del giocatore O
    nl, write('Player O enter a move (a1 - c3): '))),
    read(M),    
    isValid(M),         % Controlla che la mossa inserita sia valida 
    getBoxIndex(M, I1), % Ottiene il corrispondente indice di lista
    isEmpty(I1, B),     % Controlla che la casella corrispondente sia vuota
    I is I1.
getMove(B, P, I) :-
    player(P),
    nl, write('Invalid move, try again!'),nl,
    getMove(B, P, I).

% Predicato per gestire l'alternanza dei turni

nextPlayer(x, o).
nextPlayer(o, x).

% Controlla se e' stata raggiunta una condizione di vittoria

checkWin(B, P) :- 
    rowWin(B, P);
    colWin(B, P);
    diagWin(B, P).

% Condizioni di vittoria

% 3 simboli uguali allineati in riga
rowWin(B, P) :-
    player(P), 
    (B = [P, P, P, _, _, _, _, _, _];
     B = [_, _, _, P, P, P, _, _, _];
     B = [_, _, _, _, _, _, P, P, P]).

% 3 simboli uguali allineati in colonna
colWin(B, P) :-
    player(P), 
    (B = [P, _, _, P, _, _, P, _, _];
     B = [_, P, _, _, P, _, _, P, _];
     B = [_, _, P, _, _, P, _, _, P]).

% 3 simoli uguali allineati in diagonale 
diagWin(B, P) :-
    player(P), 
    (B = [P, _, _, _, P, _, _, _, P];
     B = [_, P, _, _, P, _, _, P, _];
     B = [_, _, P, _, P, _, P, _, _]).

% Controlla se e' stata raggiunta la condizione di parita' (tutte le caselle occupate da un giocatore)

checkTie(B) :- maplist(player, B).

% Predicato che controlla lo stato del gioco ad ogni turno comunicando all'utente un'eventuale vittoria o pareggio
% Se non e' riscontrata una condizione di fine partita, viene chiamato ricorsivamente gameLoop per iniziare un nuovo turno

% Controlla vittoria del giocatore x
checkGameState(B, P) :-
    checkWin(B, P),
    x(P),
    nl, write('Player X won!').
% Controlla vittoria del giocatore o
checkGameState(B, P) :-
    checkWin(B, P),
    o(P),
    nl, write('Player O won!').
% Controlla parita'
checkGameState(B, _) :-
    checkTie(B),
    nl, write('It\'s a tie!').
% Se tutte le precedenti falliscono, continua con il prossimo turno
checkGameState(B, P) :-
    nextPlayer(P, NP),
    gameLoop(B, NP).

% Predicato principale per la gestione della partita

gameLoop(B, P) :-
    getMove(B, P, I),            % Acquisice una mossa da giocatore corrente
    replaceAtIndex(I, P, B, NB), % Genera una nuova lista con la mossa inserita dal giocatore
    showBoard(NB),               % Sostra la nuova tabella
    checkGameState(NB, P).       % Controlla il raggiungimento di una condizione di fine partita

% Predicato per richedere all'utente se vuole giocare ancora una volta finita una partita
% Viene invocato una volta raggiunta una condizione di fine partita (parita' oppure vittoria)

newGame :-
    nl, write('Play again? (y/n): '),
    read(C),
    ((C == 'y', % Se il carattere letto e' y, chiama ricorsivamente la funzione main
     main);
    (C == 'n',  % Altrimenti, se il carattere letto e' n, termina
     nl, write('Bye, Bye!'))).
newGame :-
    nl, write('Invalid character, try again!'),
    newGame.

% Predicato main, da eseguire per lanciare il programma

main :- 
    nl, write('Welcome to tic tac toe, good luck and have fun!'), nl,
    showBoard([e,e,e,e,e,e,e,e,e]),   % Stampa la tabella vuota
    gameLoop([e,e,e,e,e,e,e,e,e], x), % Inizia il gioco con la tabella vuota e il giocatore x
    newGame.