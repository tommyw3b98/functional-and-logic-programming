/* Predicati per rappresentare lo stato delle caselle */

% Una casella è occupata da un giocatore se contiene una x oppure una o

x(B):- B = x.
o(B):- B = o.

player(B) :- x(B); o(B).

% Una casella è piena se è occupata da un giocatore, in caso contrario è vuota

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
    showBox(Z),nl.

% Visualizzazione della riga separatrice

showDividingLine :-
    write('   _____|_____|_____'), nl,
    write('        |     |     '), nl.

% Visualizzazione degli indici di colonna

showColIndex :-
    write('     1     2     3'), nl, nl.

% Visualizzazione della tabella

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

/* Validazioni dell'input utente */


% Validazione di una mossa inserita dall'utente 

isValid(M) :-
    M = A1;
    M = A2;
    M = A3;
    M = B1;
    M = B2;
    M = B3;
    M = C1;
    M = C2;
    M = C3.

% Controlla se la casella all'indice specificato è vuota

isEmpty(I, B) :- 
    % nth numera gli elementi da 1 in su
    nth(I, B, E),
    empty(E).
    
% Predicato per ottenere l'indice corrispondente ad una mossa

getBoxIndex(M, I) :-
    (M = A1, I is 1);
    (M = A2, I is 2);
    (M = A3, I is 3);
    (M = B1, I is 4);
    (M = B2, I is 5);
    (M = B3, I is 6);
    (M = C1, I is 7);
    (M = C2, I is 8);
    (M = C3, I is 9).

/* Logica di gioco */

% Sostituzione dell'N-esimo elemento di una lista, argomenti: indice, elemento da inserire, lista originale, nuova lista

replaceAtIndex(1, E, [_|T], [E|T]).
replaceAtIndex(I, E, [H|T], [H|R]) :-
    I > 1, 
    INew is I - 1, 
    replaceAtIndex(INew, E, T, R).

% Predicato per ottenere una mossa dall'utente e conoscere l'indice corrispondente

getMove(B, P, I) :- 
    player(P),
    o(P),
    nl,write('Player O enter a move (A1 - C3): '),
    read(M),
    isValid(M),
    getBoxIndex(M, I1),
    isEmpty(I1, B),
    I is I1.
getMove(B, P, I) :- 
    player(P),
    x(P),
    nl,write('Player X enter a move (A1 - C3): '),
    read(M),
    isValid(M),
    getBoxIndex(M, I1),
    isEmpty(I1, B),
    I is I1.
getMove(B, P, I) :-
    player(P),
    nl,write('Invalid move, try again!'),nl,
    getMove(B, P, I).

% Predicato per gestire l'alternanza dei turni

nextPlayer(x, o).
nextPlayer(o, x).

% Condizioni di vittoria

checkWin(B, P) :- 
    rowWin(B, P);
    colWin(B, P);
    diagWin(B, P).

rowWin(B, P) :-
    player(P), 
    (B = [P, P, P, _, _, _, _, _, _];
     B = [_, _, _, P, P, P, _, _, _];
     B = [_, _, _, _, _, _, P, P, P]).

colWin(B, P) :-
    player(P), 
    (B = [P, _, _, P, _, _, P, _, _];
     B = [_, P, _, _, P, _, _, P, _];
     B = [_, _, P, _, _, P, _, _, P]).

diagWin(B, P) :-
    player(P), 
    (B = [P, _, _, _, P, _, _, _, P];
     B = [_, P, _, _, P, _, _, P, _];
     B = [_, _, P, _, P, _, P, _, _]).

% Condizione di parità (tutte le caselle occupate da un player)

checkTie(B) :- maplist(player, B).

% Predicato che controlla lo stato del gioco comunicando all'utente un'eventuale vittoria o pareggio

checkGameState(B, P) :-
    checkWin(B, P),
    x(P),
    nl,write('Player X won!').
checkGameState(B, P) :-
    checkWin(B, P),
    o(P),
    nl,write('Player O won!').
checkGameState(B, _) :-
    checkTie(B),
    nl,write('It\'s a tie!').
checkGameState(B, P) :-
    nextPlayer(P, NP),
    gameLoop(B, NP).

% Predicato principale per la gestione della partita

gameLoop(B, P) :-
    getMove(B, P, I),
    replaceAtIndex(I, P, B, NB),
    showBoard(NB),
    checkGameState(NB, P).

% Predicato per richedere all'utente se vuole giocare ancora una volta finita una partita

newGame :-
    nl,write('Play again? (y/n): '),
    read(C),
    ((C = y,
     main);
    (C = n, 
     nl,write('Bye, Bye!'))).
newGame :-
    nl,write('Invalid character, try again!'),
    newGame.

% Predicato main, da eseguire per lanciare il programma

main :- 
    nl,write('Welcome to tic tac toe, good luck and have fun!'),nl,
    showBoard([e,e,e,e,e,e,e,e,e]),
    gameLoop([e,e,e,e,e,e,e,e,e], x),
    newGame.