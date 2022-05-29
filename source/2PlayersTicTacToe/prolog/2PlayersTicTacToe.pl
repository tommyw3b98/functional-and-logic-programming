/* predicati per rappresentare lo stato delle caselle */

% una casella e' occupata da un giocatore se contiene una x oppure una o

x(B):- B = x.
o(B):- B = o.
player(B) :- x(B); o(B).

% una casella e' piena se e' occupata da un giocatore, in caso contrario e' vuota

full(B)  :- player(B).
empty(B) :- \+(full(B)).

/* predicati per la visualizzazione dello stato della tabella */

% visualizza lo stato delle caselle

showBox(B) :- o(B), write('  O  ').
showBox(B) :- x(B), write('  X  ').
showBox(B) :- empty(B), write('  -  ').

% visualizza lo stato delle righe
% X, Y, Z: caselle che costituiscono una riga

showRow(X, Y, Z) :-
    showBox(X),
    write('|'),
    showBox(Y),
    write('|'),
    showBox(Z), nl.

% visualizza la riga separatrice 

showDividingLine :-
    write('   _____|_____|_____'), nl,
    write('        |     |     '), nl.

% visualizza gli indici di colonna

showColIndex :-
    nl,write('     1     2     3'), nl, nl.

% visualizza lo stato della tabella corrente
% la lista in input rappresenta la tabella di gioco

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

/* controlli sull'input */

% controlla se la casella all'indice specificato e' vuota
% I: indice di lista (1 - 9) 
% B: lista che rappresenta la tabella di gioco corrente

isEmpty(I, B) :- 
    nth(I, B, E), % successo se l'I-esimo elemento di B e' uguale ad E
    empty(E).
    
% ottiene l'indice corrispondente ad una mossa inserita dall'utente

getBoxIndex('a1', 1).
getBoxIndex('a2', 2).
getBoxIndex('a3', 3).
getBoxIndex('b1', 4).
getBoxIndex('b2', 5).
getBoxIndex('b3', 6).
getBoxIndex('c1', 7).
getBoxIndex('c2', 8).
getBoxIndex('c3', 9).

/* logica di gioco */

% sostituisce l'N-esimo elemento di una lista con un nuovo elemento
% argomenti: indice, elemento da inserire, lista corrente, nuova lista
% caso base: sostitizione del primo elemento
% caso ricorsivo: se l'indice e' maggiore di 1, si richiama il predicato sulle code delle liste con I diminuito di 1, fino a ricondursi al caso base

replaceAtIndex(1, E, [_ | T], [E | T]).
replaceAtIndex(I, E, [H | T], [H | R]) :-
    I > 1, 
    INew is I - 1, 
    replaceAtIndex(INew, E, T, R).

% legge una mossa dall'utente e determina l'indice corrispondente
% B: lista che rappresenta la tabella di gioco corrente
% P: giocatore attivo nel turno corrente (x oppure o)
% I: indice corrispondente alla mossa letta

getMove(B, P, I) :- 
    player(P),
    ((x(P),             % turno del giocatore X
    nl, write('Player X enter a move (a1 - c3): '));
    (o(P),              % turno del giocatore O
    nl, write('Player O enter a move (a1 - c3): '))),
    read(M),    
    getBoxIndex(M, I1), % ottiene il corrispondente indice di lista
    isEmpty(I1, B),     % controlla che la casella corrispondente sia vuota
    I is I1.
getMove(B, P, I) :-
    player(P),
    nl, write('Invalid move, try again!'), nl,
    getMove(B, P, I).

% gestisce l'alternanza dei giocatori

nextPlayer(x, o).
nextPlayer(o, x).

% controlla se e' stata raggiunta una condizione di vittoria

checkWin(B, P) :- 
    rowWin(B, P);
    colWin(B, P);
    diagWin(B, P).

% determina le condizioni di vittoria
% B: tabella corrente
% P: simbolo di un giocatore

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

% controlla se e' stata raggiunta la condizione di parita' (tutte le caselle occupate da un giocatore)

checkTie(B) :- maplist(player, B).

% controlla lo stato del gioco ad ogni turno comunicando all'utente un'eventuale vittoria o pareggio
% se non e' riscontrata una condizione di fine partita, viene chiamato ricorsivamente gameLoop per iniziare un nuovo turno
% B: tabella corrente
% P: giocatore corrente

% controlla vittoria del giocatore x
checkGameState(B, P) :-
    checkWin(B, P),
    x(P),
    nl, write('Player X won!').
% controlla vittoria del giocatore o
checkGameState(B, P) :-
    checkWin(B, P),
    o(P),
    nl, write('Player O won!').
% controlla parita'
checkGameState(B, _) :-
    checkTie(B),
    nl, write('It\'s a tie!').
% se tutte le precedenti falliscono, continua con il prossimo turno
checkGameState(B, P) :-
    nextPlayer(P, NP),
    gameLoop(B, NP).

% gestisce lo stato della partita
% B: tabella corrente
% P: giocatore corrente

gameLoop(B, P) :-
    getMove(B, P, I),            % acquisice una mossa da giocatore corrente
    replaceAtIndex(I, P, B, NB), % genera una nuova lista con la mossa inserita dal giocatore
    showBoard(NB),               % mostra la nuova tabella
    checkGameState(NB, P).       % controlla il raggiungimento di una condizione di fine partita

% chiede all'utente se vuole giocare ancora una volta finita una partita
% valutato una volta raggiunta una condizione di fine partita (parita' oppure vittoria)

newGame :-
    nl, write('Play again? (y/n): '),
    read(C),
    ((C == 'y', % se il carattere letto e' y, chiama ricorsivamente la funzione main
     main);
    (C == 'n',  % altrimenti, se il carattere letto e' n, termina
     nl, write('Bye Bye!'))).
newGame :-
    nl, write('Invalid character, try again!'),
    newGame.

% predicato main, da eseguire per lanciare il programma

main :- 
    nl, write('Welcome to tic tac toe, good luck and have fun!'), nl,
    write('Warning: input must be lowercase and end with a period'), nl,
    showBoard([e,e,e,e,e,e,e,e,e]),   % stampa la tabella vuota
    gameLoop([e,e,e,e,e,e,e,e,e], x), % inizia il gioco con la tabella vuota e il giocatore x
    newGame.