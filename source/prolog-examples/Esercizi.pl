/* 1) Lista dei massimi di ogni coppia */

% massimo di una coppia
coppia_massimo([X,Y], Max) :-
    number(X),
    number(Y),
    ((X >= Y, Max is X);
    (X < Y, Max is Y)).

% lista dei massimi di ogni coppia
lista_massimi([], []).
lista_massimi([H|T], [Nh|Nt]) :- 
    coppia_massimo(H, Nh),
    lista_massimi(T, Nt).

/* 2) Media di una lista di numeri */

% somma degli elementi
somma_lista([], 0).
somma_lista([H|T], S) :-
    number(H),
    somma_lista(T, Tmp),
    S is H + Tmp.

% media degli elementi
lista_media([], 0).
lista_media(L, M) :-
    somma_lista(L, S),
    length(L, Len),
    M is S / Len.

/* 3) Lista anni senza bisestili */

% stailire se un anno è bisestile
bisestile(Anno) :-
    T is Anno mod 400,
    T == 0.
bisestile(Anno) :-
    T1 is Anno mod 4,
    T2 is Anno mod 100,
    T1 == 0,
    T2 =\= 0.

% eliminare gli anni bisestili dalla lista
lista_non_bisestili([], []).
lista_non_bisestili([H|T], L) :-
    bisestile(H),
    lista_non_bisestili(T, L).
lista_non_bisestili([H|T], L) :-
    \+(bisestile(H)),
    lista_non_bisestili(T, L1),
    append(L1, [H], L).