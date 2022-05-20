# 2 Players Tic Tac Toe

Example program implemented in both Haskell and Prolog. 

It's a very simple 2 players cli tic tac toe game.

## Haskell version - Usage

To play the haskell version either load it into ghci to test the functions separately or compile it with

`ghc 2PlayersTicTacToe.hs -o <executable file name>`

then run it with

`./<executable file name>`

## Prolog version - Usage

To play the prolog version load it into the prolog interpreter 

`prolog`

`?- consult('2PlayersTicTacToe.pl')`

Then call the main function by typing 

`?- main.`

All the input commands must be terminated by a dot.

The prolog version was developed using gprolog, so it may not be compatible with SWI.