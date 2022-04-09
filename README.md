Notes and code snippets from a functional and logic programming course.

All the comments in the source files are in italian.

# haskell-examples

Various functions and examples implemented in class.

To use a source file simply load it in ghci:

`ghci`

`:load <relative/path/to/file.hs>`

## circonferenza.hs

Example of defining local and global variables in haskell.

## esempi.hs

- Function definition in imperative and declarative ways.
- Various examples of basic recursive functions.

## esempi_2.hs

- Reimplementation of fold and map functions.
- Various functions on lists defined using fold.

## giorno.hs

Example of defining a new datatype, then defining a function using it.

## Lista.hs

- Module that redefines various recursive functions to work with lists.
- Mergesort and quicksort implementations.

## AlberoBin.hs

Module that defines the binary tree data structure and some functions to work with it.

A binary tree is defined as empty (Nil) or a node which has at most two subtrees.

### Example:

To represent the following tree (N represents an empty subtree):

```
         38
        /  \	
      15    3
      /\   / \
     N  N 45  N  
          /\
         N  N
```

we will type: 

`(Nodo 38 (Nodo 15 Nil Nil) (Nodo 3 (Nodo 45 Nil Nil) Nil))`

## GrafoDir.hs

Module that defines the directed graph data structure and some functions to work with it.

This module imports some functions from Lista.hs which has to be in the same directory to work.

A graph is defined as an ordered pair where the first element is the list of nodes, and the second element is the list of directed edges.

Every edge is an ordered pair of nodes (the first is the source, and the second is the destination).

### Example:

To represent the following graph:

```
          2	
          ^
          |
          |
    5--->11--->10<---3
          ^
          |
          |
          7	
```
we will type:

`([2, 5, 11, 7, 10, 3], [(5, 11), (11, 2), (7, 11), (11, 10), (3, 10)])`

## QuicksortInterattivo.hs

Example of a simple interactive program to show how to do input/output with haskell.

Requires LIsta.hs in the same directory to work.
