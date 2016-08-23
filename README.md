#Solvable Games

Features some examples of puzzles solved by the selection monad in Haskell.
All of the sequentially solved puzzles are Sudoku variations.
Solved in parallel:
  * Sudoku
  * Connect 3 / Connect 4
  * TicTacToe
  * NQueens

Computed sequentially:
  * Futoshiki
  * Nonograms
  * Buchstabensalat
  * Inshi no Heya
  * Nim (SubtractionGame)
  * River Crossing Problem

The selection monad itself is contained in the module 'GameLogic'. 
[Stack](https://docs.haskellstack.org/en/stable/README/) is recommended for building and executing this code. 
Run:
```bash
$ stack setup
$ stack build
$ stack exec solvable-games
```
